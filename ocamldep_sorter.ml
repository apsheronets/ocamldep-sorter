
open ExtLib

let find_from str pos sub =
  let sublen = String.length sub in
  if sublen = 0 then
    0
  else
    let found = ref pos in
    let len = String.length str in
    try
      for i = pos to len - sublen do
        let j = ref 0 in
        while String.unsafe_get str (i + !j) = String.unsafe_get sub !j do
          incr j;
          if !j = sublen then begin found := i; raise Exit; end;
        done;
      done;
      raise Invalid_string
    with
      Exit -> !found

let replace ?beg ~str ~sub ~by =
  let strlen = String.length str
  and sublen = String.length sub in
  let buf = Buffer.create (String.length str) in
  let rec loop n =
    try
      let pos = find_from str n sub in
      Buffer.add_substring buf str n (pos - n);
      Buffer.add_string buf by;
      let newn = (pos + sublen) in
      if newn > strlen
      then ()
      else loop newn
    with Invalid_string ->
      Buffer.add_substring buf str n (strlen - n) in
  match beg with
  | None ->
      loop 0;
      Buffer.contents buf
  | Some beg when strlen < beg ->
      str
  | Some beg ->
      Buffer.add_substring buf str 0 beg;
      loop beg;
      Buffer.contents buf

let print_rules =
  List.iter (fun (target, files) ->
    Printf.printf "%s:" target;
    List.iter (fun file ->
      Printf.printf " %s" file) files;
    print_char '\n')

let parse_depend s =
  let s = replace s "\\\n" " " in
  let rules = String.nsplit s "\n" in
  let rules = List.filter (function "" -> false | _ -> true) rules in
  let rules = List.map (fun s -> String.split s ":") rules in
  let rules =
    List.map
      (function (target, depends) ->
        (target, List.filter (function "" -> false | _ -> true) (String.nsplit depends " "))) rules in
  rules

let sort_deps rules targets_to_sort =
  let l = targets_to_sort in
  let l = ExtLib.List.unique l in
  let coolsort a =
    let cmp x y =
      let rec depended target _of =
        try
          let all_deps = List.assoc target rules in
          if List.mem _of all_deps
          then true
          else begin
            (* is it a dependency of a dependency? *)
            let rec loop = function
              | [] -> false
              | subdep::t ->
                  if depended subdep _of
                  then true
                  else loop t in
            loop all_deps
          end
        with Not_found -> false in
      match depended x y, depended y x with
      | true, false -> 1 (* move x right *)
      | false, true -> -1 (* everything is ok *)
      | false, false -> 0
      | true, true -> Printf.printf "%S %S\n%!" x y; assert false in
    let swap a x y =
      let tmp = a.(x) in
      a.(x) <- a.(y);
      a.(y) <- tmp in
    for i = 0 to Array.length a - 1 do
      for j = i to Array.length a - 1 do
        if cmp a.(i) a.(j) = 1
        then swap a i j;
      done;
    done in
  let a = Array.of_list l in
  coolsort a;
  let l = Array.to_list a in
  l

let () =
  let depend = Std.input_all stdin in
  let rules = parse_depend depend in
  (* .cmi hack *)
  let rules =
    List.map
      (fun (target, deps) ->
        (target
        , List.map
            (fun x ->
              if String.ends_with x ".cmi"
              then String.slice x ~last:(-3) ^ ".cmo"
              else x) deps)) rules in
  let targets_to_sort =
    let rec loop n acc =
      try
        loop (succ n) (Sys.argv.(n) :: acc)
      with _ -> acc in
    match loop 1 [] with
    | [] ->
        List.fold_left (fun acc (target, deps) ->
          target::(List.rev_append deps acc)
        ) [] rules
    | l -> l in
  let line = sort_deps rules targets_to_sort in
  let line = String.concat " " line in
  print_endline line

