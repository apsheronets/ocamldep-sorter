
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

type rule = (string * string list)

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

let sort_deps (rules:rule list) targets_to_sort =
  let rec iteration n (rest_rules:rule list) left_side =
    match rest_rules with
    | [] -> left_side
    | _ ->
        let has_no_deps =
          List.filter (fun (target, deps) ->
            match deps with
            | [] -> true
            | _ -> false) rest_rules in
        let has_no_deps =
          List.map (fun (target, _) -> target) has_no_deps in
        (* remove targets without deps *)
        let rest_rules =
          List.filter (fun (target, deps) ->
            match deps with
            | [] -> false
            | _ -> true) rest_rules in
        (* remove targets without deps from another deps *)
        let rest_rules =
          List.map (fun (target, deps) ->
            target, List.filter (fun dep ->
              not (List.mem dep has_no_deps)) deps) rest_rules in
        let left_side = left_side @ has_no_deps in
        iteration (succ n) rest_rules left_side in
  iteration 1 rules []

let () =
  let depend = Std.input_all stdin in
  let rules = parse_depend depend in
  (* .cmi hack *)
  let rules =
    List.map
      (fun (target, deps) ->
        (target
        , List.fold_left
            (fun acc x ->
              if String.ends_with x ".cmi"
              then begin
                let new_name = String.slice x ~last:(-4) ^ ".cmo" in
                if new_name = target (* do not make circular dependencies *)
                then acc
                else new_name::acc
              end else x::acc) [] deps)) rules in
  let targets_to_sort =
    let rec loop n acc =
      try
        loop (succ n) (Sys.argv.(n) :: acc)
      with _ -> acc in
    loop 1 [] in
  let sorted_all_targets = sort_deps rules targets_to_sort in
  (* remove unnecessary *)
  let line =
    List.filter (fun dep -> List.mem dep targets_to_sort) sorted_all_targets in
  let line = String.concat " " line in
  print_endline line

