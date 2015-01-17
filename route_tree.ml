open Str

type inner = Txt of string | Wild of string
type 'a t = Node of inner * 'a option * 'a t list
type 'a query_res = Result of 'a option * (string * string) list

let default x = function
    None -> x
  | Some y -> y

let get_key = function
  | Node(x, _, _) -> x

let get_value = function
  | Node(_, x, _) -> x

let get_branches = function
  | Node(_, _, x) -> x

let set_value value node =
  let Node (key, _, branches) = node in
  Node(key, value, branches)


let find_node elem branches =
  (* first search for key matches, then wildcards *)
  (* since the order of the branch list is undefined
      search sequentially *)
  let cmp_t node =
    match get_key node with
    (* weirder than `elem = y` but fast(er) ? *)
    | Txt y -> 0 == String.compare elem y
    | Wild y -> 0 == String.compare elem y in
  try Some (List.find cmp_t branches) with Not_found -> None

let new_node elem =
  let sub s = String.sub s 1 ((String.length s) - 1) in
  let b = try elem.[0] = ':' with Invalid_argument _ -> false in
  let inner = if b then Wild (sub elem) else Txt elem in
  Node (inner, None, [])

let replace_branch branch tree =
  let a = get_key branch in
  let Node (v, q, branches) = tree in
  let filt node =
    let b = get_key node in
    match a, b with
      Wild _, Wild _ -> true
    | Txt c, Txt d -> 0 != String.compare c d
    | _, _ -> true in
  let sans_a = List.filter filt branches in
  Node (v, q, branch::sans_a)

let rec build tree parts value =
  let Node (v, _, branches) = tree in
  match parts with
    [] -> tree
  | h :: [] ->
    let n = default (new_node h) (find_node h branches) in
    let q = set_value value n in
    replace_branch q tree
  | h::t -> let n = default (new_node h) (find_node h branches) in
    replace_branch (build n t value) tree

let add_node str value tree =
  let parts = split (regexp "/") str in
  build tree parts (Some value)

let append elem parms node =
  match get_key node with
    Txt _ -> parms
  | Wild w -> (w, elem)::parms

let find_node2 elem branches =
  (* first search for key matches, then wildcards *)
  (* since the order of the branch list is undefined
      search sequentially *)
  let cmp_t node =
    match get_key node with
    (* weirder than `elem = y` but fast(er) ? *)
      Txt y -> 0 == String.compare elem y
    | Wild _ -> false in
  let cmp_w node =
    match get_key node with
      Txt _ -> false
    | Wild _ -> true in
  (* cmp_t should never return > 1 elements... *)
  let nodes = List.filter cmp_t branches in
  if List.length nodes > 0 then nodes else List.filter cmp_w branches

let rec query tree parms = function
    [] -> Some (Result ((get_value tree), List.rev parms))
  | h::t ->
    let branches = get_branches tree in
    let nodes = find_node2 h branches in
    if 0 = List.length nodes then None else
      (* now find 'best' match: *)
      let f n = query n (append h parms n) t in
      let res = List.map f nodes in
      (* filter out all empty matches *)
      let f = function
        | Some (Result ((Some _), _)) -> true
        | _ -> false in
      let res = List.filter f res in
      try List.hd res with Failure _ -> None

let search path tree =
  let sp = split (regexp "/") path in
  query tree [] sp
