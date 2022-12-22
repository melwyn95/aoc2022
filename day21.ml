let read_input file =
  let chan = open_in file in
  let rec acc lines =
    try
      let line = input_line chan in
      acc (line :: lines)
    with End_of_file -> List.rev lines
  in
  let lines = acc [] in
  let () = close_in chan in
  lines

let lines = read_input "./inputs/day21.txt"

type op = Add | Sub | Mul | Div
type exp = Const of int | Exp of string * op * string

let parse_op = function
| "+" -> Add
| "-" -> Sub
| "*" -> Mul
| "/" -> Div
| _ -> failwith "invalid op"

let tbl = Hashtbl.create 1024

let exps = lines
  |> List.map (fun line ->
      match String.split_on_char ' ' line with
      | [ monkey ; dep1 ; op ; dep2 ] ->
        let monkey = String.sub monkey 0 (String.length monkey - 1) in
        let op = parse_op op in
        Hashtbl.add tbl monkey (Exp (dep1, op, dep2))
      | [ monkey ; const ] ->
        let monkey = String.sub monkey 0 (String.length monkey - 1) in
        let const = int_of_string const in
        Hashtbl.add tbl monkey (Const const)
      | _ -> failwith "invalid exp"
  )

let root = Hashtbl.find tbl "root"

let rec eval exp =
  match exp with
  | Const n -> n
  | Exp (e1, op, e2) ->
    let e1, e2 = Hashtbl.find tbl e1, Hashtbl.find tbl e2 in 
    let e1 = eval e1 in
    let e2 = eval e2 in
    (match op with
    | Add -> e1 + e2
    | Sub -> e1 - e2
    | Mul -> e1 * e2
    | Div -> e1 / e2)

let number = eval root

let () = Printf.printf "==== Day 21 ====\n"
let () = Printf.printf "Part1> %d\n" number

let destruct exp = 
  match exp with
  | Exp (e1, _, e2) -> 
    let e1 = Hashtbl.find tbl e1 in
    let e2 = Hashtbl.find tbl e2 in
    e1, e2
  | Const _ -> failwith "invalid destruct"

let rec is_dep_humn exp =
  match exp with
  | Const n -> false
  | Exp (e1, op, e2) ->
    if e1 = "humn" || e2 = "humn" then true else
    let e1, e2 = Hashtbl.find tbl e1, Hashtbl.find tbl e2 in 
    is_dep_humn e1 || is_dep_humn e2


type hole = Hole | HConst of int | HExp of hole * op * hole
let rec exp_with_hole e =
  match e with
  | Const n -> HConst n
  | Exp ("humn", op, e2) ->
    let e2 = Hashtbl.find tbl e2 in 
    HExp (Hole, op, exp_with_hole e2)
  | Exp (e1, op, "humn") ->
    let e1 = Hashtbl.find tbl e1 in
    HExp (exp_with_hole e1, op, Hole)
  | Exp (e1, op, e2) ->
    let e1 = Hashtbl.find tbl e1 in
    let e2 = Hashtbl.find tbl e2 in
    HExp (exp_with_hole e1, op, exp_with_hole e2)

let e1, e2 = destruct root

let dep, value = if is_dep_humn e1 
  then exp_with_hole e1, string_of_int (eval e2)
  else exp_with_hole e2, string_of_int (eval e1)

let rec partial_eval hole =
  match hole with
  | Hole -> Hole
  | HConst n -> HConst n
  | HExp (HConst h1, Add, HConst h2) -> HConst (h1 + h2)
  | HExp (HConst h1, Sub, HConst h2) -> HConst (h1 - h2)
  | HExp (HConst h1, Mul, HConst h2) -> HConst (h1 * h2)
  | HExp (HConst h1, Div, HConst h2) -> HConst (h1 / h2)
  | HExp (h1, op, h2) ->
    let h1 = partial_eval h1 in
    let h2 = partial_eval h2 in
    HExp (h1, op, h2)

let rec simplify hole =
  let h = partial_eval hole in
  if h = hole then h else simplify h

let dep = simplify dep

let rec inverse dep value =
  match dep with
  | HExp (h1, Add, HConst n) ->
    let value = (int_of_string value) - n in
    let value = string_of_int value in  
    inverse h1 value
  | HExp (h1, Sub, HConst n) ->
    let value = (int_of_string value) + n in
    let value = string_of_int value in  
    inverse h1 value
  | HExp (h1, Mul, HConst n) ->
    let value = (int_of_string value) / n in
    let value = string_of_int value in  
    inverse h1 value
  | HExp (h1, Div, HConst n) ->
    let value = (int_of_string value) * n in
    let value = string_of_int value in  
    inverse h1 value
  | HExp (HConst n, Sub, h2) ->
    let value = n - (int_of_string value) in
    let value = string_of_int value in
    inverse h2 value
  | HExp (HConst n, Div, h2) ->
    let value = n / (int_of_string value) in
    let value = string_of_int value in
    inverse h2 value
  | HExp (HConst n, Add, h2) ->
    let value = (int_of_string value) - n in
    let value = string_of_int value in
    inverse h2 value
  | HExp (HConst n, Mul, h2) ->
    let value = (int_of_string value) / n in
    let value = string_of_int value in
    inverse h2 value
  | _ -> dep, value

let dep, value = inverse dep value 

let () = Printf.printf "Part2> %s\n" value