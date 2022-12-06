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

let lines = read_input "./inputs/day01.txt"

let rec list_sum = function
| [] -> 0
| x :: xs -> x + list_sum xs

let list_max ?(min = 0)xs =
  let rec aux mx xs =
    match xs with
    | [] -> mx
    | x :: xs -> aux (max mx x) xs
  in
  aux min xs

let elfs = List.fold_right (fun line (elfs, cals) ->
  if line = "" 
  then (list_sum cals) :: elfs, []
  else
    let cal = int_of_string line in
    elfs, cal :: cals
) lines ([], []) 
|> (fun (elfs, cals) -> (list_sum cals) :: elfs)

let max_cal_elf = list_max elfs

let sort2 x y =
  if x > y
  then x, y
  else y, x
 
let sort3 x y z =
  if x > y && x > z
  then
    let a, b = sort2 y z in
    x, a, b
  else if y > z && y > x
  then
    let a, b = sort2 x z in
    y, a, b
  else
    let a, b = sort2 x y in
    z, a, b

let max3 (a, b, c) x =
  if x > a || x > b || x > c
  then sort3 a b x
  else a, b, c

let list_max3 ?(min = 0, 0, 0)xs =
  let rec aux mxs xs =
    match xs with
    | [] -> mxs
    | x :: xs -> aux (max3 mxs x) xs
  in
  aux min xs

let sum3 (x, y, z) = x + y + z

let max3_cal_elf = sum3 (list_max3 elfs)

let () = Printf.printf "==== Day 01 ====\n"
let () = Printf.printf "Part1> %d\n" max_cal_elf
let () = Printf.printf "Part2> %d\n" max3_cal_elf