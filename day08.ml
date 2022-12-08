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

let lines = read_input "./inputs/day08.txt"

let r, c = List.length lines, String.length (List.nth lines 0)
let grid = Array.make_matrix r c (-1)

let rec read_grid row lines =
  match lines with
  | [] -> ()
  | line :: lines ->
    let () = String.iteri (fun col c ->
      let c = int_of_char c - int_of_char '0' in
      grid.(row).(col) <- c  
    ) line in
    read_grid (row + 1) lines

let () = read_grid 0 lines

let visibility = Array.make_matrix r c (-1, -1, -1, -1)

module TSet = Set.Make(struct
  type t = int * int
  let compare (a, b) (a', b') =
    let i = Int.compare a a' in
    if i = 0 then Int.compare b b'
    else i
end)

let find_visible_trees () =
  let visible = ref TSet.empty in
  for i = 0 to r - 1 do
    visible := TSet.add (0, i) !visible;
    visible := TSet.add (r - 1, i) !visible;
  done;
  for j = 0 to c - 1 do
    visible := TSet.add (j, 0) !visible;
    visible := TSet.add (j, c - 1) !visible;
  done;
  for i = 0 to r - 1 do
    let (_, b, c', d) = visibility.(i).(0) in
    visibility.(i).(0) <- grid.(i).(0), b, c', d;
    for j = 0 to c - 1 do
      if j - 1 >= 0 then
        let (_, b, c, d) = visibility.(i).(j) in
        let (a, _, _, _) = visibility.(i).(j - 1) in
        if grid.(i).(j) > a then
          let () = visible := TSet.add (i, j) !visible in
          visibility.(i).(j) <- grid.(i).(j), b, c, d
        else
          visibility.(i).(j) <- a, b, c, d
    done
  done;
  for i = 0 to r - 1 do
    let (a, b, _, d) = visibility.(i).(c - 1) in
    visibility.(i).(c - 1) <- a, b, grid.(i).(c - 1), d;
    for j = c - 1 downto 0 do
      if j + 1 < c then
        let (a, b, _, d) = visibility.(i).(j) in
        let (_, _, c, _) = visibility.(i).(j + 1) in
        if grid.(i).(j) > c then
          let () = visible := TSet.add (i, j) !visible in
          visibility.(i).(j) <- a, b, grid.(i).(j), d
        else
          visibility.(i).(j) <- a, b, c, d
    done
  done;
  for j = c - 1 downto 0 do
    let (a, b, c', _) = visibility.(r - 1).(j) in
    visibility.(r - 1).(j) <- a, b, c', grid.(r - 1).(j);
    for i = r - 1 downto 0 do
      if i + 1 < r then
        let (a, b, c, _) = visibility.(i).(j) in
        let (_, _, _, d) = visibility.(i + 1).(j) in
        if grid.(i).(j) > d then
          let () = visible := TSet.add (i, j) !visible in
          visibility.(i).(j) <- a, b, c, grid.(i).(j)
        else
          visibility.(i).(j) <- a, b, c, d
    done
  done;
  for j = 0 to c - 1 do
    let (a, _, c', d) = visibility.(0).(j) in
    visibility.(0).(j) <- a, grid.(0).(j), c', d;
    for i = 0 to r - 1 do
      if i - 1 >= 0 then
        let (a, _, c, d) = visibility.(i).(j) in
        let (_, b, _, _) = visibility.(i - 1).(j) in
        if grid.(i).(j) > b then
          let () = visible := TSet.add (i, j) !visible in
          visibility.(i).(j) <- a, grid.(i).(j), c, d
        else
          visibility.(i).(j) <- a, b, c, d
    done
  done;
  TSet.cardinal !visible

type mode = UP | DOWN | LEFT | RIGHT

let rec find_scenic_score ~row ~col mode curr =
  if row = 0 || row = r - 1 || col = 0 || col = c - 1 then 1 else
  if grid.(row).(col) >= curr then 1 else
    1 + 
    match mode with
    | UP -> find_scenic_score ~row:(row - 1) ~col mode curr
    | DOWN -> find_scenic_score ~row:(row + 1) ~col mode curr
    | LEFT -> find_scenic_score ~row ~col:(col - 1) mode curr
    | RIGHT -> find_scenic_score ~row ~col:(col + 1) mode curr

let find_scenic_score ~row ~col =
  let curr = grid.(row).(col) in
  if row = 0 || row = r - 1 || col = 0 || col = c - 1 then 0
  else
    find_scenic_score ~row:(row - 1) ~col UP curr
  * find_scenic_score ~row:(row + 1) ~col DOWN curr
  * find_scenic_score ~row ~col:(col - 1) LEFT curr
  * find_scenic_score ~row ~col:(col + 1) RIGHT curr


let find_max_scenic_score () =
  let m = ref (-1) in
  for i = 0 to r - 1 do
    for j = 0 to c - 1 do
      m := max !m (find_scenic_score ~row:i ~col:j)
    done
  done;
  !m

let () = Printf.printf "==== Day 08 ====\n"
let () = Printf.printf "Part1> %d\n" (find_visible_trees ())
let () = Printf.printf "Part2> %d\n" (find_max_scenic_score ())
