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

let lines = read_input "./inputs/day14.txt"

let rocks = lines
|> List.map (fun line ->
  let cs = String.split_on_char ' ' line in
  let rec interpolate p1 p2 =
    if p1 = p2 then [ p2 ]
    else
      let (x1, y1), (x2, y2) = p1, p2 in
      (* Printf.printf "%d %d --> %d %d\n" x1 y1 x2 y2; *)
      if x1 = x2 then
        if y1 < y2 then
          (x1, y1) :: interpolate (x1, y1 + 1) (x2, y2)
        else
          (x1, y1) :: interpolate (x1, y1 - 1) (x2, y2)
      else
        if x1 < x2 then
          (x1, y1) :: interpolate (x1 + 1, y1) (x2, y2)
        else
          (x1, y1) :: interpolate (x1 - 1, y1) (x2, y2)
  in
  let extract_xy c =
    match String.split_on_char ',' c with
    | [ x ; y ] -> int_of_string y, int_of_string x
    | _ -> failwith "invalid extract co-ord"
  in
  let rec aux cs =
    match cs with
    | [ c1 ; _ ; c2 ] -> 
      let c1 = extract_xy c1 in
      let c2 = extract_xy c2 in
      interpolate c1 c2
    | c1 :: _ :: c2 :: cs ->
      let c1' = extract_xy c1 in
      let c2' = extract_xy c2 in
      interpolate c1' c2' @ aux (c2 :: cs)
    | _ ->  failwith "invalid co-ords"
  in
  aux cs)
|> List.concat

let (min_x, max_x), (min_y, max_y) = rocks
|> List.fold_left (fun ((min_x, max_x), (min_y, max_y)) (rock_x, rock_y) ->
  let min_x = if rock_x < min_x then rock_x else min_x in
  let max_x = if rock_x > max_x then rock_x else max_x in
  let min_y = if rock_y < min_y then rock_y else min_y in
  let max_y = if rock_y > max_y then rock_y else max_y in
  (min_x, max_x), (min_y, max_y)
) ((max_int, min_int), (max_int, min_int))

type cell = Rock | Sand | Air

let n_rows = max_x + 3
let n_cols = 10000

let grid = Array.make_matrix n_rows n_cols Air
let start = (0, 500)

let () = rocks
|> List.iter (fun (x, y) -> grid.(x).(y) <- Rock)

let print_grid () =
  for r = 0 to max_x + 2 do
    for c = min_y - 10 to max_y + 10 do
      match grid.(r).(c) with
      | Air -> Printf.printf ". "
      | Rock -> Printf.printf "# "
      | Sand -> Printf.printf "o "
    done;
    Printf.printf "\n"
  done

(* let () = print_grid () *)

let is_valid (r, c) = r >= 0 && r < n_rows && c >= 0 && c < n_cols
let rec move_particle (r, c) =
  (* Printf.printf "%d %d\n" r c; *)
  match grid.(r + 1).(c) with
  | Air -> move_particle (r + 1, c)
  | Rock | Sand ->
    (match grid.(r + 1).(c - 1) with
    | Air -> move_particle (r + 1, c - 1)
    | Rock | Sand ->
      (match grid.(r + 1).(c + 1) with
      | Air -> move_particle (r + 1, c + 1)
      | Rock | Sand -> grid.(r).(c) <- Sand))

let rec fill_sand () =
  try
    let r, c = start in
    match grid.(r).(c) with
    | Sand -> 0
    | _ ->
      let () = move_particle start in
      (* let () = print_grid () in *)
      1 + fill_sand ()
  with _ -> 0

let sand = fill_sand ()

let () = Printf.printf "==== Day 14 ====\n"
let () = Printf.printf "Part1> %d\n" sand

let () = for i = 0 to (n_cols - 1) do
  grid.(n_rows - 1).(i) <- Rock
done

let () = for r = 0 to n_rows - 1 do
  for c = 0 to n_cols - 1 do
    if grid.(r).(c) = Sand then 
      grid.(r).(c) <- Air
  done
done

let sand = fill_sand ()

let () = Printf.printf "Part2> %d\n" sand