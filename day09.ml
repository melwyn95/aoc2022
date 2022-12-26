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

let lines = read_input "./inputs/day09.txt"

type dir = U | D | R | L
type move = int * dir

let invariant ~head ~tail =
  let hx, hy = head in
  let tx, ty = tail in 
  ((abs (hx - tx) = 1 || abs (hx - tx) = 0) && 
   (abs (hy - ty) = 1 || abs (hy - ty) = 0))

let parse_moves =
  List.map (fun line -> 
    let move = String.split_on_char ' ' line in
    match move with
    | [ "R" ; n ] -> int_of_string n, R 
    | [ "L" ; n ] -> int_of_string n, L 
    | [ "U" ; n ] -> int_of_string n, U 
    | [ "D" ; n ] -> int_of_string n, D 
    | _ -> failwith "invalid move"
  )

let find_tail rope =
  match rope with
  | [ tail ] -> tail
  | [ _ ; _ ; _ ; _ ; _ ; _ ; _ ; _ ; tail ] -> tail
  | _ -> failwith "invalid rope"

let moves = parse_moves lines

module TSet = Set.Make (struct
  type t = int * int
  let compare (x1, y1) (x2, y2) = 
    match Int.compare x1 x2 with
    | 0 -> Int.compare y1 y2
    | n -> n
end)

let fix ~head ~tail =
  if invariant ~head ~tail then tail
  else
    let hx, hy = head in
    let tx, ty = tail in
    if hx = tx then
      if (hy - ty) = 2 then tx, ty + 1
      else if (hy - ty) = -2 then tx, ty - 1
      else failwith (Printf.sprintf "dunno (%d, %d), (%d, %d)" hx hy tx ty)
    else if hy = ty then
      if (hx - tx) = 2 then tx + 1, ty
      else if (hx - tx) = -2 then tx - 1, ty
      else failwith (Printf.sprintf "dunno (%d, %d), (%d, %d)" hx hy tx ty)
    else
      if      (hx - tx) =  2 && (hy - ty) =  1 then tx + 1, ty + 1
      else if (hx - tx) =  2 && (hy - ty) = -1 then tx + 1, ty - 1
      else if (hx - tx) = -2 && (hy - ty) =  1 then tx - 1, ty + 1
      else if (hx - tx) = -2 && (hy - ty) = -1 then tx - 1, ty - 1
       
      else if (hx - tx) =  1 && (hy - ty) =  2 then tx + 1, ty + 1
      else if (hx - tx) = -1 && (hy - ty) =  2 then tx - 1, ty + 1
      else if (hx - tx) =  1 && (hy - ty) = -2 then tx + 1, ty - 1
      else if (hx - tx) = -1 && (hy - ty) = -2 then tx - 1, ty - 1

      else if (hx - tx) =  2 && (hy - ty) =  2 then tx + 1, ty + 1
      else if (hx - tx) = -2 && (hy - ty) =  2 then tx - 1, ty + 1
      else if (hx - tx) =  2 && (hy - ty) = -2 then tx + 1, ty - 1
      else if (hx - tx) = -2 && (hy - ty) = -2 then tx - 1, ty - 1
    
      else failwith (Printf.sprintf "dunno (%d, %d), (%d, %d)" hx hy tx ty)

let rec fix_rope ~head rope =
  match rope with
  | [] -> []
  | tail :: rope ->
    let tail = fix ~head ~tail in
    tail :: fix_rope ~head:tail rope

let rec move ~head rope dir =
  let hx, hy = head in
  let head =
    match dir with
    | U -> hx, hy + 1
    | D -> hx, hy - 1
    | R -> hx + 1, hy
    | L -> hx - 1, hy
  in
  head, fix_rope ~head rope

let rec move_rope rope (times, dir) =
  if times = 0 then [], rope else
  match rope with
  | head :: rope ->
    let head, rope = move ~head rope dir in
    let t = find_tail rope in
    let ts, rope = move_rope (head :: rope) (times - 1, dir) in
    t :: ts, rope
  | _ -> failwith "invalid rope"

let rope = [(0, 0) ; (0, 0)]

let visited = TSet.singleton (0, 0)
let _, visited = List.fold_left (fun (rope, visited) m ->
  let ts, rope = move_rope rope m in
  let visited = List.fold_right TSet.add ts visited in
  rope, visited
) (rope, visited) moves


let () = Printf.printf "==== Day 09 ====\n"
let () = Printf.printf "Part1> %d\n" (TSet.cardinal visited)


let rope = [ (0, 0) ; (0, 0) ; (0, 0) ; (0, 0) ; (0, 0) 
           ; (0, 0) ; (0, 0) ; (0, 0) ; (0, 0) ; (0, 0) ]
let visited = TSet.singleton (0, 0)
let _, visited = List.fold_left (fun (rope, visited) m ->
  let ts, rope = move_rope rope m in
  let visited = List.fold_right TSet.add ts visited in
  rope, visited
) (rope, visited) moves

let () = Printf.printf "Part2> %d\n" (TSet.cardinal visited)