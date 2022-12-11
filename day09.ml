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

type rope = {
  head : int * int;
  tail : int * int
}

let invariant rope =
  let { head ; tail } = rope in
  let hx, hy = head in
  let tx, ty = tail in
  assert 
    ((abs (hx - tx) = 1 || abs (hx - tx) = 0) && 
     (abs (hy - ty) = 1 || abs (hy - ty) = 0))

let rec move rope (times, dir) =
  invariant rope;
  if times = 0 then [], rope else
  let { head ; tail } = rope in
  let hx, hy = head in
  let tx, ty = tail in
  let rope =
    if hx = tx || hy = ty then
      let vert = hx = tx in
      let hori = hy = ty in
      let vert, hori = if hx = tx && ty = hy then true, true else vert, hori in
      match dir with
      | U when hori -> { head = (hx, hy + 1) ; tail = (tx, ty)     }
      | U -> if abs (hy + 1 - ty) > 1 then       
                       { head = (hx, hy + 1) ; tail = (tx, ty + 1) }
             else      { head = (hx, hy + 1) ; tail = (tx, ty)     }
      | D when hori -> { head = (hx, hy - 1) ; tail = (tx, ty)     }
      | D -> if abs (hy - 1 - ty) > 1 then          
                       { head = (hx, hy - 1) ; tail = (tx, ty - 1) }
             else
                       { head = (hx, hy - 1) ; tail = (tx, ty)     }
      | R when vert -> { head = (hx + 1, hy) ; tail = (tx, ty)     }
      | R -> if abs (hx + 1 - tx) > 1 then          
                       { head = (hx + 1, hy) ; tail = (tx + 1, ty) }
             else
                       { head = (hx + 1, hy) ; tail = (tx, ty)     }
      | L when vert -> { head = (hx - 1, hy) ; tail = (tx, ty)     }
      | L -> if abs (hx - 1 - tx) > 1 then
                       { head = (hx - 1, hy) ; tail = (tx - 1, ty) }
             else
                       { head = (hx - 1, hy) ; tail = (tx, ty)     }
    else
      if hx < tx && hy > ty then
        match dir with
        | U -> { head = (hx, hy + 1) ; tail = (hx, hy) }
        | D -> { head = (hx, hy - 1) ; tail = (tx, ty) }
        | R -> { head = (hx + 1, hy) ; tail = (tx, ty) }
        | L -> { head = (hx - 1, hy) ; tail = (hx, hy) }
      else if hx < tx && hy < ty then
        match dir with
        | U -> { head = (hx, hy + 1) ; tail = (tx, ty) }
        | D -> { head = (hx, hy - 1) ; tail = (hx, hy) }
        | R -> { head = (hx + 1, hy) ; tail = (tx, ty) }
        | L -> { head = (hx - 1, hy) ; tail = (hx, hy) }
      else if hx > tx && hy > ty then
        match dir with
        | U -> { head = (hx, hy + 1) ; tail = (hx, hy) }
        | D -> { head = (hx, hy - 1) ; tail = (tx, ty) }
        | R -> { head = (hx + 1, hy) ; tail = (hx, hy) }
        | L -> { head = (hx - 1, hy) ; tail = (tx, ty) }
      else if hx > tx && hy < ty then
        match dir with
        | U -> { head = (hx, hy + 1) ; tail = (tx, ty) }
        | D -> { head = (hx, hy - 1) ; tail = (hx, hy) }
        | R -> { head = (hx + 1, hy) ; tail = (hx, hy) }
        | L -> { head = (hx - 1, hy) ; tail = (tx, ty) }
      else failwith (Printf.sprintf "dunno :( (%d, %d)-(%d, %d) %B" hx hy tx ty (hx < tx && hy < ty))
  in
  let tail = rope.tail in
  let ts, rope = move rope (times - 1, dir) in
  tail :: ts, rope

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

let moves = parse_moves lines

module TSet = Set.Make (struct
  type t = int * int
  let compare (x1, y1) (x2, y2) = 
    match Int.compare x1 x2 with
    | 0 -> Int.compare y1 y2
    | n -> n
end)

let rope = { head = (0, 0) ; tail = (0, 0) }
let visited = TSet.singleton (0, 0)
let _, visited = List.fold_left (fun (rope, visited) m ->
  let ts, rope = move rope m in
  let visited = List.fold_right TSet.add ts visited in
  rope, visited
) (rope, visited) moves


let () = Printf.printf "==== Day 09 ====\n"
let () = Printf.printf "Part1> %d\n" (TSet.cardinal visited)