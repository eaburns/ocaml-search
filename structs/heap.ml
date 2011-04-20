type 'a t = {
  mutable elms : 'a array;
  mutable fill : int;
  less : 'a -> 'a -> bool;
  index : 'a -> int -> unit;
}

let no_pos = ~-1

let min_size = 1024

let consider_growing heap =
  let elms = heap.elms in
  let fill = heap.fill in
  if fill >= Array.length elms then begin
    let size = if fill = 0 then min_size else fill * 2 in
    let init_elm i = if i < fill then elms.(i) else elms.(0) in
    heap.elms <- Array.init size init_elm
  end

let swap index elms i j =
  let ielm = elms.(i) and jelm = elms.(j) in
  elms.(j) <- ielm;
  elms.(i) <- jelm;
  index ielm j;
  index jelm i

let parent i =
  (i - 1) / 2

let left i =
  i * 2 + 1

let right i =
  i * 2 + 2

(* Push an element down away from the root. *)
let down heap i elm =
  let elms = heap.elms and less = heap.less and fill = heap.fill in
  let index = heap.index in
  let rec loop i =
    let l = left i and r = right i in
    let si, se =
      if l < fill && less elms.(l) elm then l, elms.(l) else i, elm in
    let si, se =
      if r < fill && less elms.(r) se then r, elms.(r) else si, se in
    if si <> i then begin
      elms.(i) <- se;
      index se i;
      loop si
    end else begin
      elms.(i) <- elm;
      index elm i;
      i
    end in
  loop i

let default_index _ _ = ()

let init ?(index=default_index) less elms =
  let heap = {
    elms = Array.copy elms;
    fill = Array.length elms;
    less = less;
    index = index;
  } in
  let elms = heap.elms in
  if heap.fill > 0 then begin
    for i = (heap.fill - 1) / 2 downto 0 do
      ignore (down heap i elms.(i))
    done;
  end;
  heap

exception Empty

let minimum heap =
  if heap.fill = 0 then
    raise Empty
  else
    heap.elms.(0)

let pop heap =
  if heap.fill = 0 then
    raise Empty
  else
    let elms = heap.elms and fill = heap.fill in
    let max = elms.(0) in
    swap heap.index heap.elms 0 (fill - 1);
    heap.fill <- fill - 1;
    ignore (down heap 0 elms.(0));
    heap.index max no_pos;
    max

(* Pull an element toward the root. *)
let up heap i elm =
  let elms = heap.elms and less = heap.less in
  let index = heap.index in
  let rec loop i =
    let p = parent i in
    if i > 0 && less elm elms.(p) then begin
      let pelm = elms.(p) in
      elms.(i) <- pelm;
      index pelm i;
      loop p
    end else begin
      elms.(i) <- elm;
      index elm i;
      i
    end in
  loop i

let update_key heap i =
  assert (i < heap.fill);
  let elm = heap.elms.(i) in
  let i = down heap i elm in
  ignore (up heap i elm)

let push heap elm =
  if Array.length heap.elms = 0 then
    heap.elms <- [| elm |];
  consider_growing heap;
  let fill = heap.fill in
  heap.fill <- fill + 1;
  ignore (up heap fill elm)

let fill heap =
  heap.fill

let is_empty heap =
  heap.fill = 0

(** Truncate the size of the underlying array to be exactly the size
    of the fill. *)
let exact_fit heap =
  heap.elms <- Array.sub heap.elms 0 heap.fill
