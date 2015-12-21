open Gg

type 'a cell = { content: (Box2.t * 'a) list; square: Box2.t }
type 'a t =
  | Leaf of 'a cell
  | Node of 'a cell * 'a t * 'a t * 'a t * 'a t

(* useless with uniformely distributed points...*)
let midpoint lst =
  List.fold_left
    (fun ((vec:V2.t),nbr) (b,_) -> V2.add vec (Box2.mid b),nbr+1)
    (V2.zero,0) lst
  |> fun (vec,nbr) -> V2.smul (1./.(float nbr)) vec

let divide mp b =
  let bl = Box2.bl_pt b in
  let br = Box2.br_pt b in
  let tl = Box2.tl_pt b in
  let tr = Box2.tr_pt b in
  let b1 = Box2.of_pts mp bl in
  let b2 = Box2.of_pts mp br in
  let b3 = Box2.of_pts mp tl in
  let b4 = Box2.of_pts mp tr in
  b1,b2,b3,b4

let rec split b1 b2 b3 b4 l1 l2 l3 l4 l5 = function
  | [] -> l1,l2,l3,l4,l5
  | ((b,a) as x) :: l ->
    if Box2.subset b b1
    then split b1 b2 b3 b4 (x::l1) l2 l3 l4 l5 l
    else if Box2.subset b b2
    then split b1 b2 b3 b4 l1 (x::l2) l3 l4 l5 l
    else if Box2.subset b b3
    then split b1 b2 b3 b4 l1 l2 (x::l3) l4 l5 l
    else if Box2.subset b b4
    then split b1 b2 b3 b4 l1 l2 l3 (x::l4) l5 l
    else split b1 b2 b3 b4 l1 l2 l3 l4 (x::l5) l

let split box lst =
  let mp = Box2.mid box in
  let b1,b2,b3,b4 = divide mp box in
  let l1,l2,l3,l4,l5 = split b1 b2 b3 b4 [] [] [] [] [] lst in
  (b1,l1),(b2,l2),(b3,l3),(b4,l4),l5

let surrounding lst =
  let minx,miny,maxx,maxy =
    List.fold_left
      (fun (minx,miny,maxx,maxy) (b,_) ->
         min minx (Box2.minx b),
         min miny (Box2.miny b),
         max maxx (Box2.maxx b),
         max maxy (Box2.maxy b))
      (max_float,max_float,min_float,min_float)
      lst
  in
  Box2.of_pts
    (V2.of_tuple (minx,miny))
    (V2.of_tuple (maxx,maxy))


let rec create d_max d (box,lst) =
  if d < d_max && lst <> []
  then (
    let c1,c2,c3,c4,l = split box lst in
    Node (
      { content = l; square = box },
      create d_max (d+1) c1,
      create d_max (d+1) c2,
      create d_max (d+1) c3,
      create d_max (d+1) c4))
  else
    Leaf { content = lst; square = box }

let create ~depth lst =
  let box = surrounding lst in
  create depth 0 (box,lst)


let rev_map_filter f box =
  let rec aux acc = function
    | [] -> acc
    | ((b,_) as c) :: l ->
      if Box2.isects box b
      then aux ((b,f c) :: acc) l
      else aux acc l
  in
  aux []

let rec map (f: (Box2.t * 'a) -> 'b) box = function
  | Leaf c ->
    let content = 
      if Box2.isects box c.square
      then rev_map_filter f box c.content
      else []
    in Leaf { c with content }
  | Node (c,t1,t2,t3,t4) ->
    if Box2.isects box c.square
    then
      let t1 = map f box t1 in
      let t2 = map f box t2 in
      let t3 = map f box t3 in
      let t4 = map f box t4 in
      let content = rev_map_filter f box c.content in
      Node ({ c with content },t1,t2,t3,t4)
    else Leaf { c with content = [] }

let map box f t = map f box t


let rec fold_filter f box acc = function
  | [] -> acc
  | ((b,_) as c) :: l ->
    if Box2.isects box b
    then fold_filter f box (f c acc) l
    else fold_filter f box acc l

let rec fold (f: (Box2.t * 'a) -> 'b -> 'b) box acc = function
  | Leaf c ->
    if Box2.isects box c.square
    then fold_filter f box acc c.content
    else acc
  | Node (c,t1,t2,t3,t4) ->
    if Box2.isects box c.square
    then
      let acc = fold f box acc t1 in
      let acc = fold f box acc t2 in
      let acc = fold f box acc t3 in
      let acc = fold f box acc t4 in
      fold_filter f box acc c.content
    else acc

let fold box f t acc = fold f box acc t


let rec iter_filter f box = function
  | [] -> ()
  | ((b,_) as c) :: l ->
    if Box2.isects box b then f c;
    iter_filter f box l

let rec iter (f: (Box2.t * 'a) -> unit) box = function
  | Leaf c ->
    if Box2.isects box c.square
    then iter_filter f box c.content
  | Node (c,t1,t2,t3,t4) ->
    if Box2.isects box c.square
    then (
      iter_filter f box c.content;
      iter f box t1;
      iter f box t2;
      iter f box t3;
      iter f box t4)

let iter box f t = iter f box t

