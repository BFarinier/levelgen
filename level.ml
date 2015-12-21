open Gg
open Graph
open Qtree

let compare_float (x: float) (y: float) = compare x y
let equal_float (x: float) (y: float) = x = y

module D = Delaunay.Make
    (struct
      type point = V2.t
      let ccw v1 v2 v3 =
        Delaunay. FloatPoints.ccw
          (V2.to_tuple v1)
          (V2.to_tuple v2)
          (V2.to_tuple v3)
      let in_circle v1 v2 v3 v4 =
        Delaunay.FloatPoints.in_circle
          (V2.to_tuple v1)
          (V2.to_tuple v2)
          (V2.to_tuple v3)
          (V2.to_tuple v4)
    end)

module G = Persistent.Graph.ConcreteLabeled
    (struct
      type t = V2.t
      let compare = V2.compare
      let hash = Hashtbl.hash
      let equal = V2.equal
    end)
    (struct
      type t = float
      let compare = compare_float
      let default = 0.
    end)

module K = Kruskal.Make(G)
    (struct
      type t = G.E.label
      let compare = compare_float
      let hash = Hashtbl.hash
      let equal = equal_float
    end)


let biased_float f =
  let f1 = Random.float f in
  let f2 = Random.float f in
  (f1 +. f2) /. 2.

let random_float f = Random.float f

let biased_points ~x ~y ~nbr =
  let rec aux acc n =
    if n > 0 then
      let x = biased_float x in
      let y = biased_float y in
      aux (V2.v x y :: acc) (n-1)
    else acc
  in
  aux [] nbr

let random_points ~x ~y ~nbr =
  let rec aux acc n =
    if n > 0 then
      let x = random_float x in
      let y = random_float y in
      aux (V2.v x y :: acc) (n-1)
    else acc
  in
  aux [] nbr


let delaunay pts =
  Array.of_list pts
  |> D.triangulate

let extract e = G.E.src e, G.E.dst e

let add_edge v1 v2 g =
  let d = V2.(norm2 (v2 - v1)) in
  let e = G.E.create v1 d v2 in
  G.add_edge_e g e

let spanning_tree pts =
  let trs = delaunay pts in
  D.fold add_edge trs G.empty
  |> K.spanningtree
  |> List.map extract



module SM = Map.Make
    (struct
      type t = V2.t * V2.t
      let compare (v1,v2) (v3,v4) =
        Box2.compare
          (Box2.of_pts v1 v2)
          (Box2.of_pts v3 v4)
    end)

module TM = Map.Make
    (struct
      type t = V2.t * V2.t * V2.t
      let compare = compare
    end)


type color = Black | White
type segment  = V2.t * V2.t
type triangle = V2.t * V2.t * V2.t

type t =
  { qtree : (V2.t * V2.t) Qtree.t;
    smap : triangle list SM.t;
    tmap : color TM.t }


let add_smap seg tri smap =
  let lst =
    try SM.find seg smap
    with Not_found -> []
  in
  SM.add seg (tri :: lst) smap

let create_smap_tmap trs =
  let rs = ref SM.empty in
  let rt = ref TM.empty in
  D.iter_triangles
    (fun v1 v2 v3 ->
       let smap =
         !rs
         |> add_smap (v1, v2) (v1, v2, v3)
         |> add_smap (v2, v3) (v1, v2, v3)
         |> add_smap (v3, v1) (v1, v2, v3)
       in
       rs := smap;
       rt := TM.add (v1,v2,v3) Black !rt)
    trs; !rs,!rt

let create_qtree trs =
  D.fold
    (fun v1 v2 acc ->
       let b = Box2.of_pts v1 v2 in
       (b,(v1,v2)) :: acc)
    trs []
  |> Qtree.create ~depth:16

let create pts =
  let trs = delaunay pts in
  let qtree = create_qtree trs in
  let smap,tmap = create_smap_tmap trs in
  { qtree; smap; tmap }


let triangles_intersecting ((v1,v2) as v) lvl =
  let box = Box2.of_pts v1 v2 in
  Qtree.fold box
    (fun (_,v') acc ->
       if Intersect.seg_seg v v'
       then (SM.find v' lvl.smap) :: acc
       else acc)
    lvl.qtree []
  |> List.concat

let remove_triangle t lvl =
  let tmap = TM.add t White lvl.tmap in
  { lvl with tmap }


let fold (f: triangle -> 'a -> 'a) lvl acc =
  TM.fold
    (fun t c acc ->
       match c with
       | Black -> f t acc
       | White -> acc)
    lvl.tmap acc

let iter (f: triangle -> unit) lvl =
  TM.iter
    (fun t c ->
       match c with
       | Black -> f t
       | White -> ())
    lvl.tmap
