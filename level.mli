open Gg

type t

type segment  = V2.t * V2.t
type triangle = V2.t * V2.t * V2.t

val biased_points: x:float -> y:float -> nbr:int -> V2.t list
val random_points: x:float -> y:float -> nbr:int -> V2.t list

val spanning_tree: V2.t list -> segment list

val create: V2.t list -> t
val triangles_intersecting: segment -> t -> triangle list
val remove_triangle : triangle -> t -> t

val fold: (triangle -> 'a -> 'a) -> t -> 'a -> 'a
val iter: (triangle -> unit) -> t -> unit
