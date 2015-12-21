open Gg

type 'a t

val create: depth:int -> (Box2.t * 'a) list -> 'a t

val map:  Box2.t -> ((Box2.t * 'a) -> 'b) -> 'a t -> 'b t
val fold: Box2.t -> ((Box2.t * 'a) -> 'b -> 'b) -> 'a t -> 'b -> 'b
val iter: Box2.t -> ((Box2.t * 'a) -> unit) -> 'a t -> unit
