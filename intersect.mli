open Gg

type segment  = V2.t * V2.t
type triangle = V2.t * V2.t * V2.t

val seg_seg: segment -> segment -> bool
val tri_seg: triangle -> segment -> bool
