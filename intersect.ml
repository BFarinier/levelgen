open Gg


module E = struct
  let epsilon = 0.001
  let (=) a b =
    Pervasives.(=) a b 
    || abs_float (a -. b) <=
       max 1. (max (abs_float a) (abs_float b)) *. epsilon

  let (<=) a b = Pervasives.(a < b) || (a = b)
  let (>=) a b = Pervasives.(a > b) || (a = b)
  let (<) a b = (a <= b) && not (a = b)
  let (>) a b = (a >= b) && not (a = b)
end


type segment  = V2.t * V2.t
type triangle = V2.t * V2.t * V2.t


let is_null f = E.(f = 0.)
let between f = E.(f > 0. && f < 1.)

let cross v1 v2 = V2.(dot v1 (ortho v2))

let seg_seg (v1,v2) (v3,v4) =
  Box2.isects (Box2.of_pts v1 v2) (Box2.of_pts v3 v4)
  && (
    let p,q,r,s = v1, v3, V2.(v2 - v1), V2.(v4 - v3) in
    let c1 = cross r s in
    is_null c1
    && (
      let c2 = cross V2.(q - p) r in
      is_null c2
      && (
        let t = V2.(dot (q - p) r) /. V2.(dot r r) in
        let u = t +. V2.(dot s r) /. V2.(dot r r) in
        between t || between u))
    || (
      let t = (cross V2.(q - p) s) /. (cross r s) in
      let u = (cross V2.(q - p) r) /. (cross r s) in
      (between t && between u)))

let tri_seg (v1,v2,v3) (v4,v5) =
  seg_seg (v1,v2) (v4,v5)
  || seg_seg (v2,v3) (v4,v5)
  || seg_seg (v3,v1) (v4,v5)

