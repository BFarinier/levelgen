open Gg
open Graphics
open Level

let rec sleep t =
  if t > 0. then
    let now = Unix.gettimeofday () in
    (try ignore (Unix.select [] [] [] t) with
     | _ -> ());
    sleep (t -. ((Unix.gettimeofday ()) -. now))

let ( >> ) v f =
  let x,y = V2.to_tuple v in
  f (truncate x) (truncate y)

let print_path color =
  set_color color;
  List.iter
    (fun (v1,v2) ->
       v1 >> moveto;
       v2 >> lineto)

let print_level color =
  set_color color;
  iter
    (fun (v1,v2,v3) ->
       v1 >> moveto;
       v2 >> lineto;
       v3 >> lineto;
       v1 >> lineto)

let () =
  Random.self_init ();
  open_graph " 1024x1024";
  auto_synchronize false;

  let biased = biased_points ~x:1024. ~y:1024. ~nbr:1024 in
  let random = random_points ~x:1024. ~y:1024. ~nbr:1024 in
  let path = spanning_tree biased in
  let level = create random in
  print_level blue level;
  print_path red path;
  synchronize ();

  let level =
    List.fold_left
      (fun lvl v ->
         print_level green lvl;
         let lvl =
           List.fold_left (fun lvl tr -> remove_triangle tr lvl)
             lvl (triangles_intersecting v lvl)
         in
         print_level blue lvl;
         print_path red path;
         synchronize ();
         sleep 0.01;
         lvl)
      level path
  in print_level blue level;
  synchronize ();
  sleep 1.;
  exit 0
