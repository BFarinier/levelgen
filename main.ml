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


let print_segment color =
  set_color color;
  (fun (v1,v2) ->
     v1 >> moveto;
     v2 >> lineto)

let print_path color =
  List.iter (print_segment color)


let print_triangle color =
  set_color color;
  (fun (v1,v2,v3) ->
     v1 >> moveto;
     v2 >> lineto;
     v3 >> lineto;
     v1 >> lineto)

let print_level color =
  iter (print_triangle color)

let () =
  Random.self_init ();
  open_graph " 1024x1024";
  auto_synchronize false;

  let path =
    biased_points ~x:1024. ~y:1024. ~nbr:1024
    |> spanning_tree in
  let level =
    random_points ~x:1024. ~y:1024. ~nbr:1024
    |> create in
  print_level blue level;
  print_path red path;
  synchronize ();

  let level =
    List.fold_left
      (fun lvl v ->
         let lvl =
           List.fold_left
             (fun lvl tr ->
                print_triangle green tr;
                remove_triangle tr lvl)
             lvl (triangles_intersecting v lvl)
         in
         print_level blue lvl;
         print_path red path;
         synchronize ();
         sleep 0.001;
         lvl)
      level path
  in
  print_level blue level;
  synchronize ();
  let _ = wait_next_event [Button_down; Button_up; Key_pressed] in
  exit 0
