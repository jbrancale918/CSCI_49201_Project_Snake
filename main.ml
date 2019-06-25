
open Tsdl

(* Game state *)
module State = struct

  (* make initial game state *)
  let make (x, y) = 
    ( x, y, 0.0, 0.0 )  (* x  y  velocity-x  velocity-y *)

  (* move in increments that singly directional *)
  let push (fx, fy) dt state = 
      let (x, y, vx, vy) = state in
      (x, y, fx*.dt, fy*.dt)

  (* update over time dt *)
  let update (w, h) dt st = 
    let (x, y, vx, vy) = st in


    (* displacement *)
    let x = x +. vx*.dt in
    let y = y +. vy*.dt in

    (* wrap around *)

    if x < 0.0 then
      (float w, y, vx, vy)
    else if x > float w then
      (0.0, y, vx , vy)
    else if y < 0.0 then
      (x, float h, vx, vy )
    else if y > float h then
      (x, 0.0, vx, vy)
    else
      (x, y, vx, vy)

end

type event = 
  Up | Down | Left | Right | Exit
   
let rec get_event () =
  
  let e = Sdl.Event.create () in
  
  if Sdl.poll_event(Some e) then
    match Sdl.Event.get e Sdl.Event.typ |> Sdl.Event.enum with
    | `Quit -> Some Exit
    
    | `Key_down -> 
        
        let keycode = Sdl.Event.get e Sdl.Event.keyboard_keycode in
        let repeat = Sdl.Event.get e Sdl.Event.keyboard_repeat in

        if repeat = 0 then (* not a repeat *)
          begin
            if keycode = Sdl.K.q then Some Exit
            else if keycode = Sdl.K.left || keycode = Sdl.K.a then Some Left
            else if keycode = Sdl.K.right || keycode = Sdl.K.d then Some Right
            else if keycode = Sdl.K.up || keycode = Sdl.K.w then Some Up
            else if keycode = Sdl.K.down || keycode = Sdl.K.s then Some Down
            else None
          end
        else
          None

    | _ -> get_event () (* if it's an even of another type, get the next event *)
  else
    None

let round x = int_of_float (floor (0.5 +. x))

let draw win rend tex state =
  (* draw the background *)
  ignore (Sdl.set_render_draw_color rend 32 32 32 255);
  ignore (Sdl.render_clear rend);
  
  (* draw the ball *)
  let (x, y, _, _) = state in
  
  let tex_rect = Sdl.Rect.create 0 0 20 20 in
  let dst_rect = Sdl.Rect.create (round x - 10) (round y - 10) 20 20 in
  ignore (Sdl.render_copy ~src:tex_rect ~dst:dst_rect rend tex);
    
  Sdl.render_present rend


let run w h win rend tex =
        
  let rec loop time_prev st =
      
    Sdl.delay 250l; (* in milliseconds *)
        
    let time_cur = Int32.to_int (Sdl.get_ticks()) in
    (* elapsed time in seconds *)
    let dt = float (time_cur - time_prev) *. 0.001 in
    (* print frame rate *)
    Printf.printf "FPS: %g\n%!" (1.0 /. dt);
    
    match get_event () with
    | Some Exit -> ()
    | opt ->
        (* process one key pressed, if needed *)
        let st2 = 
          let force = ((float_of_int w) *. 0.05) in
          match opt with
          | None -> st
          | Some Left -> State.push (-.force, 0.0) 1.0 st
          | Some Up -> State.push (0.0, -.force) 1.0 st
          | Some Right -> State.push (force, 0.0) 1.0 st
          | Some Down -> State.push (0.0, force) 1.0 st
          | Some _ -> st
        in

        (* if the game state should update with time, update it: *)
        let st3 = State.update (w, h) dt st2 in

        (* draw *)
        draw win rend tex st3;

        (* call the loop again *)
        loop time_cur st3
  in

  loop (Int32.to_int (Sdl.get_ticks())) (State.make (0.5 *. float w, 0.5 *. float h));
  
  Sdl.destroy_texture tex;
  Sdl.destroy_renderer rend;
  Sdl.destroy_window win;
  Sdl.quit (); 
  exit 0



let () =
  
  let width = 1000 in
  let height = 500 in
  
  (* init SDL *)
  match Sdl.init Sdl.Init.video with 
  | Error (`Msg e) -> Sdl.log "Init error: %s" e; exit 1
  | Ok () -> 
    ( match Sdl.create_window ~w:width ~h:height "TSDL Demo" Sdl.Window.(shown + input_focus) with 
      | Error (`Msg e) -> Sdl.log "Create window error: %s" e; exit 1
      | Ok win -> 
        ( match Sdl.create_renderer win ~index:(-1) ~flags:Sdl.Renderer.(accelerated + presentvsync) with
          | Error (`Msg e) -> Sdl.log "Create renderer error: %s" e; exit 1
          | Ok rend ->
            ( match Sdl.load_bmp "img/pic.bmp" with
            | Error (`Msg e)  -> Sdl.log "Load bmp error: %s" e; exit 1
            | Ok surf ->

                let win_px_fmt = Sdl.get_window_pixel_format win in

                (* convert surface pixel format *)
                let surf = match Sdl.convert_surface_format surf win_px_fmt with
                  | Ok s -> s
                  | _ -> surf
                in
                
                (* set the transparent color (0,255,255) *)
                begin match Sdl.alloc_format win_px_fmt with
                  | Ok fmt -> 
                      let bg_color_uint = Sdl.map_rgb fmt 0 255 255 in
                      ignore(Sdl.set_color_key surf true bg_color_uint);
                      Sdl.free_format fmt
                  | _ -> ()
                end;

                (* create texture out of the surface and start main loop *)
                begin match Sdl.create_texture_from_surface rend surf with
                  | Ok tex -> 
                      (* all is good, run the program *)
                      run width height win rend tex

                  | Error (`Msg e) -> Sdl.log "Creating texture error: %s" e; exit 1
                end
            )
        )
    )

