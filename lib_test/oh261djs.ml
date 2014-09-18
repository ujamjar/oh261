module Src = Ovideo.Bits.File_source
module R = Ovideo.Bits.Reader(Src)
module H261 = Oh261.Decode.Make(R)
module F = Ovideo.Frame.U8
module P = F.Plane

let width, height = 352, 288 
let display = true

let bits = R.init (open_in_bin "cifakiyo128.p64")
let h261 = H261.State.init bits

let log s = Firebug.console##log(Js.string s)
let jstri i = Js.string (string_of_int i)
let log_time() =
  let date = (jsnew Js.date_now())##toString() in
  Firebug.console##log(date)
let get_ms () = 
  (jsnew Js.date_now())##getTime()

let get_element e = 
    let d = Dom_html.document in
    Js.Opt.get 
        (d##getElementById (Js.string e))
        (fun () -> assert false)

let mk_canvas width height = 
    let d = Dom_html.document in
    let canvas = Dom_html.createCanvas d in
    let style = canvas##style in
    canvas##width <- width;
    canvas##height <- height;
    style##width <- jstri width;
    style##height <- jstri height;
    canvas

let convert_to_rgb f img = 
  (* 1d array accesses are much faster than 2d *)
  let y' = P.to_1d f.F.y in
  let u' = P.to_1d f.F.u in
  let v' = P.to_1d f.F.v in

  let oy' = ref 0 in
  let ouv' = ref 0 in
  let rgb' = ref 0 in

  for y=0 to height/2-1 do
    for x=0 to width/2-1 do

      let oy, ouv = !oy', !ouv' in

      let y0' = y'.{oy} in
      let y1' = y'.{oy+1} in
      let y2' = y'.{oy+width} in
      let y3' = y'.{oy+width+1} in

      let u' = u'.{ouv} in
      let v' = v'.{ouv} in

      oy' := !oy' + 2;
      ouv' := !ouv' + 1;

      (* approx conversion in integer arithmetic *)
      let r' = (v' + ((v' * 103) lsr 8)) - 179 in
      let g' = ((u' * 88) lsr 8) - 44 + ((v' * 183) lsr 8) - 91 in
      let b' = (u' + ((u' * 198) lsr 8)) - 227 in
     
      let rgb = !rgb' in
      Dom_html.pixel_set img (rgb+0) (y0' + r');
      Dom_html.pixel_set img (rgb+1) (y0' - g');
      Dom_html.pixel_set img (rgb+2) (y0' + b');
      Dom_html.pixel_set img (rgb+3) 255;                   
      let rgb = rgb+4 in                                    
      Dom_html.pixel_set img (rgb+0) (y1' + r');
      Dom_html.pixel_set img (rgb+1) (y1' - g');
      Dom_html.pixel_set img (rgb+2) (y1' + b');
      Dom_html.pixel_set img (rgb+3) 255;                   
      let rgb = rgb+(width*4) in                            
      Dom_html.pixel_set img (rgb+0) (y3' + r');
      Dom_html.pixel_set img (rgb+1) (y3' - g');
      Dom_html.pixel_set img (rgb+2) (y3' + b');
      Dom_html.pixel_set img (rgb+3) 255;                   
      let rgb = rgb-4 in                                    
      Dom_html.pixel_set img (rgb+0) (y2' + r');
      Dom_html.pixel_set img (rgb+1) (y2' - g');
      Dom_html.pixel_set img (rgb+2) (y2' + b');
      Dom_html.pixel_set img (rgb+3) 255;

      rgb' := !rgb' + 8
    done;

    oy' := !oy' + width;
    rgb' := !rgb' + (width*4);

  done

let convert_to_gray f img = 
  let rgb = ref 0 in
  for y=0 to height-1 do
    for x=0 to width-1 do
      let l = f.F.y.{y,x} in
      Dom_html.pixel_set img (!rgb+0) l;
      Dom_html.pixel_set img (!rgb+1) l;
      Dom_html.pixel_set img (!rgb+2) l;
      Dom_html.pixel_set img (!rgb+3) 255;
      rgb := !rgb + 4;
    done
  done

let main _ = 
  
  let div = get_element "output" in
  let canvas = mk_canvas 352 288 in
  let ctx = canvas##getContext(Dom_html._2d_) in
  let img = ctx##createImageData(width, height) in
  let img_data = img##data in
  Dom.appendChild div canvas;

  log_time ();
  let rec decode () =
    try
      let start_dec = get_ms () in
      let () = H261.decode_picture h261 in
      let end_dec = get_ms () in
      let () = if display then
        convert_to_rgb h261.H261.State.cur img_data;
        ctx##putImageData(img, 0., 0.) 
      in
      let end_disp = get_ms () in
      let () = 
        log ("frame dec=[" ^ 
          string_of_float (end_dec -. start_dec) ^
          "] disp=[" ^ 
          string_of_float (end_disp -. end_dec) ^
          "]")
      in
      let _ = Dom_html.window##setTimeout(Js.wrap_callback decode, 0.0) in
      ()
    with _ ->
      log_time ();
      log "finished"
  in

  let () = decode () in


  Js._false

let _ = Dom_html.window##onload <- Dom_html.handler main


