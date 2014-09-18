open Printf

let source_format = Oh261.Types.Source_format.Qcif
let w,h = Oh261.Types.frame_dims source_format

module Sink = Ovideo.Bits.Buffer_sink
module W = Ovideo.Bits.Writer(Sink)
module Me = Ovideo.Motion.Full_search(Ovideo.Motion.Sad)
module H261 = Oh261.Encode.Make(W)(Me)
module F = Ovideo.Frame.U8
module P = F.Plane

let buffer = Buffer.create (1024*100)
let bits = W.init buffer
let h261 = H261.State.init bits source_format

let read_frame () = 
  ignore (P.map_na (fun _ -> input_byte stdin) h261.H261.State.cur.F.y);
  ignore (P.map_na (fun _ -> input_byte stdin) h261.H261.State.cur.F.u);
  ignore (P.map_na (fun _ -> input_byte stdin) h261.H261.State.cur.F.v)

let () = 
  let frameno = ref 0 in
  try
    while true do
      read_frame ();
      H261.encode_intra_picture h261;
      eprintf "encode_intra_picture %i bits %i\n%!" !frameno (W.pos bits);
      incr frameno;
    done
  with End_of_file ->
      let s = Buffer.contents buffer in
      output_string stdout s

