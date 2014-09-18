open Printf

let source_format = Oh261.Types.Source_format.Qcif
let w,h = Oh261.Types.frame_dims source_format

module Sink = Ovideo.Bits.Buffer_sink
module W = Ovideo.Bits.Writer(Sink)
module Me = Ovideo.Motion.Full_search(Ovideo.Motion.Sad)
module H261 = Oh261.Encode.Make(W)(Me)
module F = Ovideo.Frame.U8
module P = F.Plane

let bits = W.init (Buffer.create 1024)
let h261 = H261.State.init bits source_format

let read_frame () = 
  for y=0 to h-1 do
    for x=0 to w-1 do
      h261.H261.State.inp.F.y.{y,x} <- input_byte stdin
    done
  done;
  for y=0 to h/2-1 do
    for x=0 to w/2-1 do
      h261.H261.State.inp.F.u.{y,x} <- input_byte stdin
    done
  done;
  for y=0 to h/2-1 do
    for x=0 to w/2-1 do
      h261.H261.State.inp.F.v.{y,x} <- input_byte stdin
    done
  done

let () = 
  let frameno = ref 0 in
  while true do
    read_frame ();
    H261.encode_intra_picture h261;
    eprintf "encode_intra_picture %i bits %i\n" !frameno (W.pos bits);
    incr frameno;
  done

