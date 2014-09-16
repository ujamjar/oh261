open Printf

module Src = Ovideo.Bits.File_source
module R = Ovideo.Bits.Reader(Src)
module H261 = Oh261.Decode.Make(R)
module F = Ovideo.Frame.U8
module P = F.Plane

let bits = R.init stdin
let h261 = H261.State.init bits

let output = true

let () = 
  let frameno = ref 0 in
  while true do
    eprintf "decode_picture %i [@%i]\n" !frameno (R.pos bits);
    H261.decode_picture h261;
    if output then begin
      P.iter (output_byte stdout) h261.H261.State.cur.F.y;
      P.iter (output_byte stdout) h261.H261.State.cur.F.u;
      P.iter (output_byte stdout) h261.H261.State.cur.F.v;
    end;
    incr frameno
  done

