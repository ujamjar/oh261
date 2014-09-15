open Printf

module Src = Ovideo.Bits.File_source
module R = Ovideo.Bits.Reader(Src)
module H261 = Oh261.Decode.Make(R)

let bits = R.init stdin
let h261 = H261.State.init bits

let () = 
  let frameno = ref 0 in
  while true do
    printf "decoding frame %i...%!" !frameno;
    H261.decode_picture h261;
    printf "\n%!"
  done

