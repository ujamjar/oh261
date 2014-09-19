open Printf

let in_file = ref "-"
let bits_file = ref "-"
let rec_file = ref ""
let quant = ref 0
let source_format = ref Oh261.Types.Source_format.Qcif

let set_size = function
  | "qcif" -> source_format := Oh261.Types.Source_format.Qcif
  | "cif" -> source_format := Oh261.Types.Source_format.Cif
  | _ -> failwith "expected size is 'qcif' or 'cif'"

let () = 
  Arg.parse [ 
    "-i", Arg.Set_string in_file, "input yuv file [stdin]";
    "-o", Arg.Set_string bits_file, "output bitstream [stdout]";
    "-s", Arg.String(set_size), "cif or qcif";
    "-rec", Arg.Set_string rec_file, "reconstructed yuv file [none]";
    "-q", Arg.Set_int quant, "quantiser";
  ]
  (fun _ -> failwith "invalid anon argument")
  (Sys.argv.(0) ^ " - h261 video encoder")

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

