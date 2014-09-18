open Printf

let log_file = ref ""
let bits_file = ref "-"
let yuv_file = ref ""
let no_repeat = ref false
let verbose = ref false

(* command line *)
let () = 
  Arg.parse [
    "-i", Arg.Set_string bits_file, "h.261 file [stdin]";
    "-o", Arg.Set_string yuv_file, "output yuv file [none]";
    "-no-repeat", Arg.Set no_repeat, "disable repeated frames";
    "-log", Arg.Set_string log_file, "log file (if log_level>0) [stderr]";
    "-v", Arg.Set verbose, "show stats";
  ] 
  (fun _ -> failwith "invalid anon argument")
  (Sys.argv.(0) ^ " - h261 video decoder")

let bits_file, yuv_file = begin
  if !log_file <> "" then begin
    Oh261.Decode.log_chan := open_out !log_file
  end;
  let yuv_file = 
    if !yuv_file = "" then None
    else if !yuv_file = "-" then Some stdout
    else Some(open_out !yuv_file)
  in
  let bits_file = 
    if !bits_file = "-" then stdin
    else open_in !bits_file
  in
  bits_file, yuv_file
end

(* build the decoder *)

module Src = Ovideo.Bits.File_source
module R = Ovideo.Bits.Reader(Src)
module H261 = Oh261.Decode.Make(R)
module F = Ovideo.Frame.U8
module P = F.Plane

let bits = R.init bits_file
let h261 = H261.State.init bits

(* run the decoder *)

let output_picture () = 
  let open H261.State in
  match yuv_file with
  | Some(out) -> 
    P.iter (output_byte out) h261.cur.F.y;
    P.iter (output_byte out) h261.cur.F.u;
    P.iter (output_byte out) h261.cur.F.v
  | _ -> ()

(* output frames, repeating as necessary to match temporal reference *)
let rec output_pictures tr = 
  let open H261.State in
  let open Oh261.Types.Picture_header in
  if tr = -1 || !no_repeat then begin
    output_picture ();
    h261.picture_header.temporal_reference
  end else begin
    output_picture ();
    let tr = (tr + 1) mod 32 in
    if tr <> h261.picture_header.temporal_reference then
      output_pictures tr
    else
      tr
  end

let () = 
  let bitpos = ref 0 in
  let frameno = ref 0 in
  let tr = ref (-1) in
  while true do
    H261.decode_picture h261;
    tr := output_pictures !tr;
    if !verbose then eprintf "[%6i] [tr=%2i] [bits=%6i]\n%!" !frameno !tr 
      (R.pos bits - !bitpos);
    incr frameno;
    bitpos := R.pos bits;
  done


