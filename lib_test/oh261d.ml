open Printf

let log_file = ref ""
let bits_file = ref "-"
let yuv_file = ref ""

(* command line *)
let () = 
  Arg.parse [
    "-i", Arg.Set_string bits_file, "h.261 file [stdin]";
    "-o", Arg.Set_string yuv_file, "output yuv file [none]";
    "-log", Arg.Set_string log_file, "log file (if log_level>0) [stderr]";
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

let () = 
  while true do
    H261.decode_picture h261;
    match yuv_file with
    | Some(out) ->
      P.iter (output_byte out) h261.H261.State.cur.F.y;
      P.iter (output_byte out) h261.H261.State.cur.F.u;
      P.iter (output_byte out) h261.H261.State.cur.F.v
    | _ -> ()
  done

