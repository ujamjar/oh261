module Source_format = struct
  type t = 
    | Qcif 
    | Cif 
    deriving(Show)
end

module Picture_header = struct
  type t = 
    {
      mutable temporal_reference : int;
      mutable split_screen_indicator : bool;
      mutable document_camera_indicator : bool;
      mutable freeze_picture_release : bool;
      mutable source_format : Source_format.t;
      mutable hi_res : bool;
      mutable spare : bool;
    } deriving(Show)
  let empty = 
    {
      temporal_reference = 0;
      split_screen_indicator = false;
      document_camera_indicator = false;
      freeze_picture_release = false;
      source_format = Source_format.Qcif;
      hi_res = false;
      spare = false;
    }
end

module Gob_header = struct
  type t = 
    {
      mutable group_number : int;
      mutable gob_quant : int;
    } deriving(Show)
  let empty = 
    {
      group_number = 0;
      gob_quant = 0;
    }
end

module Mtype = struct
  type t = 
    {
      mutable intra : bool;
      mutable fil : bool;
      mutable quant : bool;
      mutable mvd : bool;
      mutable cbp : bool;
      mutable coef : bool;
    } deriving(Show) 
end

type 'a with_prev = 
  {
    mutable prev : 'a;
    mutable cur : 'a;
  }

type 'a mv = 
  {
    mvx : 'a;
    mvy : 'a;
  }

let num_gobs = function
  | Source_format.Qcif -> 3
  | Source_format.Cif -> 12
  
let frame_dims = function
  | Source_format.Qcif -> 176, 144
  | Source_format.Cif -> 352, 288
  
let mb_to_pos gob mb = 
  assert (mb > 0);
  assert (mb <= 33);
  assert (gob <= 12);
  let mb = mb - 1 in
  let y = (((gob-1) / 2) * 3) + (mb / 11) in
  let x = (mb mod 11) + (if (gob land 1) = 0 then 11 else 0) in
  (x,y)

let alloc_frame = 
  let open Ovideo.Frame.U8 in
  function
  | Source_format.Qcif -> make ~chroma:C420 ~w:176 ~h:144
  | Source_format.Cif -> make ~chroma:C420 ~w:352 ~h:288

(* logging utility *)
let string_of_bits n v = 
  let s = Bytes.create n in
  for i=0 to n-1 do
    if v land (1 lsl i) = 0 then Bytes.set s i '0'
    else Bytes.set s i '1'
  done;
  Bytes.to_string s


