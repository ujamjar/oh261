module Source_format = struct
  type t = 
    | Qcif 
    | Cif
end

module Picture_header = struct
  type t = 
    {
      temporal_reference : int;
      split_screen_indicator : int;
      document_camera_indicator : int;
      freeze_picture_release : int;
      source_format : Source_format.t;
      hi_res : int;
      spare : int;
    }
  let empty = 
    {
      temporal_reference = 0;
      split_screen_indicator = 0;
      document_camera_indicator = 0;
      freeze_picture_release = 0;
      source_format = Source_format.Qcif;
      hi_res = 0;
      spare = 0;
    }
end

module Gob_header = struct
  type t = 
    {
      group_number : int;
      gob_quant : int;
    }
  let empty = 
    {
      group_number = 0;
      gob_quant = 0;
    }
end

let num_gobs = function
  | Source_format.Qcif -> 3
  | Source_format.Cif -> 12
  
let frame_dims = function
  | Source_format.Qcif -> 176, 144
  | Source_format.Cif -> 352, 288
  
let mb_to_pos gob mb = 
  assert (mb < 33);
  assert (gob <= 12);
  let y = ((gob / 2) * 3) + (mb / 11) in
  let x = (mb mod 11) + (if (gob land 1) = 0 then 11 else 0) in
  (x,y)



