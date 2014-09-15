module Source_format : sig
  type t = 
    | Qcif 
    | Cif
end

module Picture_header : sig
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
    val empty : t
end

module Gob_header : sig
  type t = 
    {
      group_number : int;
      gob_quant : int;
    }
    val empty : t
end

val num_gobs : Source_format.t -> int
val frame_dims : Source_format.t -> int * int
val mb_to_pos : int -> int -> int * int


