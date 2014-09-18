module Source_format : sig
  type t = 
    | Qcif 
    | Cif
    deriving(Show)
end

module Picture_header : sig
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
    val empty : t
end

module Gob_header : sig
  type t = 
    {
      mutable group_number : int;
      mutable gob_quant : int;
    } deriving(Show)
    val empty : t
end

module Mtype : sig
  type t = 
    {
      mutable intra : bool;
      mutable fil : bool;
      mutable quant : bool;
      mutable mvd : bool;
      mutable cbp : bool;
      mutable coef : bool;
    } 
end

val num_gobs : Source_format.t -> int
val frame_dims : Source_format.t -> int * int
val mb_to_pos : int -> int -> int * int
val alloc_frame : Source_format.t -> Ovideo.Frame.U8.t

