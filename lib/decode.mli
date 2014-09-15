open Ovideo
open Types

module Bitstream(R : Bits.Reader) : sig
  val find_start_code : R.t -> unit
  val find_picture_start_code : R.t -> unit
  val read_spare : R.t -> unit
  val read_picture_header : R.t -> Picture_header.t
  val read_gob_header : R.t -> Gob_header.t
  val lookup_code : R.t -> 'a Table.table * int -> 'a
  val lookup_coef : R.t -> bool -> bool -> bool * int * int
end

module Quant : sig
  val inv : int -> int -> int
end

module Recon : sig
  val clip : int -> int
  val copy : x:int -> y:int -> ref:Frame.U8.t -> cur:Frame.U8.t -> unit
  val intra : x:int -> y:int -> pred:Frame.SInt.Plane.t -> cur:Frame.U8.Plane.t -> unit
  val skip : x:int -> y:int -> mvx:int -> mvy:int -> 
    ref:Frame.U8.Plane.t -> cur:Frame.U8.Plane.t -> unit
  val mc : x:int -> y:int -> mvx:int -> mvy:int -> 
    ref:Frame.U8.Plane.t -> pred:Frame.SInt.Plane.t -> cur:Frame.U8.Plane.t -> unit
  val fil : x:int -> y:int -> mvx:int -> mvy:int -> 
    ref:Frame.U8.Plane.t -> pred:Frame.SInt.Plane.t -> cur:Frame.U8.Plane.t -> unit
end

module Make(R : Bits.Reader) : sig

  module State : sig
    type t = 
      {
        mutable bits : R.t;
        mutable picture_header : Picture_header.t;
        mutable gob_header : Gob_header.t;
        mutable ref : Frame.U8.t;
        mutable cur : Frame.U8.t;
        mutable mba : int;
        mutable mbadiff : int;
        mutable mvx : int;
        mutable mvy : int;
        mutable mvdx : int;
        mutable mvdy : int;
        mutable cbp : int;
        mutable quant : int;
        mutable cofs : Frame.SInt.Plane.t;
        mutable mtype : Tables.Mtype.t;
      }
    val init : R.t -> t
  end

  val read_mb_header : State.t -> unit
  val get_mv : int -> int -> int
  val get_block : State.t -> int -> 
    (Frame.U8.Plane.t * Frame.U8.Plane.t * int * int * int * int * int)
  val decode_coefs : State.t -> bool -> int -> unit
  val copy_skipped_mbs : State.t -> int -> unit
  val compute_mvs : State.t -> int -> unit
  val update_mv_pred : State.t -> unit
  val decode_block : State.t -> int -> int -> int -> unit
  val decode_blocks : State.t -> int -> int -> unit
  val decode_mb : State.t -> unit

end

