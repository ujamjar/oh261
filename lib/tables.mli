module Mba : sig
  type t = 
    {
      mba : int;
      stuff : int;
    } deriving(Show)
  val codes : t Ovideo.Table.code list
end

module Mtype : sig
  type t = 
    {
      intra : bool;
      fil : bool;
      quant : bool;
      mvd : bool;
      cbp : bool;
      coef : bool;
    } deriving(Show)
  val codes : t Ovideo.Table.code list
  val empty : t
end

module Cbp : sig
  type t = 
    {
      cbp : int;
    } deriving(Show)
  val codes : t Ovideo.Table.code list
end

module Mvd : sig
  type t = 
    {
      mvd : int;
    } deriving(Show)
  val codes : t Ovideo.Table.code list
end

module Coef : sig
  type t = 
    {
      eob : bool;
      escape : bool;
      sign : int;
      run : int;
      level : int;
    } deriving(Show)
  val codes : t Ovideo.Table.code list
  val first : t Ovideo.Table.code
end


