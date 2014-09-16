open Ovideo.Table

module Mba = struct
  type t = 
    {
      mba : int;
      stuff : int;
    } deriving(Show) 
  let codes = 
    [{length =  1; code =  1; data={mba =  1; stuff = 0}};
     {length =  3; code =  3; data={mba =  2; stuff = 0}};
     {length =  3; code =  2; data={mba =  3; stuff = 0}};
     {length =  4; code =  3; data={mba =  4; stuff = 0}};
     {length =  4; code =  2; data={mba =  5; stuff = 0}};
     {length =  5; code =  3; data={mba =  6; stuff = 0}};
     {length =  5; code =  2; data={mba =  7; stuff = 0}};
     {length =  7; code =  7; data={mba =  8; stuff = 0}};
     {length =  7; code =  6; data={mba =  9; stuff = 0}};
     {length =  8; code = 11; data={mba = 10; stuff = 0}};
     {length =  8; code = 10; data={mba = 11; stuff = 0}};
     {length =  8; code =  9; data={mba = 12; stuff = 0}};
     {length =  8; code =  8; data={mba = 13; stuff = 0}};
     {length =  8; code =  7; data={mba = 14; stuff = 0}};
     {length =  8; code =  6; data={mba = 15; stuff = 0}};
     {length = 10; code = 23; data={mba = 16; stuff = 0}};
     {length = 10; code = 22; data={mba = 17; stuff = 0}};
     {length = 10; code = 21; data={mba = 18; stuff = 0}};
     {length = 10; code = 20; data={mba = 19; stuff = 0}};
     {length = 10; code = 19; data={mba = 20; stuff = 0}};
     {length = 10; code = 18; data={mba = 21; stuff = 0}};
     {length = 11; code = 35; data={mba = 22; stuff = 0}};
     {length = 11; code = 34; data={mba = 23; stuff = 0}};
     {length = 11; code = 33; data={mba = 24; stuff = 0}};
     {length = 11; code = 32; data={mba = 25; stuff = 0}};
     {length = 11; code = 31; data={mba = 26; stuff = 0}};
     {length = 11; code = 30; data={mba = 27; stuff = 0}};
     {length = 11; code = 29; data={mba = 28; stuff = 0}};
     {length = 11; code = 28; data={mba = 29; stuff = 0}};
     {length = 11; code = 27; data={mba = 30; stuff = 0}};
     {length = 11; code = 26; data={mba = 31; stuff = 0}};
     {length = 11; code = 25; data={mba = 32; stuff = 0}};
     {length = 11; code = 24; data={mba = 33; stuff = 0}};
     {length = 11; code = 15; data={mba =  0; stuff = 1}}]
end

module Mtype = struct
  type t = 
    {
      intra : bool;
      fil : bool;
      quant : bool;
      mvd : bool;
      cbp : bool;
      coef : bool;
    } deriving(Show)
  let codes = 
    [{length =  4; code = 1; data={intra =  true; fil = false; quant = false; mvd = false; cbp = false; coef =  true}};
     {length =  7; code = 1; data={intra =  true; fil = false; quant =  true; mvd = false; cbp = false; coef =  true}};
     {length =  1; code = 1; data={intra = false; fil = false; quant = false; mvd = false; cbp =  true; coef =  true}};
     {length =  5; code = 1; data={intra = false; fil = false; quant =  true; mvd = false; cbp =  true; coef =  true}};
     {length =  9; code = 1; data={intra = false; fil = false; quant = false; mvd =  true; cbp = false; coef = false}};
     {length =  8; code = 1; data={intra = false; fil = false; quant = false; mvd =  true; cbp =  true; coef =  true}};
     {length = 10; code = 1; data={intra = false; fil = false; quant =  true; mvd =  true; cbp =  true; coef =  true}};
     {length =  3; code = 1; data={intra = false; fil =  true; quant = false; mvd =  true; cbp = false; coef = false}};
     {length =  2; code = 1; data={intra = false; fil =  true; quant = false; mvd =  true; cbp =  true; coef =  true}};
     {length =  6; code = 1; data={intra = false; fil =  true; quant =  true; mvd =  true; cbp =  true; coef =  true}}]
  let empty = {intra=false; fil=false; quant=false; mvd=false; cbp=false; coef=false}
end

module Cbp = struct
  type t = 
    {
      cbp : int;
    } deriving(Show)
  let codes = 
    [{length = 3; code =  7; data={cbp = 60}}; {length = 4; code = 13; data={cbp =  4}};
     {length = 4; code = 12; data={cbp =  8}}; {length = 4; code = 11; data={cbp = 16}};
     {length = 4; code = 10; data={cbp = 32}}; {length = 5; code = 19; data={cbp = 12}};
     {length = 5; code = 18; data={cbp = 48}}; {length = 5; code = 17; data={cbp = 20}};
     {length = 5; code = 16; data={cbp = 40}}; {length = 5; code = 15; data={cbp = 28}};
     {length = 5; code = 14; data={cbp = 44}}; {length = 5; code = 13; data={cbp = 52}};
     {length = 5; code = 12; data={cbp = 56}}; {length = 5; code = 11; data={cbp =  1}};
     {length = 5; code = 10; data={cbp = 61}}; {length = 5; code =  9; data={cbp =  2}};
     {length = 5; code =  8; data={cbp = 62}}; {length = 6; code = 15; data={cbp = 24}};
     {length = 6; code = 14; data={cbp = 36}}; {length = 6; code = 13; data={cbp =  3}};
     {length = 6; code = 12; data={cbp = 63}}; {length = 7; code = 23; data={cbp =  5}};
     {length = 7; code = 22; data={cbp =  9}}; {length = 7; code = 21; data={cbp = 17}};
     {length = 7; code = 20; data={cbp = 33}}; {length = 7; code = 19; data={cbp =  6}};
     {length = 7; code = 18; data={cbp = 10}}; {length = 7; code = 17; data={cbp = 18}};
     {length = 7; code = 16; data={cbp = 34}}; {length = 8; code = 31; data={cbp =  7}};
     {length = 8; code = 30; data={cbp = 11}}; {length = 8; code = 29; data={cbp = 19}};
     {length = 8; code = 28; data={cbp = 35}}; {length = 8; code = 27; data={cbp = 13}};
     {length = 8; code = 26; data={cbp = 49}}; {length = 8; code = 25; data={cbp = 21}};
     {length = 8; code = 24; data={cbp = 41}}; {length = 8; code = 23; data={cbp = 14}};
     {length = 8; code = 22; data={cbp = 50}}; {length = 8; code = 21; data={cbp = 22}};
     {length = 8; code = 20; data={cbp = 42}}; {length = 8; code = 19; data={cbp = 15}};
     {length = 8; code = 18; data={cbp = 51}}; {length = 8; code = 17; data={cbp = 23}};
     {length = 8; code = 16; data={cbp = 43}}; {length = 8; code = 15; data={cbp = 25}};
     {length = 8; code = 14; data={cbp = 37}}; {length = 8; code = 13; data={cbp = 26}};
     {length = 8; code = 12; data={cbp = 38}}; {length = 8; code = 11; data={cbp = 29}};
     {length = 8; code = 10; data={cbp = 45}}; {length = 8; code =  9; data={cbp = 53}};
     {length = 8; code =  8; data={cbp = 57}}; {length = 8; code =  7; data={cbp = 30}};
     {length = 8; code =  6; data={cbp = 46}}; {length = 8; code =  5; data={cbp = 54}};
     {length = 8; code =  4; data={cbp = 58}}; {length = 9; code =  7; data={cbp = 31}};
     {length = 9; code =  6; data={cbp = 47}}; {length = 9; code =  5; data={cbp = 55}};
     {length = 9; code =  4; data={cbp = 59}}; {length = 9; code =  3; data={cbp = 27}};
     {length = 9; code =  2; data={cbp = 39}}] 
end

module Mvd = struct
  type t = 
    {
      mvd : int;
    } deriving(Show)
  let codes = 
    [{length = 11; code = 25; data = {mvd = -16}}; {length = 11; code = 27; data = {mvd = -15}};
     {length = 11; code = 29; data = {mvd = -14}}; {length = 11; code = 31; data = {mvd = -13}};
     {length = 11; code = 33; data = {mvd = -12}}; {length = 11; code = 35; data = {mvd = -11}}; 
     {length = 11; code = 34; data = {mvd =  11}}; {length = 11; code = 32; data = {mvd =  12}}; 
     {length = 11; code = 30; data = {mvd =  13}}; {length = 11; code = 28; data = {mvd =  14}}; 
     {length = 11; code = 26; data = {mvd =  15}}; {length = 11; code = 24; data = {mvd =  16}}; 
     {length = 10; code = 19; data = {mvd = -10}}; {length = 10; code = 21; data = {mvd =  -9}}; 
     {length = 10; code = 23; data = {mvd =  -8}}; {length = 10; code = 22; data = {mvd =   8}}; 
     {length = 10; code = 20; data = {mvd =   9}}; {length = 10; code = 18; data = {mvd =  10}}; 
     {length =  8; code =  7; data = {mvd =  -7}}; {length =  8; code =  9; data = {mvd =  -6}}; 
     {length =  8; code = 11; data = {mvd =  -5}}; {length =  8; code = 10; data = {mvd =   5}}; 
     {length =  8; code =  8; data = {mvd =   6}}; {length =  8; code =  6; data = {mvd =   7}}; 
     {length =  7; code =  6; data = {mvd =   4}}; {length =  7; code =  7; data = {mvd =  -4}}; 
     {length =  5; code =  3; data = {mvd =  -3}}; {length =  5; code =  2; data = {mvd =   3}}; 
     {length =  4; code =  3; data = {mvd =  -2}}; {length =  4; code =  2; data = {mvd =   2}}; 
     {length =  3; code =  3; data = {mvd =  -1}}; {length =  3; code =  2; data = {mvd =   1}}; 
     {length =  1; code =  1; data = {mvd =   0}}]
end

module Coef = struct
  type t = 
    {
      eob : bool;
      escape : bool;
      sign : int;
      run : int;
      level : int;
    } deriving(Show)
  let codes = 
    [{length =  2; code =  2; data = {eob =  true; escape = false; sign =  0; run =  0; level = 0}};
     {length =  2; code =  3; data = {eob = false; escape = false; sign = 11; run =  0; level = 1}};
     {length =  4; code =  4; data = {eob = false; escape = false; sign =  9; run =  0; level = 2}}; 
     {length =  5; code =  5; data = {eob = false; escape = false; sign =  8; run =  0; level = 3}}; 
     {length =  7; code =  6; data = {eob = false; escape = false; sign =  6; run =  0; level = 4}}; 
     {length =  8; code = 38; data = {eob = false; escape = false; sign =  5; run =  0; level = 5}}; 
     {length =  8; code = 33; data = {eob = false; escape = false; sign =  5; run =  0; level = 6}}; 
     {length = 10; code = 10; data = {eob = false; escape = false; sign =  3; run =  0; level = 7}}; 
     {length = 12; code = 29; data = {eob = false; escape = false; sign =  1; run =  0; level = 8}}; 
     {length = 12; code = 24; data = {eob = false; escape = false; sign =  1; run =  0; level = 9}}; 
     {length = 12; code = 19; data = {eob = false; escape = false; sign =  1; run =  0; level =10}}; 
     {length = 12; code = 16; data = {eob = false; escape = false; sign =  1; run =  0; level =11}}; 
     {length = 13; code = 26; data = {eob = false; escape = false; sign =  0; run =  0; level =12}}; 
     {length = 13; code = 25; data = {eob = false; escape = false; sign =  0; run =  0; level =13}}; 
     {length = 13; code = 24; data = {eob = false; escape = false; sign =  0; run =  0; level =14}}; 
     {length = 13; code = 23; data = {eob = false; escape = false; sign =  0; run =  0; level =15}}; 
     {length =  3; code =  3; data = {eob = false; escape = false; sign = 10; run =  1; level = 1}}; 
     {length =  6; code =  6; data = {eob = false; escape = false; sign =  7; run =  1; level = 2}}; 
     {length =  8; code = 37; data = {eob = false; escape = false; sign =  5; run =  1; level = 3}}; 
     {length = 10; code = 12; data = {eob = false; escape = false; sign =  3; run =  1; level = 4}}; 
     {length = 12; code = 27; data = {eob = false; escape = false; sign =  1; run =  1; level = 5}}; 
     {length = 13; code = 22; data = {eob = false; escape = false; sign =  0; run =  1; level = 6}}; 
     {length = 13; code = 21; data = {eob = false; escape = false; sign =  0; run =  1; level = 7}}; 
     {length =  4; code =  5; data = {eob = false; escape = false; sign =  9; run =  2; level = 1}}; 
     {length =  7; code =  4; data = {eob = false; escape = false; sign =  6; run =  2; level = 2}}; 
     {length = 10; code = 11; data = {eob = false; escape = false; sign =  3; run =  2; level = 3}}; 
     {length = 12; code = 20; data = {eob = false; escape = false; sign =  1; run =  2; level = 4}}; 
     {length = 13; code = 20; data = {eob = false; escape = false; sign =  0; run =  2; level = 5}}; 
     {length =  5; code =  7; data = {eob = false; escape = false; sign =  8; run =  3; level = 1}}; 
     {length =  8; code = 36; data = {eob = false; escape = false; sign =  5; run =  3; level = 2}}; 
     {length = 12; code = 28; data = {eob = false; escape = false; sign =  1; run =  3; level = 3}}; 
     {length = 13; code = 19; data = {eob = false; escape = false; sign =  0; run =  3; level = 4}}; 
     {length =  5; code =  6; data = {eob = false; escape = false; sign =  8; run =  4; level = 1}}; 
     {length = 10; code = 15; data = {eob = false; escape = false; sign =  3; run =  4; level = 2}}; 
     {length = 12; code = 18; data = {eob = false; escape = false; sign =  1; run =  4; level = 3}}; 
     {length =  6; code =  7; data = {eob = false; escape = false; sign =  7; run =  5; level = 1}}; 
     {length = 10; code =  9; data = {eob = false; escape = false; sign =  3; run =  5; level = 2}}; 
     {length = 13; code = 18; data = {eob = false; escape = false; sign =  0; run =  5; level = 3}}; 
     {length =  6; code =  5; data = {eob = false; escape = false; sign =  7; run =  6; level = 1}}; 
     {length = 12; code = 30; data = {eob = false; escape = false; sign =  1; run =  6; level = 2}}; 
     {length =  6; code =  4; data = {eob = false; escape = false; sign =  7; run =  7; level = 1}}; 
     {length = 12; code = 21; data = {eob = false; escape = false; sign =  1; run =  7; level = 2}}; 
     {length =  7; code =  7; data = {eob = false; escape = false; sign =  6; run =  8; level = 1}}; 
     {length = 12; code = 17; data = {eob = false; escape = false; sign =  1; run =  8; level = 2}}; 
     {length =  7; code =  5; data = {eob = false; escape = false; sign =  6; run =  9; level = 1}}; 
     {length = 13; code = 17; data = {eob = false; escape = false; sign =  0; run =  9; level = 2}}; 
     {length =  8; code = 39; data = {eob = false; escape = false; sign =  5; run = 10; level = 1}}; 
     {length = 13; code = 16; data = {eob = false; escape = false; sign =  0; run = 10; level = 2}}; 
     {length =  8; code = 35; data = {eob = false; escape = false; sign =  5; run = 11; level = 1}}; 
     {length =  8; code = 34; data = {eob = false; escape = false; sign =  5; run = 12; level = 1}}; 
     {length =  8; code = 32; data = {eob = false; escape = false; sign =  5; run = 13; level = 1}}; 
     {length = 10; code = 14; data = {eob = false; escape = false; sign =  3; run = 14; level = 1}}; 
     {length = 10; code = 13; data = {eob = false; escape = false; sign =  3; run = 15; level = 1}}; 
     {length = 10; code =  8; data = {eob = false; escape = false; sign =  3; run = 16; level = 1}}; 
     {length = 12; code = 31; data = {eob = false; escape = false; sign =  1; run = 17; level = 1}}; 
     {length = 12; code = 26; data = {eob = false; escape = false; sign =  1; run = 18; level = 1}}; 
     {length = 12; code = 25; data = {eob = false; escape = false; sign =  1; run = 19; level = 1}}; 
     {length = 12; code = 23; data = {eob = false; escape = false; sign =  1; run = 20; level = 1}}; 
     {length = 12; code = 22; data = {eob = false; escape = false; sign =  1; run = 21; level = 1}}; 
     {length = 13; code = 31; data = {eob = false; escape = false; sign =  0; run = 22; level = 1}}; 
     {length = 13; code = 30; data = {eob = false; escape = false; sign =  0; run = 23; level = 1}}; 
     {length = 13; code = 29; data = {eob = false; escape = false; sign =  0; run = 24; level = 1}}; 
     {length = 13; code = 28; data = {eob = false; escape = false; sign =  0; run = 25; level = 1}}; 
     {length = 13; code = 27; data = {eob = false; escape = false; sign =  0; run = 26; level = 1}}; 
     {length =  6; code =  1; data = {eob = false; escape =  true; sign =  0; run =  0; level = 0}}]
  let first = 
     {length =  2; code =  1; data = {eob = false; escape = false; sign = 12; run =  0; level = 1}}
end

