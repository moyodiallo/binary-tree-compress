module Compr:Compress = struct
  type 'a t = None | Node0 of { mutable e: 'a ; mutable g: 'a t; mutable d: 'a t ; key_g: int; key_d: int};;
  let construct abr
end;;