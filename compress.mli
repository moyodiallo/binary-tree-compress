module type Compress = sig
  type t
  type abr
  val construct : abr -> t
  val fill : t -> abr -> t
end;;