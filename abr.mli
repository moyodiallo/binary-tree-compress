module type Abr = sig
  type t
  val construct : 'a list -> ('a -> 'a -> int) -> t
  val identify : t -> t
  val identity : t -> string
end;;


