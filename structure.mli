module type Structure = sig 
    type t
    type 'a value = ValueOne of 'a | ValueKey of 'a*int list
    val empty : unit -> t
    val add : t -> 'a value -> t
    val compare : 'a -> 'a -> int
    val search : t -> int list -> 'a value
end;;