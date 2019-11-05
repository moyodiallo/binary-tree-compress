(*
module type Abr = sig
  type t
  val construct : 'a list -> ('a -> 'a -> int) -> t
  val identify : t -> t
  val identity : t -> string
end;;
*)

module type Compress = sig
  type t
  type abr
  val construct : abr -> t
  val fill : t -> abr -> t
end;;

module type Structure = sig 
  type t
  type 'a value = ValueOne of 'a | ValueKey of 'a*int list
  val empty : unit -> t
  val add : t -> 'a value -> t
  val compare : 'a -> 'a -> int
  val search : t -> int list -> 'a value
end;;

module Abr = struct

  type 'a t = Nil | Node of 'a * 'a t * 'a t;;

  (*
    construction d'un abr
    l: list
    compare: 'a->'a->int
    return : 'a abr
  *)
  let construct l compare = 
    let rec add abr elt compare = 
      match abr with
      | Nil          -> Node(elt,Nil,Nil)
      | Node(e,g,d)  -> 
        if compare elt e < 0 then  Node(e,add g elt compare, d)
        else Node(e,g, add d elt compare)
    in
    let rec construct abr l = 
      match l with
      | []    -> abr
      | a::[] -> add abr a compare 
      | a::l  -> construct (add abr a compare) l
    in
    construct Nil l
  ;;
    

  (*
    construit l'identite structurelle de l'ABR
    abr: 'a abr
    return: string,arb
  *)
  let identitfy abr =
    let rec construct abr = 
      match abr with
      | Nil             -> "",Nil
      | Node(a,Nil,Nil) -> "()",Node((a,"()"),Nil,Nil)
      | Node(a,g,d)     -> 
        let id_g,ng = construct g and id_d,nd = construct d in 
        let id = "("^id_g^")"^id_d in id,Node((a,id),ng,nd)
    in 
    let _,m = construct abr in m
  ;;

  
  (*
    permet de recupere l'identite simplement d'un abr
    abr: ABR
  *)
  let identity abr = match abr with
  | Nil             -> failwith "l'indentite de Nil"
  | Node((_,i),_,_) -> i
  ;;

end;;

module Compr:Compress = struct
  type 'a t = None | Node0 of { mutable e: 'a ; mutable g: 'a t; mutable d: 'a t ; key_g: int; key_d: int};;
  let construct abr
end;;
