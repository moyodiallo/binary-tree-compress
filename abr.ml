(*module type Abr =
  sig
    type 'a abr = Nil | Node of 'a * 'a abr * 'a abr;;
    
    type 'a compress   = 
      None 
      | Node0 of { mutable e:'a ; mutable g: 'a compress; mutable d: 'a compress ; key_g: int; key_d: int}
    ;;
    
    val new_abr : 'a list -> ('a -> 'a -> int) -> 'a abr
    val identitfy : 'a abr -> ('a * string) abr
    val identity : ('a * string) abr -> string
    
    val construct : 'a abr -> 'a compress
    val fill : 'a abr -> 'a compress -> 'a compress 

  end;;
*)

module type Structure = sig 
  type t
  type 'a value = 'a * int list
  val empty : unit -> t
  val add : t -> 'a value -> t
  val compare : 'a -> 'a -> int
  val search : t -> int list -> 'a value
end;;

module Abr(S:Structure) = struct

  type 'a abr = Nil | Node of 'a * 'a abr * 'a abr;;
    
  type 'a compress   = 
    None 
    | Node0 of { mutable e: S.t ; 
      mutable g: 'a compress; 
      mutable d: 'a compress ; 
      key_g: int; key_d: int
      }
  ;;

  (* 
    la generation d'identifiant
    Astuce: generation impaire, pour aussi utilise la somme de ces IDs comme identifiants aussi
  *)
  let genValue  = ref 1;;
  let genere () = (genValue:= !genValue + 2; !genValue);;
  
  (*
    construction d'un abr
    l: list
    compare: 'a->'a->int
    return : 'a abr
  *)
  let new_abr l compare = 
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


   (*
    construct une structure ARB compressed de l'ABR de depart
    abr: ABR simple
    return: une structure compressee sans valeur
    
    construct interne retourne un noeud,une cle,une liste des deja construit
    pour la cle 0 on a un lien directe 
  *)
  let construct abr = 
    let rec is_constructed  constructed node = 
      match constructed with
      | []         -> None
      | (n,id)::tl -> if id = identity node then n else is_constructed tl node  
    in
    let rec construct abr constructed =
      match abr with
      | Nil                 -> None,0,[]
      | Node((a,i),Nil,Nil) ->
        let m = is_constructed constructed abr in 
        if m = None then Node0({e = S.empty (); g = None; d = None; key_g = 0; key_d = 0}),0,[]
        else m,genere (),[]
      | Node((a,i),g,d)     -> 
        let m = is_constructed constructed abr in
        if m <> None then m,genere (),constructed 
        else 
          let mG,keyG,constructG = construct g constructed in
          let mD,keyD,constructD = construct d constructG  in
          let m = Node0({e = S.empty (); g = mG; d = mD; key_g = keyG; key_d = keyD}) in m,0,constructD
    in 
    let m,_,_ = construct abr [] in m
  ;;

(*
  remplie les data de abr-simple dans l'abr compresse
  abr: ABR simple contenant les valeurs
  abr_com: ABR compressee(la structure compressee)
  add: pour l'ajout d'une donnee dans un noeud
  return: void (unit en ocaml)

  Remarque: la cle 0 n'est pas pri en compte puisque c'est un lien direct
*)
let fill abr abr_com = 
  let rec fill abr abr_com keys = 
    match abr,abr_com with
    | Nil,_                     -> ()
    | Node((a,i),g,d), Node0(e) -> 
      ( e.e <- S.add e.e (a,keys) ; 
        fill g e.g (if e.key_g = 0 then keys else e.key_g::keys); 
        fill d e.d (if e.key_d = 0 then keys else e.key_d::keys)
      )
    | _,_                       -> failwith "doit pas etre attent"
  in fill abr abr_com []
;;


  (*
    la function consiste a recherche un element dans un arbre compressee
    abr_compressed: consistitue la structure compressee contenant les donnees
    search: la fonction qui consiste a rechercher un element dans un noeud par des ces indexes
    compare: comparer 2 element qu'un noeud peut contenir
    element: l'element a rechercher dans un noeud celon la structure utilisee dans le noeud
  *)
  let recherche abr_compressed element = 
    let rec exist abr keys element =
      match abr with
      | None                                              -> false
      | Node0({e=e; g=g; d=d; key_g=key_g; key_d=key_d }) -> 
        let m = S.compare (S.search e keys) element in
        if m = 0 then true 
        else if m < 0 then exist g (if key_g = 0 then keys else key_g::keys) element
        else if m > 0 then exist d (if key_d = 0 then keys else key_d::keys) element
        else false
    in exist abr_compressed [] element
  ;;

end;;
