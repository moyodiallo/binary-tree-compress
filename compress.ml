(*
  la representation du d'un arb Node(elt, sous_node_gauche, sous_node_droite)
*)
type 'a abr = Nil | Node of 'a * 'a abr * 'a abr;;

(* 
  ajouter un elt a une arbre
  abr : 'a abr
  elt : 'a
  compare: 'a->'a->int
  return: abr
*)
let rec add abr elt compare = 
  match abr with
  | Nil          -> Node(elt,Nil,Nil)
  | Node(e,g,d)  -> if compare elt e < 0 then  Node(e,add g elt compare, d)
                    else Node(e,g, add d elt compare)
;;

(*
  la generation d'une aleatoire avec equiprobabilite
  n: int
  return: list
*)
let generate n = 
  let rec genere_list n = if n = 0 then [0] else n-1::genere_list (n-1)
  in 
  let rec shuffle l =
    let rec without l n = match l with
      | []    -> []
      | a::l  -> if n = a then l else a::without l n
    in
    let length = List.length l in 
      if length = 0 then [] 
      else 
      let n = List.nth l (Random.int length) in
      n::shuffle (without l n)
  in shuffle (genere_list n)
;;

(*
  construction d'un abr
  l: list
  compare: 'a->'a->int
  return : 'a abr
*)
let construct_abr l compare = 
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
let rec construct_identity abr = 
  match abr with
  | Nil             -> "",Nil
  | Node(a,Nil,Nil) -> "()",Node((a,"()"),Nil,Nil)
  | Node(a,g,d)     -> 
    let id_g,ng = construct_identity g and id_d,nd = construct_identity d in 
    let id = "("^id_g^")"^id_d in id,Node((a,id),ng,nd)
;;

(*
  permet de recupere l'identite simplement d'un abr
  abr: ABR
*)
let identity_abr abr = match abr with
  | Nil             -> failwith "l'indentite de Nil"
  | Node((_,i),_,_) -> i
;;

(*
  La structure pour l'arbre binaire compressee
  None  : les noeuds vide
  Node0 : les noeuds sans cles (exemp racine dans certaine cas)
  Node1 : les noeuds avec une cle qui correspond au sous arbre gauche
  Node2 : les noeuds avec 2 cle qui correspond a chaque sous arbre respectivement

  Les elements mutables dans la liste qui correspond dans les cas ou on veut ajouter un element
    dans un noeud, ou pointer vers un autre node sans faire une copy complete, en gros cette structure
    revient a la particularite de OCaml
  Remarque: toute construction d'enregistrement d'un noeud(voir OCaml) est effectuer qu'une seule fois
*)
type 'a compress   = 
  None 
  | Node0 of { mutable e:'a ; mutable g: 'a compress; mutable d: 'a compress }
  | Node1 of { mutable e:'a ; mutable g: 'a compress; mutable d: 'a compress ; key2: int}
  | Node2 of { mutable e:'a ; mutable g: 'a compress; mutable d: 'a compress ; key1: int; key2: int}
;;

(*
  les donnees stocker dans l'ABR
  ValueOne: la donnee seule sans cle
  ValueKey: (donnee,cle emmagasiner)
*)
type 'a value = 
  | ValueOne  of 'a
  | ValueKey  of 'a*int list
;;


(*
  remplie les data de abr-simple dans l'abr compresse
  abr: ABR simple contenant les valeurs
  abr_com: ABR compressee(la structure compressee)
  add: pour l'ajout d'une donnee dans un noeud
*)
let rec fill abr abr_com add keys = 
  match abr,abr_com with
  | Nil,_                     -> ()
  | Node((a,i),g,d), Node0(e) -> (e.e <- add a e.e keys ; fill g e.g add keys; fill d e.d add keys)
  | Node((a,i),g,d), Node1(e) -> (e.e <- add a e.e keys ; fill g e.g add keys; fill d e.d add (keys@[e.key2]))
  | Node((a,i),g,d), Node2(e) -> (e.e <- add a e.e keys ; fill g e.g add (keys@[e.key1]); fill d e.d add (keys@[e.key2]))
  | _,_                       -> failwith "pas sense etre attent"

(*
  produit une structure ARB compresse en la remplissant des valeurs
  de l'ABR de depart
  abr: ABR simple 
  nbr_node: nombre total d'etiquette(noeud) dans l'ABR
  add: pour l'ajout d'une donnee dans un noeud
*)
let compress abr nbr_node add = 
  true



(*
  compress fait un parcours gauche-etique-droite, c'est-a-dire infixe

    sous function rec compression
      index_array -> un tableau qui lie les index et les 
*)
let compress abr nbr_node = 
  let is_in_identity = true (* ?????? *)
  in
  let add_to_compressed node e  = None
  in
  let rec compress abr index_array identity_list current_identity  = 
    match abr with
    | Nil                  -> None
    | Node((e,id),Nil,Nil) -> None
    | Node((e,id),g,d)     -> None
    | _                    -> None
;;