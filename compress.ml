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
let id_abr abr = match abr with
  | Nil             -> failwith "l'indentite de Nil"
  | Node((_,i),_,_) -> i
;;

(*
  Convention : la cle 0 n'existe pas, c'est-a-dire lien directe
  La structure pour l'arbre binaire compressee
  None  : les noeuds vide
  Node0 : les noeuds avec 2 cle qui correspond a chaque sous arbre respectivement

  Les elements mutables dans la liste qui correspond dans les cas ou on veut ajouter un element
    dans un noeud, ou pointer vers un autre node sans faire une copy complete, en gros cette structure
    revient a la particularite de OCaml
  Remarque: toute construction d'enregistrement d'un noeud(voir OCaml) est effectuer qu'une seule fois
*)
type 'a compress   = 
  None 
  | Node0 of { mutable e:'a ; mutable g: 'a compress; mutable d: 'a compress ; key_g: int; key_d: int}
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

let genValue  = ref 1;;
let genere () = (genValue:= !genValue + 1; !genValue);;

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
    | (n,id)::tl -> if id = id_abr node then n else is_constructed tl node  
  in
  let rec construct abr constructed =
    match abr with
    | Nil                 -> None,0,[]
    | Node((a,i),Nil,Nil) -> 
      let m = is_constructed constructed abr in 
        if m = None then Node0({e = a; g = None; d = None; key_g = 0; key_d = 0}),0,[] else m,genere (),[]
    | Node((a,i),g,d)     -> 
      let m = is_constructed constructed abr in
      if m <> None then m,genere (),constructed 
      else 
        let mG,keyG,constructG = construct g constructed in
        let mD,keyD,constructD = construct d constructG  in
        let m = Node0({e = a; g = mG; d = mD; key_g = keyG; key_d = keyD}) in m,0,constructD
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
let rec fill abr abr_com add keys = 
  match abr,abr_com with
  | Nil,_                     -> ()
  | Node((a,i),g,d), Node0(e) -> 
    ( e.e <- add a e.e keys ; 
      fill g e.g add (keys@(if e.key_g = 0 then [] else [e.key_g])); 
      fill d e.d add (keys@(if e.key_g = 0 then [] else [e.key_g]))
    )
  | _,_                       -> failwith "doit pas etre attent"
;;

(*
  construct et remplie la structure compressee
*)
let compress abr add =  let m = construct abr in fill abr m add [] 
;;

(*
  abr_compressed: consistitue la structure compressee contenant les donnees
  search: la fonction qui consiste a rechercher un element dans les noeud
  element: l'element a rechercher dans un noeud celon la structure utilisee dans le noeud
*)
let exist abr_compressed search element  = (* A remplir*) true;;