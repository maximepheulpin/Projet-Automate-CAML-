module MAIN:AEF=
  
struct

(*QUESTION 1*)
  type alpha = char list
  type etat = int
  type q = etat list
  type f = etat list
  type delta = transition list and transition = etat*char*etat
  type aef = {alphabet : alpha ; ensemble_fini_etats :q  ; application : delta ; q0 : etat ; etats_acceptants : f }
(*Fonction auxiliaire*)
  let rec trouver_char : delta*etat -> char = fun (a,etat) -> match a with
    |[] -> ' '
    |(e,c,e')::t -> if e = etat then c else trouver_char(t,etat)
            
  let rec retourner_transition : delta*char -> transition = fun (a,et) -> match a with 
    |[] -> (0,' ',0)
    |(e,c,e')::t -> let d = (e,c,e') in if c = et then d else retourner_transition(t,et)
            
  let nouveau_etat : transition -> etat = fun transition -> let (e,c,e') = transition in e'
      
  let rec actualiser : transition*delta -> delta = fun (tr,a) -> match a with
    |[] -> []
    |(e,c,e')::t -> let h = (e,c,e') in if tr = h then actualiser(tr,t) else h::actualiser(tr,t);;

  let concat : 'a list -> 'a list -> 'a list = fun l1 -> fun l2 -> 
    let rec aux : 'a list -> 'a list -> 'a list = fun l -> fun l' ->
      match l with 
      |[] ->l'
      |h::t -> if not(List.mem h l') then (aux t (h::l')) else (aux t l') in
  
    if (List.length l1)=0 then
      l2
    else if (List.length l2)=0 then
      l1
    else if(List.length l1)<=(List.length l2) then 
      aux l1 l2
    else
      aux l2 l1

  let rec random : int * q -> etat = fun (i ,etats) -> if not(List.mem i etats) then i else (random (i+1,etats))


(* QUESTION 2 *) 


  let est_correct : aef -> bool = fun a -> 
    let rec est_sous_ensembles : f -> q -> bool = fun e' -> fun e ->
      if (List.length e) >= (List.length e') then 
        match e' with 
        |[]->true
        |h::t -> (List.mem h e) && (est_sous_ensembles t e) 
      else false in 
    let rec recherche = fun list elt -> match list with
      |[] -> false 
      |h::t -> let (e,c,r) = h in 
          if (elt = e || elt = r) then true else (recherche t elt) in 
    let rec test : delta -> q -> bool = fun d -> fun q -> match q with
      |[] -> true
      |h::t -> (recherche d h) && (test d t) in
    a.alphabet!=[] && (est_sous_ensembles a.etats_acceptants a.ensemble_fini_etats) && (test a.application a.ensemble_fini_etats)


(*QUESTION 3*)


  let est_complet : aef -> bool = fun a -> 
    let rec est_present : char -> delta -> bool = fun c -> fun d ->
      match d with 
      |[]->false
      |(a1,b1,a2)::t-> if b1=c then true || (est_present c t) else false || (est_present c t)
    in 
    let rec sont_present : alpha -> delta -> bool = fun alpha -> fun d ->
      match alpha with 
      |[]->true
      |h::t-> (est_present h d) && sont_present t d
    in 
    if(a.alphabet !=[] && a.application!=[]) then
      sont_present a.alphabet a.application
    else
      false


(* QUESTION 4 *) 


  let completer : aef -> aef = fun a -> 
    let rec est_present : char -> delta -> bool = fun c -> fun d ->
      match d with 
      |[]->false
      |(a1,b1,a2)::t->if b1=c then true ||  (est_present c t) else false ||  (est_present c t) in
                                                                          
    let rec recherche_char : alpha -> delta -> char = fun alpha -> fun del -> match alpha with
      |[]->' '
      |h::t->if (est_present h del) then (recherche_char t del) else h in

    let random_etat : aef -> etat = fun a -> 
      let rec calcul (ae,acc) = if List.mem acc ae.ensemble_fini_etats then calcul(ae,acc+1) else acc
      in (calcul (a,0)) in

    let ajout = fun (list,elt) -> if list = [] then [elt] else elt::list in

    let ajout_transition = fun (list,etat,char) -> (etat,char,etat)::list in
    let nb = (random_etat a) in 
    let nouveau_char = (recherche_char a.alphabet a.application) in
    if (est_complet a) 
    then a 
    else {alphabet = a.alphabet; 
          ensemble_fini_etats = ajout (a.ensemble_fini_etats, nb); 
          application = ajout_transition (a.application,nb,nouveau_char); 
          q0 = a.q0; 
          etats_acceptants = a.etats_acceptants}


(* QUESTION 5 *) 


  let langage_vide : aef -> bool = fun a -> if (a.application = [] || a.etats_acceptants = []) then true else false


(* QUESTION 6 *) 


  let est_deterministe : aef -> bool = fun a -> let rec aux (l,acc) = match l with 
      |[] -> true
      |(e,c,s)::t -> if List.mem c acc then false else aux (t,c::acc)
    in aux(a.application,[])


(* QUESTION 7 *) 


  let rec lecture_car : transition -> int -> char option = 
    fun t -> fun etat -> let (e,c,e') = t in if etat = e then Some c else None
  

(* QUESTION 8 *) 


  let lecture_mot : delta*etat -> string = fun (list,et) -> 
    let rec aux (l,e,acc) = let vl = trouver_char(l,e) in let r = retourner_transition (l,vl) in let c = lecture_car r e in 
      if c != None then aux(actualiser(r,l),nouveau_etat r,acc^(Char.escaped(vl))) else acc
    in aux(list,et,"")


(* QUESTION 9 *) 


  let accepter_mot : string*aef -> bool = fun (mot,a) -> 
    let liste_mots_acceptants : delta*etat*aef -> string list = fun (list,et,a) ->
      let char_of_option : char option -> char = fun s -> match s with
        |None -> ' '
        |Some(a) -> a in
      let der : string -> char = fun s -> String.get s (String.length s -1) in
      let rec aux (l,e,ae,acc,liste,cpt) =
        let vl = trouver_char(l,e) in
        let r = retourner_transition (l,vl) in
        let c = lecture_car r e in 
        if c <> None && List.mem (nouveau_etat (retourner_transition (l,der (Char.escaped (char_of_option c))))) ae.etats_acceptants then
          let nv = acc^(Char.escaped(vl)) in
          aux(actualiser(r,l),nouveau_etat r,ae,acc^(Char.escaped(vl)),nv::liste,cpt+1)
        else 
        if cpt > List.length l then liste else aux(actualiser(r,l),nouveau_etat r,ae,acc^(Char.escaped(vl)),liste,cpt+1)
      in
      let l = aux(list,et,a,"",[],0) in l in
    List.mem mot (liste_mots_acceptants (a.application,a.q0,a))


(* QUESTION 10 *) 


  let union : aef -> aef -> aef = fun a -> fun a' -> 
    let alphabet_=(concat a.alphabet a'.alphabet) in 
    let ensemble_fini_etats_=(concat a.ensemble_fini_etats a'.ensemble_fini_etats) in 
    let q0_=random (0,ensemble_fini_etats_) in
    let application_=(concat a.application a'.application) in
  
    let union_acceptants = 
      if(List.mem a.q0 a.etats_acceptants)||(List.mem a'.q0 a'.etats_acceptants) then
        q0_ ::(concat a.etats_acceptants a'.etats_acceptants) 
      else (concat a.etats_acceptants a'.etats_acceptants) in 
  
    {alphabet=alphabet_; ensemble_fini_etats=q0_::ensemble_fini_etats_; application=application_; q0= q0_; etats_acceptants=union_acceptants}


(* QUESTION 11 *) 


  let reco_union : aef -> aef -> aef = fun a -> fun a' -> 
    let rec x_n : transition -> delta -> delta -> delta = fun (a,b,c) -> fun l' -> fun acc ->
      match l' with 
      |[]->acc
      |(e,i,e')::t->(x_n (a,b,c) t ((c,i,e')::acc)) in 
    let rec concat_transition : delta -> delta -> delta -> delta= fun l1 -> fun l2 -> fun acc ->
      match l1 with
      |[]->acc
      |h::t->(concat_transition t l2 (concat (x_n h l2 [h]) acc)) in 
    let concat_11 : delta -> delta -> delta = fun l -> fun l' -> concat l (concat_transition l l' []) in
    let alphabet_=(concat a.alphabet a'.alphabet) in 
    let ensemble_fini_etats_=(concat a.ensemble_fini_etats a'.ensemble_fini_etats) in 
    let application_=(concat_11 a.application a'.application) in
    let q0_=a.q0 in
    let union_acceptants = 
      if(List.mem a'.q0 a'.etats_acceptants) then
        (concat a.etats_acceptants a'.etats_acceptants) 
      else a'.etats_acceptants in 
    {alphabet=alphabet_; ensemble_fini_etats=ensemble_fini_etats_; application=application_; q0= q0_; etats_acceptants=union_acceptants} 


(*QUESTION 12*)


  let afficher : aef -> unit = fun q -> 
    let rec aux : string -> delta -> string = fun acc -> function
      |[]->acc
      |(e,a,e')::t->(aux ((string_of_int e) ^ " ---> (" ^ (Char.escaped a) ^ ") " ^ (string_of_int e') ^ "\n" ^ acc) t) in
  
    print_string (aux "" q.application) 

end