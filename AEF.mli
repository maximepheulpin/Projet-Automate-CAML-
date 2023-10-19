module type AEF =
sig 
  
  type alpha
  type etat 
  type q 
  type f 
  type delta = transition list and transition = etat*char*etat
  type aef
  
  val trouver_char : delta*etat -> char
  val nouveau_etat : transition -> etat 
  val retourner_transition : delta*char -> transition
  val actualiser : transition*delta -> delta
  val concat : 'a list -> 'a list -> 'a list
  val random : int * q -> etat
  
  val est_correct : aef -> bool
  val est_complet : aef -> bool
  val completer : aef -> aef
  val langage_vide : aef -> bool
  val est_deterministe : aef -> bool
    
  val lecture_car : transition -> int -> char option
  
  val lecture_mot : delta*etat -> string
  val accepter_mot : string*aef -> bool 
    
  
  val union : aef -> aef -> aef
  val reco_union : aef -> aef -> aef
  val afficher : aef -> unit

end
