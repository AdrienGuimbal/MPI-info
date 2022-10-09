type 'a mot = 'a list
type 'a automate = {
  nb : int;
  sigma : 'a array;
  i : int mot;
  f : int mot;
  delta : (int * 'a, int mot) Hashtbl.t;
}
val ajouter_transition : 'a automate -> int -> 'a -> int -> unit
val affiche_transitions : 'a automate -> ('a -> unit) -> unit
val affiche_automate : 'a automate -> ('a -> unit) -> unit
val affiche_automate_char : char automate -> unit
val a0 : char automate
