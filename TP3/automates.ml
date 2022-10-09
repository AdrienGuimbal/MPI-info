type 'a mot = 'a list

type 'a automate = {
	nb : int ; (* nombre d'états : numérotés 0, 1, ..., nb-1 *)
	sigma : 'a array ;
	i : int list ;
	f : int list ;
	delta : (int * 'a, int list) Hashtbl.t
}

let ajouter_transition (a : 'a automate) (q1 : int) (lettre : 'a) (q2 : int) : unit =
  assert((q1 >= 0) && (q1 < a.nb));
  assert((q2 >= 0) && (q2 < a.nb));
  assert(Array.mem lettre a.sigma);
  match Hashtbl.find_opt a.delta (q1,lettre) with
  | None -> Hashtbl.add a.delta (q1,lettre) [q2]
  | Some l -> if not (List.mem q2 l) then
        Hashtbl.replace a.delta (q1,lettre) (q2::l)

let affiche_transitions (a : 'a automate) (print_lettre : 'a -> unit) : unit =
  for i = 0 to a.nb-1 do
    for j = 0 to (Array.length a.sigma)-1 do
      match Hashtbl.find_opt a.delta (i,a.sigma.(j)) with
      | None -> ()
      | Some l -> print_int i ;
            print_string "," ;
            print_lettre a.sigma.(j) ;
            print_string " -> " ;
            List.iter (fun x -> print_int x ; print_string " ") l ;
            print_newline ()
    done
  done

let affiche_automate (a : 'a automate) (print_lettre : 'a -> unit) : unit =
  Printf.printf "n : %d\n" a.nb ;
  print_string "I : ";
  List.iter (fun x -> print_int x ; print_string " ") a.i ;
  print_newline ();
  print_string "F : ";
  List.iter (fun x -> print_int x ; print_string " ") a.f ;
  print_newline ();
  affiche_transitions a print_lettre

let affiche_automate_char (a : 'a automate) : unit =
  affiche_automate a print_char


(* Exemple : automate a0 *)
let (a0 : char automate) = {
  nb = 6 ;
  sigma = [|'a';'b'|] ;
  i = [0] ;
  f = [1;4] ;
  delta = Hashtbl.create 36
}

let () =
  ajouter_transition a0 0 'a' 1 ;
  ajouter_transition a0 0 'b' 0 ;
  ajouter_transition a0 1 'a' 3 ;
  ajouter_transition a0 1 'b' 2 ;
  ajouter_transition a0 2 'a' 5 ;
  ajouter_transition a0 2 'b' 1 ;
  ajouter_transition a0 3 'a' 4 ;
  ajouter_transition a0 3 'b' 0 ;
  ajouter_transition a0 4 'a' 0 ;
  ajouter_transition a0 4 'b' 5 ;
  ajouter_transition a0 5 'a' 5 ;
  ajouter_transition a0 5 'b' 1
;;