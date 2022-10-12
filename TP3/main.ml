(* Author : Adrien GUIMBAL *)
(* quelques fonction non demandés on été ajoutés (notés par Bonus)                  *)
(* elles n'ont honetement pas grand chose a faire là mais c'est un bon entrainement *)
(* donc pas besoin de les coriger (surtout qu'elles ne sont pas aussi commentés     *)
(* que le reste et que l'implementation est parfois un peu obscure)                 *)


open Automates

let (%) = (mod) ;;

(*********************)
(*   Mise en Place   *)
(*********************)

(* renvoie si l'automate est deterministe *)
let est_deterministe auto =
  if auto.i = [] then true else (* Si il n'y as pas d'état d'entrée, l'automate est deterministe*)

  let almost_empty = function (* renvoie true si la liste contient un ou aucun élélment*)
    | [] | [_] -> true
    | _        -> false
  in

  (* il faut un seul état d'entrée *)
  almost_empty auto.i &&
  (* et que tout couple etat,lettre ne soit lié à au plus un état*)
  Hashtbl.fold (fun _ neib acc ->
    acc && almost_empty neib
  ) auto.delta true

(* construction de a1 *)
let (a1 : char automate) = {
  nb = 4 ;
  sigma = [|'a';'b'|] ;
  i = [0;1] ;
  f = [2] ;
  delta = Hashtbl.create 36
}

let () =
  ajouter_transition a1 0 'b' 1 ;
  ajouter_transition a1 0 'b' 2 ;

  ajouter_transition a1 1 'a' 2 ;
  ajouter_transition a1 1 'b' 3 ;

  ajouter_transition a1 2 'a' 0 ;
  ajouter_transition a1 2 'b' 2 ;
  ajouter_transition a1 2 'b' 3 ;

  ajouter_transition a1 3 'b' 1 ;
  ajouter_transition a1 3 'b' 2 ;


(***************************)
(*   Parties de [0, N-1]   *)
(***************************)
module Partie = struct
  type t = bool array

  let appartient i arr =
    if i < 0 || i >= Array.length arr then
      false
    else 
      arr.(i)

  let vide n = Array.make n false
      
  let rec intersection arr1 arr2 =
    assert (Array.length arr2 = Array.length arr1);
    Array.map2 (&&) arr1 arr2


  let ajoute arr1 arr2 =
    assert (Array.length arr2 = Array.length arr1);
    Array.map2 (||) arr1 arr2

  let construit lst n =
    let arr = Array.make n false in
    List.iter ( fun i -> arr.(i) <- true ) lst;
    arr
    
  (* Bonus *)
  
  (* test si tout les lements de la partie sont false *)
  let est_vide arr =
    not (Array.fold_left (||) false arr)
    (* merci De Morgan *)

  let fold_left func init part =
    let rec _fold index acc =
      if index = Array.length part then acc else
      if part.(index)
        then _fold (index+1) (func acc index)
        else _fold (index+1) (acc)
    in
    _fold 0 init

  let substract arr1 arr2 =
    assert (Array.length arr2 = Array.length arr1);
    Array.map2 (fun a b -> a && not b) arr1 arr2

  let ajoute_entier i arr =
    assert (i >= 0 && i < Array.length arr);
    Array.mapi (fun j a -> j=i || a) arr

  let non_disjointes arr1 arr2 =
    intersection arr1 arr2
    |> est_vide
    |> not
  
  let print arr =
    print_char '{';
    print_char ' ';
    for i=0 to Array.length arr - 2 do
      if arr.(i) then print_int i else print_char '_';
      print_char ' ';
    done;
    print_char '}';
end

(************************)
(*   Lecture d'un mot   *)
(************************)

let lire_lettre_etat auto state letter =
  match Hashtbl.find_opt auto.delta (state, letter) with
  | None -> Partie.vide auto.nb
  | Some lst -> Partie.construit lst auto.nb


let lire_lettre_partie auto states letter =
  Partie.fold_left
    (fun acc q -> Partie.ajoute acc (lire_lettre_etat auto q letter))
    (Partie.vide auto.nb)
    states


let lire_mot auto start word =
  List.fold_left (lire_lettre_partie auto) start word

let accepte_mot auto word =
  let swap_args f a b c = f a c b in

  Partie.construit auto.i auto.nb      (* états initiaux *)
  |> swap_args lire_mot auto word      (* lecture du mot *)
  |> Partie.non_disjointes (Partie.construit auto.f auto.nb) (* verification qu'il y ai des états acceptants *)

(*********************)
(*  Determinisation  *)
(*********************)

let vers_entier =
  Array.fold_left (fun acc x -> 2*acc + if x then 1 else 0) 0

let vers_partie number size =
  let a = Partie.vide size in

  let rec fill n i =
    if i < 0 then a else (
      a.(i) <- (n%2 = 1);
      fill (n/2) (i-1)
    )
  in
  fill number (size-1)


let puissance a n = (* calcule de puissance avec exponentiation rapide *)
  let rec _pow a n acc =
    match n with
    | 0 -> acc
    | n -> _pow (a*a) (n/2) (if n%2 = 0 then acc else a*acc)
  in
  _pow a n 1


let determinise auto =
  let n, m = auto.nb, Array.length auto.sigma in
  let part lst = Partie.construit lst n in (* alais pour rendre le code plus concis *)
  let delta' = Hashtbl.create (auto.nb * auto.nb) in (* nouveaux arcs *)

  let autof = Partie.construit auto.f n in
  let final = Partie.non_disjointes autof in (* renvoie un super etat est final *)

  let i' = part auto.i in

  let visited = Array.make (puissance 2 n) false in
  visited.(vers_entier i') <- true;

  let rec process s_state j remaining accept = (* s_sate : super état traité, j : index de la lettre etudiée *)
    match j=m, remaining with
    | true, []  -> { (* terminaison *)
        nb = puissance 2 n ;
        sigma = auto.sigma ;
        i = [vers_entier i'] ;
        f = accept ;
        delta = delta'
      }
    | true, new_state::remaining' -> (* toutes les lettres de l'état on étés étudiés, passer a la suivante *)
      if final s_state then 
        process new_state 0 remaining' ((vers_entier s_state)::accept)
      else
        process new_state 0 remaining' accept
    | false, _ -> ( (* etude d'une lettre *)
      let s' = lire_lettre_partie auto s_state auto.sigma.(j) in 
      Hashtbl.add delta' (vers_entier s_state, auto.sigma.(j)) [vers_entier s'];
      if visited.(vers_entier s') then
        process s_state (j+1) remaining accept
      else (
        visited.(vers_entier s') <- true;
        process s_state (j+1) (s'::remaining) accept
      )
    )
  in
  process i' 0 [] []

(* bonus *)
let emonde auto =
  let n = auto.nb in
  
  let accesibles =
    let accesibles_from states = 
      Array.fold_left ( fun acc letter ->
        letter
        |> lire_lettre_partie auto states
        |> Partie.ajoute acc
      ) (Partie.vide n) auto.sigma
    in

    let rec visit to_visit acc =
      if Partie.est_vide to_visit then acc else
      let _new = accesibles_from to_visit in
      visit (Partie.substract _new acc) (Partie.ajoute to_visit acc)
    in

    visit (Partie.construit auto.i n) (Partie.vide n)
  in
  
  let co_accesibles =
    let co_accesibles_from states =
      Hashtbl.fold ( fun (q,_) q' acc ->
        if Partie.non_disjointes (Partie.construit q' n) acc then
          Partie.ajoute_entier q acc
        else
          acc
      ) auto.delta (Partie.construit auto.f n)
    in

    let rec visit to_visit acc =
      if Partie.est_vide to_visit then acc else
      let _new = co_accesibles_from to_visit in
      visit (Partie.substract _new acc) (Partie.ajoute to_visit acc)
    in

    visit (Partie.construit auto.f n) (Partie.vide n)
  in

  let utiles = Partie.intersection accesibles co_accesibles in
  let n' = Array.fold_left (fun acc x -> acc + if x then 1 else 0) 0 utiles in

  let corresp = Array.make n (-1) in (* tableau des correspondaces des anciens états dans le nouvel automate *)
  let _ = Partie.fold_left (fun i' i -> (corresp.(i) <- i'; i' +1)) 0 utiles in

  let convert states = states
    |> List.filter (fun q -> Partie.appartient q utiles) 
    |> List.map (Array.get corresp)
  in

  let i' = convert auto.i
  and f' = convert auto.f
  and delta' = Hashtbl.create (n'*n') in

  Hashtbl.iter ( fun (q, y) ps ->
    if Partie.appartient q utiles then
      let ps' = convert ps in
      if ps' <> [] then
        Hashtbl.add delta' (corresp.(q), y) ps'
  ) auto.delta;

  {
    nb = n' ;
    sigma = auto.sigma ;
    i = i';
    f = f' ;
    delta = delta'
  }

(*************)
(*   Tests   *)
(*************)

let pretty_test res output message =

  if res = output then
    print_endline @@ "\x1b[1;32m [PASSED] \x1b[0m\t" ^ message
  else
    print_endline @@ "\x1b[1;31m [FAILED] \x1b[0m\t" ^ message


let () =
  print_endline "==== Mise en place ====";
  pretty_test (est_deterministe a0) true  "a0 est deterministe";
  pretty_test (est_deterministe a1) false "a1 n'est pas deterministe";

  print_endline "==== Parties de [0; N-] ====";
  (* pour representer des parties de [0;N-1] facilement *)
  let (#|) size number =
    let a = Array.make size false in

    let rec fill n i =
      if i < 0 then a
      else (
        a.(i) <- (n%10 = 1);
        fill (n/10) (i-1)
      )
    in
    fill number (size-1)
  in

  pretty_test (Partie.appartient   5  8#|11111111) true  " 5 est dans {0,1,2,3,4,5,6,7}";
  pretty_test (Partie.appartient   4  5#|00001   ) true " 4 est dans {_,_,_,_,4}";
  pretty_test (Partie.appartient   3  5#|00000   ) false " 3 n'est pas dans {_,_,_,_,_}";
  pretty_test (Partie.appartient   2  5#|11011   ) false " 2 n'est pas dans {0,1,_,3,4}";
  pretty_test (Partie.appartient   5  5#|11111   ) false " 5 n'est pas dans {0,1,2,3,4}";
  pretty_test (Partie.appartient (-1) 5#|11111   ) false "-1 n'est pas dans {0,1,2,3,4}";
  pretty_test (Partie.appartient   9  5#|11111   ) false " 9 n'est pas dans {0,1,2,3,4}";
  pretty_test (Partie.appartient   2  0#|0       ) false " 0 n'est pas dans {}";
  print_newline ();
  pretty_test (Partie.ajoute 4#|1111  4#|1111 ) 4#|1111   "{0,1,2,3}   u {0,1,2,3}   = {0,1,2,3} ";
  pretty_test (Partie.ajoute 5#|00111 5#|10101) 5#|10111  "{_,_,2,3,4} u {0,_,2,_,4} = {0,_,2,3,4} ";
  pretty_test (Partie.ajoute 1#|0     1#|0    ) 1#|0      "{_} u {_} = {_}";
  print_newline ();
  pretty_test (Partie.intersection 4#|1111  4#|1111 ) 4#|1111   "{0,1,2,3}   n {0,1,2,3}   = {0,1,2,3} ";
  pretty_test (Partie.intersection 5#|00111 5#|10101) 5#|00101  "{_,_,2,3,4} n {0,_,2,_,4} = {_,_,2,_,4} ";
  pretty_test (Partie.intersection 1#|0     1#|1    ) 1#|0      "{_} u {0} = {_}";
  

  print_endline "==== Determinise ====";
  let a2 = determinise a1 in
  affiche_automate a2 print_char;
  pretty_test (est_deterministe a2) true "a1 a été correctement déterminisé";

  print_endline "==== Emonde ====";
  let a3 = emonde a2 in
  affiche_automate a3 print_char;



