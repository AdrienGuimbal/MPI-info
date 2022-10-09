(* author : Adrien GUIMBAL *)

(**************)
(*    MOTS    *)
(**************)

(* 1 *)
type 'a mot = 'a list ;;

(* 2 *)
(* c'est une type générique, il permet de ne pas avoir a réecrire plusieurs fois les même fonctions
   pour travailler sur des structures contenant des elements inchangées.
   *)

(* 3 *)
let longeur = List.length ;;
let est_vide = (=) [] ;;

(* 4 *)
let miroir = List.rev ;;
(* c'est un mot de 'char'. La complexité est en O(longeur du mot) *)

(* 5 *)
let concat = (@) ;;
(* la complexité est en O(longeur du premeir mot) *)


(***************)
(*  SOUS-MOTS  *)
(***************)

(* 6 *)
let rec est_prefixe wordA wordB =
    if longeur wordA > longeur wordB then false else

    match wordA, wordB with
    | [] , _ -> true
    | _ , [] -> false
    | a::tA , b::tB -> a=b && est_prefixe tA tB
;;

(* 7 *)
let est_suffixe wordA wordB =
    est_prefixe (miroir wordA) (miroir wordB)
;;

(* 8 *)
let rec est_facteur wa wb =
    if longeur wa > longeur wb then false else

    match wb with
    | [] -> wa = []
    | b::tb -> est_prefixe wa wb || est_facteur wa tb
;;

(* 9 *)
let rec est_sous_mot wa wb =
    if longeur wa > longeur wb then false else

    match wa, wb with
    | [], _ -> true
    | _, [] -> false
    | a::ta, b::tb -> (a=b && est_sous_mot ta tb)
                       || est_sous_mot wa tb
;;

(* 10 *)
let coupe word len =
    let rec _coupe word pref suff i =
        match word with
        | [] -> if i<len 
            then failwith "Word is not long enougth"
            else (miroir pref, miroir suff)
        | h::t -> if i<len
            then _coupe t (h::pref) suff (i+1)
            else _coupe t pref (h::suff) (i+1)
    in
    _coupe word [] [] 0
;;
(* la complexité de la fonction est en O(n) *)

(* 11 *)
let rev_prefixe word len =
    let rec _rev_pref word i acc =
        if i = len then acc else
        match word with
        | [] -> failwith "Word is not long enougth"
        | h::t -> _rev_pref t (i+1) (h::acc)
    in
    _rev_pref word 0 []
;;

let prefixe word len = miroir (rev_prefixe word len) ;;
let suffixe word len = rev_prefixe (miroir word) len ;;


(***************)
(*    REGEX    *)
(***************)

(* 12 *)
type 'a regex = 
      Vide 
    | Epsilon
    | Symbole of 'a
    | Union of 'a regex * 'a regex
    | Concat of 'a regex * 'a regex
    | Etoile of 'a regex
;;

(* 13 *)
let e = Union (
    Concat (
        Symbole 'a',
        Etoile (Symbole 'b')
    ),
    Concat (
        Symbole 'b',
        Etoile (Symbole 'a')
    )
);;

let e = Union (
    Concat (
        Symbole 1,
        Etoile (Symbole 0)
    ),
    Symbole 0
);;

(* 14 *)
let rec test word expression =
    let rec for_all_cuts exp1 exp2 cut_index word_length =
        if cut_index > word_length then false else
        let p,s = coupe word cut_index in
        test p exp1 && test s exp2
    in
    match expression with
    | Vide -> false
    | Epsilon -> word = []
    | Symbole e -> word = [e]
    | Union (e,e') -> test word e || test word e'
    | Concat (e,e') -> for_all_cuts e e' 0 (longeur word)
    | Etoile e -> (test word Epsilon) 
                || for_all_cuts e (Etoile e) 1 (longeur word)
;;

(**************)
(* DINOSAURES *)
(**************)
(* 16 *)
let string_vers_mot str =
    let rec svm i acc =
        if i < 0 then acc
        else svm (i-1) (str.[i] :: acc)
    in
    svm (String.length str - 1) []
;;
(* 17 *)
let rec mot_vers_expression word =
    match word with
    | [] -> Epsilon
    | [a] -> Symbole a
    | a::t -> Concat(Symbole a, mot_vers_expression t)


(**************)
(*    TEST    *)
(**************)
type nucleotides =  A | C | G | T ;;
let mot1 = [0;1;2;1;3;1;5;1;5;1;0]
and mot2 = ['a';'b';'c';'d']
and mot2' = ['e';'f']
and mot3 = [C;G;T;T;G;A;G;G;T;A;G;A;A;T;T;C;G;A;A;T];;
    
let () = 
    assert(longeur mot1 = 11);
    assert(longeur mot2 = 4);
    print_endline "[\x1b[32mOK\x1b[0m]    longeur";

    assert(miroir mot2 = ['d';'c';'b';'a']);
    print_endline "[\x1b[32mOK\x1b[0m]    miroir";

    assert(concat mot2 mot2' = ['a';'b';'c';'d';'e';'f']);
    print_endline "[\x1b[32mOK\x1b[0m]    concat";

    assert(est_prefixe [C;G;T] mot3);
    assert(not (est_prefixe [C;T;T] mot3));
    print_endline "[\x1b[32mOK\x1b[0m]    est_prefixe";

    assert(est_facteur ['b';'c'] mot2);
    assert(not(est_facteur ['b';'c'] mot2'));
    print_endline "[\x1b[32mOK\x1b[0m]    est_facteur";

    assert(est_sous_mot [1;2;1;5;5] mot1);
    assert(not(est_sous_mot [5;5;5] mot1));
    print_endline "[\x1b[32mOK\x1b[0m]    est_sous_mot";

    assert(
        let a,b = coupe mot1 5 in
        est_prefixe a mot1 && 
        est_suffixe b mot1 
    );
    assert(
        let a,b = coupe mot2' 2 in
        est_prefixe a mot2' && 
        est_suffixe b mot2' 
    );
    print_endline "[\x1b[32mOK\x1b[0m]    coupe";

    assert(
        let a = prefixe mot3 5 in
        est_prefixe a mot3 &&
        longeur a = 5
    );
    print_endline "[\x1b[32mOK\x1b[0m]    prefixe";

    assert(
        let a = suffixe mot1 4 in
        est_suffixe a mot1 &&
        longeur a = 4
    );
    print_endline "[\x1b[32mOK\x1b[0m]    suffixe";
;;
