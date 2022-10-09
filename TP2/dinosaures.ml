open Regex

(* construit une expression régulière représentant en fait l'alphabet entier *)
let rec construire_alphabet (current : char) (res : char exp_reg) =
  if current = 'Z' then
    construire_alphabet 'a' (Union(res,Symbole('Z')))
  else
    if current = 'z' then
      (Union(res,Symbole('z')))
    else
      let next = Char.chr (1 + (Char.code current)) in
      construire_alphabet next (Union(res,Symbole(current)))

let alphabet = construire_alphabet 'A' Vide

let list_of_dinosaurs = ["Velociraptor";
  "Cryolophosaurus";
  "Dilophosaurus";
  "Spinosaurus";
  "Suchomimus";
  "Baryonyx";
  "Irritator";
  "Utahraptor";
  "Sinotyrannus";
  "Chilantaisaurus";
  "Shaochilong";
  "Dilong";
  "Allosaurus";
  "Ceratosaurus";
  "Torvosaurus";
  "Sinraptor";
  "Acrocanthosaurus";
  "Tyrannotitan";
  "Carcharodontosaurus";
  "Mapusaurus";
  "Giganotosaurus";
  "Megaraptor";
  "Skorpiovenator";
  "Neovenator";
  "Afrovenator";
  "Carnotaurus";
  "Abelisaurus";
  "Coelophysis";
  "Megalosaurus";
  "Dromaeosaurus";
  "Achillobator";
  "Eocarcharia";
  "Kryptops";
  "Rugops";
  "Rajasaurus";
  "Spinosaurus";
  "Tyrannosaurus";
  "Tarbosaurus";
  "Daspletosaurus";
  "Gorgosaurus";
  "Albertosaurus";
  "Alioramus";
  "Appalachiosaurus";
  "Zhuchengtyrannus";
  "Dryptosaurus";
  "Alectrosaurus"
]

