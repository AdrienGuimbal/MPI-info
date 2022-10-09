let w0: char word = ['a'; 'b'; 'b'; 'a'; 'b'] in
let w1: bool word = [false; false; true; false] in
let w2: char word = [] in
let w3: char word = ['c'; 'a'; 'b'] in
let w4: char word = ['a'; 'b'] in
let w5: char word = ['b'; 'a'] in
let w6: char word = ['b'; 'b'; 'a'] in
let w7: char word = ['b'; 'b'; 'b'] in
let w8: char word = ['b'; 'b'; 'b'; 'a'] in
let w9: char word = ['a'; 'b'; 'a'; 'b'; 'a'; 'b'] in


let r0 = Union(Concat(Char 'a', Star(Char 'b')), Concat(Char 'b', Star(Char 'a'))) in
let r1 = Concat(Star(Concat(Char 'a', Char 'b')), Concat(Char 'a', Char 'b')) in
let r2 = Star(Char 'b') in

correct (match_regex w0 r0) (false) ~print:(Printf.printf "1. %B");
correct (match_regex w5 r0) (true) ~print:(Printf.printf "2. %B");
correct (match_regex w2 r0) (false) ~print:(Printf.printf "3. %B");

correct (match_regex w0 r1) (false) ~print:(Printf.printf "4. %B");
correct (match_regex w4 r1) (true) ~print:(Printf.printf "5. %B");
correct (match_regex w9 r1) (true) ~print:(Printf.printf "6. %B");
correct (match_regex w2 r1) (false) ~print:(Printf.printf "7. %B");

correct (match_regex w0 r2) (false) ~print:(Printf.printf "8. %B");
correct (match_regex w6 r2) (false) ~print:(Printf.printf "9. %B");
correct (match_regex w7 r2) (true) ~print:(Printf.printf "10. %B");
correct (match_regex w2 r2) (true) ~print:(Printf.printf "11. %B");
