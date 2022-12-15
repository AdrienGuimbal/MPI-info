type item = int * int (* value and weigth *)

let knapsack (items : item list) max_weight =
  let to_consider = Stack.create () in
  let best_value = ref 0 in
  let best_bag = ref [] in
  Stack.push ([], 0, 0, items) to_consider;
  
  while not (Stack.is_empty to_consider) do
    let (bag, value, weigth, remain) = Stack.pop to_consider in
    match remain with
    | [] -> if value > !best_value then (
        best_value := value;
        best_bag := bag;
      )
    | (v, w)::r -> begin
      if value >= !best_value then
        Stack.push (bag, value, weigth, r) to_consider;
      if value + v >= !best_value && weigth + w <= max_weight then
        Stack.push ((v,w)::bag, value + v, weigth + w, r) to_consider;
      end
  done;
  (!best_bag, !best_value)


let () =
  let items = [
    (92, 23);  (57, 31);
    (49, 29);  (68, 44);
    (60, 53);  (43, 38);
    (67, 63);  (84, 85);
    (87, 89);  (72, 82)]
  in (* expects 309$ *)
  let bag, value = knapsack items 165 in
  Printf.printf "tha bag conatains %i$\n" value;