let test = [
  [7; 6; 4; 2; 1];
  [1; 2; 7; 8; 9];
  [9; 7; 6; 2; 1];
  [1; 3; 2; 4; 5];
  [8; 6; 4; 4; 1];
  [1; 3; 6; 7; 9]]

let input =
  let file = open_in "assets/day2_input" in

  let rec aux acc =
    match In_channel.input_line file with 
      | None ->
          acc

      | Some line ->
          let l = String.split_on_char ' ' line 
                  |> List.filter_map int_of_string_opt 
          in
          aux (l :: acc)
  in

  let res = aux [] in
  close_in file;
  res


let is_safe_without_dampener l = 
  let rec aux min max prev = function
    | hd :: tl ->
        let diff = hd - prev in
        diff >= min && diff <= max && aux min max hd tl

    | [] -> 
        true

  in
  match l with 
    | [] | [_] ->
        true

    | x :: ((y :: _) as tl) when x > y ->
        aux (-3) (-1) x tl

    | x :: ((y :: _) as tl) when x < y ->
        aux 1 3 x tl

    | _ -> 
        false


let is_safe_with_dampener l =
  let rec aux v min max prev = function
    | hd :: tl ->
        let diff = hd - prev in
        (diff >= min && diff <= max && aux v min max hd tl)
        || (v && aux false min max prev tl)

    | [] ->
        true
  in
  match l with
    | [] | [_] | [_; _] ->
        true
    
    | x :: ((y :: ((_z :: _) as tl2) as tl1)) ->
        let min, max = if x > y then (-3, -1) else (1, 3) in
        aux true min max x tl1 ||
        aux false min max x tl2 ||
        aux false min max y tl2

let puzzle1 =
  input 
    |> List.fold_left (fun acc x -> if is_safe_without_dampener x then acc+1 else acc) 0

let puzzle2 = 
  input
    |> List.fold_left (fun acc x -> if is_safe_with_dampener x then acc+1 else acc) 0
