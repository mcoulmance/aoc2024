module IMap = Map.Make(Int)

let load_input path =
  let file = open_in path in

  let rec aux acc1 acc2 =
    match In_channel.input_line file with
      | None ->
          (List.sort Int.compare acc1), (List.sort Int.compare acc2)
      | Some line ->
          String.split_on_char ' ' line 
            |> List.filter_map int_of_string_opt 
            |> function
                  | [a; b] ->
                    aux (a::acc1) (b::acc2)
                  | _ ->
                    raise (Invalid_argument "ill formed file")
  in
  
  let res = aux [] [] in
  close_in file;

  res



let puzzle1 () =
  let (l1, l2) = load_input "assets/day1_input" in
  List.fold_left2 (fun acc a b -> acc + (abs (a - b))) 0 l1 l2


let puzzle2 () =
  let (l1, l2) = load_input "assets/day1_input" in
  
  let occ_l2 = 
    List.fold_left (fun acc a -> IMap.update a (function None -> Some 1 | Some i -> Some (i+1)) acc) IMap.empty l2 in

  List.map (fun a -> a * (Option.value (IMap.find_opt a occ_l2) ~default:0)) l1
    |> List.fold_left (+) 0
