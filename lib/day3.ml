type state = M | U | L | RPar | Digit of int | Comma of int * int | Skip 
            | D | O | N | T | App
            | LPar1 (* mul( *)
            | LPar2 (* do( *)
            | LPar3 (* don't( *)


let is_digit = function
  | '0'..'9' -> true
  | _        -> false


let make_digit acc i =
  (acc * 10) + ((Char.code i) - (Char.code '0'))


let eval1 s =
  let aux (state, acc) c =
    match state with 
      | Skip when c = 'm'             -> (M, acc)
      | M when c = 'u'                -> (U, acc)
      | U when c = 'l'                -> (L, acc)
      | L when c = '('                -> (LPar1, acc) 
      | LPar1 when is_digit c         -> (Digit (make_digit 0 c), acc)
      | Digit i when is_digit c       -> (Digit (make_digit i c), acc)
      | Digit i when c = ','          -> (Comma (i, 0), acc)
      | Comma (i, j) when is_digit c  -> (Comma (i, make_digit j c), acc)
      | Comma (i, j) when c = ')'     -> (Skip, acc + (i * j))
      | _                             -> (Skip, acc)
  in
  String.fold_left aux (Skip, 0) s |> snd


let eval2 s = 
  let aux (enabled, state, acc) c =
    if enabled then
      match state with 
        | Skip when c = 'm'             -> (true, M, acc)
        | M when c = 'u'                -> (true, U, acc)
        | U when c = 'l'                -> (true, L, acc)
        | L when c = '('                -> (true, LPar1, acc) 
        | LPar1 when is_digit c         -> (true, Digit (make_digit 0 c), acc)
        | Digit i when is_digit c       -> (true, Digit (make_digit i c), acc)
        | Digit i when c = ','          -> (true, Comma (i, 0), acc)
        | Comma (i, j) when is_digit c  -> (true, Comma (i, make_digit j c), acc)
        | Comma (i, j) when c = ')'     -> (true, Skip, acc + (i * j))
        | Skip when c = 'd'             -> (true, D, acc)
        | D when c = 'o'                -> (true, O, acc) 
        | O when c = 'n'                -> (true, N, acc)
        | O when c = '('                -> (true, LPar2, acc)
        | LPar2 when c = ')'            -> (true, Skip, acc)
        | N when c = '\''               -> (true, App, acc)
        | App when c = 't'              -> (true, T, acc)
        | T when c = '('                -> (true, LPar3, acc)
        | LPar3 when c = ')'            -> (false, Skip, acc) 
        | _                             -> (true, Skip, acc)
    else
      match state with 
        | Skip when c = 'd'             -> (false, D, acc)
        | D when c = 'o'                -> (false, O, acc)
        | O when c = '('                -> (false, LPar2, acc)
        | LPar2 when c = ')'            -> (true, Skip, acc)
        | _                             -> (false, Skip, acc)
  in
  let (_, _, res) = String.fold_left aux (true, Skip, 0) s in
  res

        


let () = 
  let t = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))" in
  assert (eval1 t = 161);

  let t' = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))" in
  print_int (eval2 t');
  assert (eval2 t' = 48)


let input = open_in "assets/day3_input" |> In_channel.input_all 

let puzzle1 = eval1 input
let puzzle2 = eval2 input
