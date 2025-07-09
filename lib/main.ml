open! Core
open! Async
open! Game_strategies_common_lib

let generate_squares (game : Game.t) =
  let length = Game_kind.board_length game.game_kind in

  (* let length = sqrt_length * (sqrt_length) in *)
  let rows = List.init length ~f:(fun x -> x) in
  let cols = List.init length ~f:(fun x -> x) in
  let position_coordinates = List.cartesian_product rows cols in
  List.map position_coordinates ~f:(fun (x, y) ->
      { Position.row = x; column = y })

(* This is a helper function for constructing games from a list of positions *)
let init_game (board : (Position.t * Piece.t) list) : Game.t =
  { (Game.empty Tic_tac_toe) with board = Position.Map.of_alist_exn board }

let win_for_x =
  init_game
    [
      ({ row = 0; column = 0 }, X);
      ({ row = 1; column = 0 }, O);
      ({ row = 2; column = 2 }, X);
      ({ row = 2; column = 0 }, O);
      ({ row = 2; column = 1 }, X);
      ({ row = 1; column = 1 }, O);
      ({ row = 0; column = 2 }, X);
      ({ row = 0; column = 1 }, O);
      ({ row = 1; column = 2 }, X);
    ]

let non_win =
  init_game
    [
      ({ row = 0; column = 0 }, X);
      ({ row = 1; column = 0 }, O);
      ({ row = 2; column = 2 }, X);
      ({ row = 2; column = 0 }, O);
    ]

let print_game (game : Game.t) =
  (* let num_squares = match game.game_kind with Omok -> 225 | _ -> 9 in *)
  let board_length = Game_kind.board_length game.game_kind in

  (* let game_list =
    List.init length ~f:(fun num ->
        let r = num / board_length in
        let c = num % board_length in
        let pos : Position.t = { row = r; column = c } in
        let some_player = Map.find game.board pos in
        match some_player with
        | Some some_player -> Piece.to_string some_player
        | None -> " ")
  in *)
  (* let position_coordinates =
    List.cartesian_product
      (List.init board_length ~f:(fun x -> x))
      (List.init board_length ~f:(fun x -> x))
  in *)
  (* let piece_strings =
    List.map position_coordinates ~f:(fun (r, c) ->
        let pos : Position.t = { row = r; column = c } in
        let some_player = Map.find game.board pos in
        match some_player with
        | Some some_player -> Piece.to_string some_player
        | None -> " ")
  in *)

  (* ignore game; *)
  (* 
  if length = 225 then
    List.iteri piece_strings ~f:(fun ind x ->
        if not ((ind + 1) % board_length = 0) then print_string (x ^ " |")
        else if ind + 1 = 225 then print_string x
        else
          print_string (x ^ "\n---------------------------------------------\n"))
  else *)

  (* List.iteri piece_strings ~f:(fun ind x ->
        if not ((ind + 1) % board_length = 0) then print_string (x ^ " | ")
        else if ind + 1 = num_squares then print_string x
        else
           print_string (x ^ "\n---------\n")) *)
  let rows = List.init board_length ~f:(fun x -> x) in
  let cols = List.init board_length ~f:(fun x -> x) in
  List.iter rows ~f:(fun x ->
      List.iter cols ~f:(fun y ->
          let piece =
            match Map.find game.board { Position.row = x; column = y } with
            | Some some_player -> Piece.to_string some_player
            | None -> " "
          in

          if not (y + 1 = board_length) then print_string (piece ^ " | ")
          else print_string piece);

      if not (x + 1 = board_length) then
        print_string
          ("\n" ^ String.init (board_length * 3) ~f:(fun _ -> '-') ^ "\n"))

let%expect_test "print_win_for_x" =
  print_game win_for_x;
  [%expect
    {|
      X | O | X
      ---------
      O | O | X
      ---------
      O | X | X
      |}];
  return ()

let%expect_test "print_non_win" =
  print_game non_win;
  [%expect
    {|
      X |   |
      ---------
      O |   |
      ---------
      O |   | X
      |}];
  return ()

(* Exercise 1 *)
let available_moves (game : Game.t) : Position.t list =
  (* ignore game; *)
  (* let sqrt_length = Game_kind.board_length game.game_kind in

  (* let length = sqrt_length * (sqrt_length) in *)
  let rows = List.init sqrt_length ~f:(fun x -> x) in
  let cols = List.init sqrt_length ~f:(fun x -> x) in
  let position_coordinates = List.cartesian_product rows cols in *)
  let postions = generate_squares game in

  (* List.iter rows ~f:(fun x ->
    
    List.iter cols ~f:(fun y -> )
    
    
    
    ) *)
  (* let vals =
    List.init length ~f:(fun x ->
        let r = x / sqrt_length in
        let c = x % sqrt_length in
        let input : Position.t = { row = r; column = c } in
        input)
  in *)
  List.filter postions ~f:(fun x ->
      match Map.find game.board x with Some _ -> false | None -> true)
(* failwith "Implement me!" *)

let%expect_test "available_moves" =
  let available_moves = available_moves win_for_x in
  let () =
    match List.is_empty available_moves with
    | true -> ()
    | false -> print_string (string_of_int (List.length available_moves))
  in
  return ()

(* Exercise 2 *)
let evaluate (game : Game.t) : Evaluation.t =
  (* let length = match game.game_kind with Omok -> 225 | _ -> 9 in
   *)
  let length = match game.game_kind with Omok -> 15 | _ -> 3 in
  let to_win = Game_kind.win_length game.game_kind in

  let squares = generate_squares game in

  let output : Evaluation.t ref = ref Evaluation.Game_continues in

  List.iter squares ~f:(fun square ->
      match Map.find game.board square with
      | Some current_piece ->
          let my_filter position_list =
            List.for_all position_list ~f:(fun pos ->
                match Map.find game.board pos with
                | Some piece -> Piece.equal piece current_piece
                | None -> false)
          in

          let horiz =
            List.init to_win ~f:(fun x ->
                { Position.row = square.row; column = square.column + x })
          in

          let is_horiz_win = my_filter horiz in

          let vert =
            List.init to_win ~f:(fun x ->
                { Position.row = square.row + x; column = square.column })
          in

          let is_vert_win = my_filter vert in

          let diag1 =
            List.init to_win ~f:(fun x ->
                { Position.row = square.row - x; column = square.column + x })
          in

          let is_diag1_win = my_filter diag1 in

          let diag2 =
            List.init to_win ~f:(fun x ->
                { Position.row = square.row + x; column = square.column + x })
          in

          let is_diag2_win = my_filter diag2 in

          if is_horiz_win || is_vert_win || is_diag1_win || is_diag2_win then
            if
              String.equal
                (Piece.to_string (Map.find_exn game.board square))
                "X"
            then output := Game_over { winner = Some X }
            else output := Game_over { winner = Some O }
          else ()
      | None -> ());

  (match !output with
  | Game_continues ->
      if List.length (available_moves game) = 0 then
        output := Game_over { winner = None }
      else ()
  | _ -> ());
  Map.iter_keys game.board ~f:(fun x ->
      if x.row < 0 || x.row >= length || x.column < 0 || x.column >= length then
        output := Illegal_move
      else ());
  !output

(* ignore game; *)
(* failwith "Implement me!" *)

let%expect_test "evaluate1" =
  let x = evaluate win_for_x in
  let _ =
    match x with
    | Game_over { winner = Some X } -> print_string "Oh yeah!"
    | _ -> print_s (Evaluation.sexp_of_t x)
  in
  return ()

(* let%expect_test "evaluate2" =
  let x = available_moves win_for_x in
  let _ =
    match List.length x with
    | 0 -> ()
    | _ -> print_string (x)
  in
  return () *)

(* Exercise 3 *)
let winning_moves ~(me : Piece.t) (game : Game.t) : Position.t list =
  (* ignore me;
  ignore game; *)
  let moves_available = available_moves game in
  List.filter moves_available ~f:(fun move ->
      let temp_board = Map.add_exn game.board ~key:move ~data:me in
      let temp_game = { game with Game.board = temp_board } in

      match evaluate temp_game with
      | Game_over { winner = Some winning_piece } ->
          Piece.equal winning_piece me
      | _ -> false)
(* failwith "Implement me!" *)

let%expect_test "winningmoves1" =
  let moves_to_win = winning_moves ~me:X non_win in
  print_s [%message (moves_to_win : Position.t list)];
  return ()

(* Exercise 4 *)
let losing_moves ~(me : Piece.t) (game : Game.t) : Position.t list =
  let opp = match me with X -> Piece.O | _ -> Piece.X in
  let moves_available = available_moves game in
  List.filter moves_available ~f:(fun move ->
      let temp_board = Map.add_exn game.board ~key:move ~data:opp in
      let temp_game = { game with Game.board = temp_board } in

      match evaluate temp_game with
      | Game_over { winner = Some winning_piece } ->
          Piece.equal winning_piece opp
      | _ -> false)

let%expect_test "winningmoves1" =
  let moves_to_win = losing_moves ~me:O non_win in
  print_s [%message (moves_to_win : Position.t list)];
  return ()

let exercise_one =
  Command.async ~summary:"Exercise 1: Where can I move?"
    (let%map_open.Command () = return () in
     fun () ->
       let moves = available_moves win_for_x in
       print_s [%sexp (moves : Position.t list)];
       let moves = available_moves non_win in
       print_s [%sexp (moves : Position.t list)];
       return ())

let exercise_two =
  Command.async ~summary:"Exercise 2: Is the game over?"
    (let%map_open.Command () = return () in
     fun () ->
       let evaluation = evaluate win_for_x in
       print_s [%sexp (evaluation : Evaluation.t)];
       let evaluation = evaluate win_for_x in
       print_s [%sexp (evaluation : Evaluation.t)];
       return ())

let piece_flag =
  let open Command.Param in
  flag "piece"
    (required (Arg_type.create Piece.of_string))
    ~doc:
      ("PIECE "
      ^ (Piece.all |> List.map ~f:Piece.to_string |> String.concat ~sep:", "))

let exercise_three =
  Command.async ~summary:"Exercise 3: Is there a winning move?"
    (let%map_open.Command () = return () and piece = piece_flag in
     fun () ->
       let winning_moves = winning_moves ~me:piece non_win in
       print_s [%sexp (winning_moves : Position.t list)];
       return ())

let exercise_four =
  Command.async ~summary:"Exercise 4: Is there a losing move?"
    (let%map_open.Command () = return () and piece = piece_flag in
     fun () ->
       let losing_moves = losing_moves ~me:piece non_win in
       print_s [%sexp (losing_moves : Position.t list)];
       return ())

let command =
  Command.group ~summary:"Exercises"
    [
      ("one", exercise_one);
      ("two", exercise_two);
      ("three", exercise_three);
      ("four", exercise_four);
    ]

(* Exercise 5 *)
let make_move ~(game : Game.t) ~(you_play : Piece.t) : Position.t =
  let total_moves = available_moves game in
  let first_choice = winning_moves ~me:you_play game in
  let second_choice = losing_moves ~me:you_play game in
  if not (List.is_empty first_choice) then List.random_element_exn first_choice
  else if not (List.is_empty second_choice) then
    List.random_element_exn second_choice
  else if not (List.is_empty total_moves) then
    List.random_element_exn total_moves
  else failwith "No more available moves"

(* ignore game;
  ignore you_play;
  failwith "Implement me!" *)
(* 
Exerdcise  *)
let available_moves_that_do_not_immediately_lose ~(me : Piece.t) (game : Game.t)
    =
  let total_moves = available_moves game in
  let opp = Piece.flip me in
  List.filter total_moves ~f:(fun move ->
      let temp_board = Map.add_exn game.board ~key:move ~data:me in
      let temp_game = { game with Game.board = temp_board } in

      let winning_moves_for_opp = winning_moves temp_game ~me:opp
      match (List.length winning_moves_for_opp)  with
      | 0 -> True
      | _ -> false)
