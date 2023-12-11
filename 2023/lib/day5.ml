open! Imports
open Base

module M = struct
  (* Type to parse the input into *)
  type t = string * char Stack.t list

  (** [D] [N] [C] [Z] [M] [P] 1 2 3 In this example, there are three stacks
      of crates. Stack 1 contains two crates: crate Z is on the bottom, and
      crate N is on top. Stack 2 contains three crates; from bottom to top,
      they are crates M, C, and D. Finally, stack 3 contains a single crate,
      P. *)
  let string_to_list_stacks str =
    let lines = String.split_lines str in
    let last_line = List.last_exn lines in
    (* stack amount = elements that are != " " *)
    let amount_of_stacks =
      String.count last_line ~f:(fun x -> Char.(x <> ' '))
    in
    let stacks : char Stack.t list =
      List.init amount_of_stacks ~f:(fun _ -> Stack.create ())
    in
    let without_last_line =
      List.take lines (List.length lines - 1) |> List.rev
    in
    let donger =
      List.map
        ~f:(fun line ->
          let line = String.to_list line in
          let chunks = List.chunks_of ~length:4 line in
          List.map ~f:(fun chunk -> List.nth_exn chunk 1) chunks )
        without_last_line
    in
    List.fold_left donger ~init:stacks ~f:(fun acc curr_line ->
        List.mapi acc ~f:(fun index stack ->
            match List.nth curr_line index with
            (* check for some x when x != empty char *)
            | Some x when Char.(x <> ' ') -> Stack.push stack x ; stack
            | _ -> stack ) )

  (* Convert the parsed input into the appropriate type *)
  let tuple_to_output a = string_to_list_stacks a

  (* print top of stack list as string *)
  let print_stack_list stacks =
    List.iter stacks ~f:(fun stack ->
        Stdio.printf "%c" (Stack.top_exn stack) ) ;
    Stdio.printf "\n"

  (* Read the input into a tuple of strings *)
  let parse inputs =
    let a = Str.split (Str.regexp "\n\n") inputs in
    let output =
      match a with
      | [a; b] -> (b, tuple_to_output a)
      | _ -> failwith "bad input"
    in
    output

  (* Move amount from stackFrom to stackTo *)
  let stack_move stackFrom stackTo amount =
    let rec move stackFrom stackTo amount =
      if amount = 0 then ()
      else (
        Stack.push stackTo (Stack.pop_exn stackFrom) ;
        move stackFrom stackTo (amount - 1) )
    in
    move stackFrom stackTo amount

  let stack_move_combined stackFrom stackTo amount =
    let stack' = Stack.create () in
    match Stack.length stackFrom < amount with
    | true -> failwith "not enough elements"
    | false ->
        let rec move stackFrom stack amount =
          if amount = 0 then ()
          else (
            Stack.push stack (Stack.pop_exn stackFrom) ;
            move stackFrom stack (amount - 1) )
        in
        move stackFrom stack' amount ;
        Stack.iter stack' ~f:(fun x -> Stack.push stackTo x)

  (* Comand: move 5 from 3 to 6 *)
  let parse_command command stacks mover =
    let open Core in
    let command = String.split command ~on:' ' in
    let amount = Int.of_string (List.nth_exn command 1) in
    let from = Int.of_string (List.nth_exn command 3) in
    let to_ = Int.of_string (List.nth_exn command 5) in
    let stackFrom = List.nth_exn stacks (from - 1) in
    let stackTo = List.nth_exn stacks (to_ - 1) in
    mover stackFrom stackTo amount

  (* Run part 1 with parsed inputs *)
  let part1 (data : t) =
    let commands, stacks = data in
    let commands = String.split_lines commands in
    List.iter commands ~f:(fun command ->
        parse_command command stacks stack_move ) ;
    print_stack_list stacks

  (* Run part 2 with parsed inputs *)
  let part2 data =
    let commands, stacks = data in
    let commands = String.split_lines commands in
    List.iter commands ~f:(fun command ->
        parse_command command stacks stack_move_combined )
end

include M
include Day.Make (M)

(* Example input *)
let example =
  {|
    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2|}

(* Expect test for example input *)
let%expect_test _ = run example ; [%expect {|
CMZ
MCD|}]
