open! Imports
open Base

module M = struct
  (* Type to parse the input into *)

  type rps = Rock | Paper | Scissor

  type outcome = Win of rps | Lose of rps | Draw of rps

  (* Parse a line of input into a type t *)
  let char_to_rps = function
    | 'A' | 'X' -> Rock
    | 'B' | 'Y' -> Paper
    | 'C' | 'Z' -> Scissor
    | _ -> failwith "Invalid rpc"

  let line_to_tuple str =
    let a = String.split_on_chars str ~on:[' '] in
    match a with
    | [a; b] -> (char_to_rps a.[0], char_to_rps b.[0])
    | _ -> failwith "Invalid input"

  let rps_to_points = function Rock -> 1 | Paper -> 2 | Scissor -> 3

  let outcome_to_points = function
    | Win a -> 6 + rps_to_points a
    | Lose a -> rps_to_points a
    | Draw a -> 3 + rps_to_points a

  let tuple_to_outcome (a, b) =
    match (a, b) with
    | Rock, Scissor | Scissor, Paper | Paper, Rock -> Win a
    | Rock, Paper | Scissor, Rock | Paper, Scissor -> Lose a
    | _ -> Draw a

  type t = string list

  (* Parse the input to type t, invoked for both parts *)
  let parse inputs = Str.split (Str.regexp "\n") inputs

  let part1 inputs =
    let inputs =
      inputs |> List.map ~f:line_to_tuple |> List.map ~f:tuple_to_outcome
    in
    let points = List.map ~f:outcome_to_points inputs in
    let tp = List.fold_left points ~init:0 ~f:( + ) in
    Stdio.printf "%d\n" tp

  let list_to_tuple (l : string list) =
    match l with [a; b] -> (a.[0], b.[0]) | _ -> failwith "Invalid input"

  let line_to_points str =
    let a = String.split_on_chars str ~on:[' '] in
    (* X=lose, Y=draw Z=win *)
    (* A=rock, B=paper, C=scissor *)
    match list_to_tuple a with
    | 'A', 'X' -> 3
    | 'A', 'Y' -> 4
    | 'A', 'Z' -> 8
    | 'B', 'X' -> 1
    | 'B', 'Y' -> 5
    | 'B', 'Z' -> 9
    | 'C', 'X' -> 2
    | 'C', 'Y' -> 6
    | 'C', 'Z' -> 7
    | _ -> failwith "Invalid input"

  (* Run part 2 with parsed inputs *)
  let part2 inputs =
    let points = List.map ~f:line_to_points inputs in
    let tp = List.fold_left points ~init:0 ~f:( + ) in
    Stdio.printf "%d\n" tp
end

include M
include Day.Make (M)

let example = "A Y\nB X\nC Z\n"

(* Example of how to run day1 *)

(* Expect test for example input *)
let%expect_test _ = run example ; [%expect {|
15
12|}]
