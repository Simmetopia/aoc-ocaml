open! Imports
open Base

module M = struct
  (* Type to parse the input into *)
  type t = string list

  (* Parse the input to type t, invoked for both parts *)
  let parse inputs = inputs |> String.split_lines

  (* Lowercase item types a through z have priorities 1 through 26. *)
  (* Uppercase item types A through Z have priorities 27 through 52. *)
  let char_to_points c =
    match c with
    | 'a' .. 'z' -> Char.to_int c - Char.to_int 'a' + 1
    | 'A' .. 'Z' -> Char.to_int c - Char.to_int 'A' + 27
    | _ -> failwith "Invalid char"

  let part1_line (a, b) =
    let a = Set.of_list (module Char) a in
    let b = Set.of_list (module Char) b in
    let intersection = Set.inter a b in
    char_to_points @@ Set.choose_exn intersection

  let str_to_set s = Set.of_list (module Char) (String.to_list s)

  let part2_line (list : string list) =
    match List.map ~f:str_to_set list with
    | [a; b; c] ->
        let intersection = Set.inter a (Set.inter c b) in
        Set.choose_exn intersection |> char_to_points
    | _ ->
        failwith
          "Invalid input: the list should contain exactly three elements"

  let part1 inputs =
    let res =
      inputs
      |> List.map ~f:(fun st ->
             let a = String.to_list st in
             List.split_n a (List.length a / 2) )
      |> List.map ~f:part1_line
      |> List.fold_left ~init:0 ~f:( + )
    in
    Stdio.printf "%d\n" res

  (* Run part 2 with parsed inputs *)
  let part2 (inputs : string list) =
    let res =
      List.chunks_of ~length:3 inputs
      |> List.map ~f:part2_line
      |> List.fold_left ~init:0 ~f:( + )
    in
    Stdio.printf "%d\n" res
end

include M
include Day.Make (M)

(* Example input *)
let example =
  {|vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw|}

(* Expect test for example input *)
let%expect_test _ = run example ; [%expect {|
157
70|}]
