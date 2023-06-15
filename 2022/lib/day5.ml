open! Imports
open Base

module M = struct
  (* Type to parse the input into *)
  type t = string * char Stack.t list

  (** 
      [D]    
  [N] [C]    
  [Z] [M] [P]
   1   2   3 
  In this example, there are three stacks of crates. Stack 1 contains two
  crates: crate Z is on the bottom, and crate N is on top. Stack 2 contains
  three crates; from bottom to top, they are crates M, C, and D. Finally, stack
  3 contains a single crate, P.
  *)
  let string_to_list_stacks str = 
    let lines = String.split_lines str in
    let last_line = List.last_exn lines in
    let amount_of_stacks = String.split_on_chars ~on:[' ']  last_line |> List.length in
    let stacks = List.init amount_of_stacks ~f:(fun _ -> Stack.create ()) in
    let without_last_line = List.take lines (List.length lines - 1) in

  (* Convert the parsed input into the appropriate type *)
  let tuple_to_output (a, b) = (string_to_list_stacks a, b)

  (* Read the input into a tuple of strings *)
  let parse inputs =
    let a = Str.split (Str.regexp "\n\n") inputs in
    match a with [a; b] -> (a, b) | _ -> failwith "bad input"

  (* Run part 1 with parsed inputs *)
  let part1 _ = ()

  (* Run part 2 with parsed inputs *)
  let part2 _ = ()
end

include M
include Day.Make (M)

(* Example input *)
let example = ""

(* Expect test for example input *)
let%expect_test _ = run example ; [%expect {| |}]
