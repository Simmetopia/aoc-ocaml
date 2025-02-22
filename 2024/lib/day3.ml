open! Imports
open Core

(* --- Day 3: Mull It Over --- "Our computers are having issues, so I have no
   idea if we have any Chief Historians in stock! You're welcome to check the
   warehouse, though," says the mildly flustered shopkeeper at the North Pole
   Toboggan Rental Shop. The Historians head out to take a look.

   The shopkeeper turns to you. "Any chance you can see why our computers are
   having issues again?"

   The computer appears to be trying to run a program, but its memory (your
   puzzle input) is corrupted. All of the instructions have been jumbled up!

   It seems like the goal of the program is just to multiply some numbers. It
   does that with instructions like mul(X,Y), where X and Y are each 1-3
   digit numbers. For instance, mul(44,46) multiplies 44 by 46 to get a
   result of 2024. Similarly, mul(123,4) would multiply 123 by 4.

   However, because the program's memory has been corrupted, there are also
   many invalid characters that should be ignored, even if they look like
   part of a mul instruction. Sequences like mul(4*, mul(6,9!, ?(12,34), or
   mul ( 2 , 4 ) do nothing.

   For example, consider the following section of corrupted memory:

   xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))
   Only the four highlighted sections are real mul instructions. Adding up
   the result of each instruction produces 161 (2*4 + 5*5 + 11*8 + 8*5).

   Scan the corrupted memory for uncorrupted mul instructions. What do you
   get if you add up all of the results of the multiplications? *)
module M = struct
  (* Type to parse the input into *)
  type t = string list

  let delim = "thiswillneveroccur"

  (* Parse the input to type t, invoked for both parts *)
  let parse inputs = inputs |> String.split_lines

  let pattern = Re2.create_exn "\\w*mul\\((\\d+),(\\d+)\\)"

  let extract_muls str =
    Re2.get_matches_exn pattern str
    |> List.map ~f:(fun m ->
           let n1 = Re2.Match.get_exn ~sub:(`Index 1) m |> Int.of_string in
           let n2 = Re2.Match.get_exn ~sub:(`Index 2) m |> Int.of_string in
           (n1, n2) )

  let computation_enabled_on_chunk str =
    let first = String.get str 0 in
    not (phys_equal first '0')

  let super_mul_list_creatation str =
    let marked =
      str
      |> String.substr_replace_all ~pattern:"do()" ~with_:"\n1"
      |> String.substr_replace_all ~pattern:"don't()" ~with_:"\n0"
    in
    let parts = String.split_lines marked in
    let rec process_parts acc = function
      | [] -> acc
      | part :: rest ->
          let new_acc =
            if computation_enabled_on_chunk part then acc @ extract_muls part
            else acc
          in
          process_parts new_acc rest
    in
    process_parts [] parts

  (* Run part 1 with parsed inputs *)
  let part1 parts =
    let res =
      parts |> String.concat ~sep:"" |> extract_muls
      |> List.map ~f:(fun (a, b) -> a * b)
      |> List.fold ~f:( + ) ~init:0
    in
    Stdio.print_endline (Int.to_string res)

  (* Run part 2 with parsed inputs *)
  let part2 parts =
    let res =
      parts |> String.concat ~sep:"" |> super_mul_list_creatation
      |> List.map ~f:(fun (a, b) -> a * b)
      |> List.fold ~init:0 ~f:( + )
    in
    Stdio.print_endline (Int.to_string res)
end

include M
include Day.Make (M)

(* Example input *)
let example =
  "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

(* Expect test for example input *)
let%expect_test _ = run example ; [%expect {|
  161
  48|}]
