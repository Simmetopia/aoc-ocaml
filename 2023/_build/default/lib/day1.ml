open! Imports
open Base

module M = struct
  (* Type to parse the input into *)
  type t = string list

  let print_int_list lst =
    let string_list = List.map ~f:Int.to_string lst in
    let joined_string = String.concat ~sep:" " string_list in
    Stdio.printf "[%s]\n" joined_string

  let filter_non_numeric_ascii char = Char.get_digit char

  let return_fist_and_last_of_list string_input =
    let integer_list =
      String.to_list string_input
      |> List.filter_map ~f:filter_non_numeric_ascii
    in
    let joined_int_string =
      match (List.hd integer_list, List.last integer_list) with
      | Some first, Some last -> Some (Printf.sprintf "%d%d" first last)
      | _ -> None
    in
    match joined_int_string with
    | Some s -> Some (Int.of_string s)
    | None -> None

  let valid_integers string_input =
    string_input |> List.filter_map ~f:return_fist_and_last_of_list

  (* Parse the input to type t, invoked for both parts *)

  (* Parse the input to type t, invoked for both parts *)
  let parse inputs = String.split inputs ~on:'\n'

  (* Run part 1 with parsed inputs *)
  let part1 inputs =
    let res = inputs |> valid_integers |> List.sum (module Int) ~f:Fn.id in
    Stdio.printf "%d\n" res

  let process_string str =
    let number_words_to_digits = function
      | "zero" -> "0"
      | "one" -> "1"
      | "two" -> "2"
      | "three" -> "3"
      | "four" -> "4"
      | "five" -> "5"
      | "six" -> "6"
      | "seven" -> "7"
      | "eight" -> "8"
      | "nine" -> "9"
      | digit when Char.is_digit digit.[0] ->
          String.of_char digit.[0] (* Directly use single digits *)
      | _ -> "" (* This case should never happen *)
    in
    let pattern =
      Re.(
        compile
          (alt
             [ str "zero"
             ; str "one"
             ; str "two"
             ; str "three"
             ; str "four"
             ; str "five"
             ; str "six"
             ; str "seven"
             ; str "eight"
             ; str "nine"
             ; digit ] ) )
    in
    let rec find_matches s pos acc =
      if pos >= String.length s then acc
      else
        match Re.exec_opt ~pos pattern s with
        | Some group ->
            let matched_word = Re.Group.get group 0 in
            let digit = number_words_to_digits matched_word in
            find_matches s (pos + 1) (acc ^ digit)
        | None -> find_matches s (pos + 1) acc
    in
    find_matches str 0 ""

  (* Run part 2 with parsed inputs *)
  let part2 input =
    let res =
      input
      |> List.map ~f:process_string
      |> valid_integers
      |> List.sum (module Int) ~f:Fn.id
    in
    Stdio.printf "%d\n" res
end

include M
include Day.Make (M)

(* Example input *)
let example =
  "two1nine\n\
   eightwothree\n\
   abcone2threexyz\n\
   xtwone3four\n\
   4nineeightseven2\n\
   zoneight234\n\
   7pqrstsixteen"

(* Expect test for example input *)
let%expect_test _ = run example ; [%expect {|
209
281|}]
