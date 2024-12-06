open! Imports
open Base

module M = struct
  (* Type to parse the input into *)
  type t = int list list

  (* Parse the input to type t, invoked for both parts *)
  let parse inputs =
    inputs |> String.split_lines
    |> List.map ~f:(String.split ~on:' ')
    |> List.map ~f:(List.map ~f:Int.of_string)

  type is_gut = Good | Bad

  let parse_iline (l : int list) =
    match l with
    | [] | [_] -> Good (* empty or single number is valid *)
    | first :: second :: _ ->
        (* Determine if sequence should be rising or falling based on first
           two numbers *)
        let is_rising = second > first in
        (* Debug print *)
        let rec check_sequence prev rest =
          match rest with
          | [] -> Good (* reached end without violations *)
          | curr :: remaining ->
              let diff = abs (curr - prev) in
              if
                (is_rising && curr > prev && diff >= 1 && diff <= 3)
                || ((not is_rising) && curr < prev && diff >= 1 && diff <= 3)
              then check_sequence curr remaining
              else Bad
        in
        check_sequence first (List.tl_exn l)

  (* Run part 1 with parsed inputs *)
  let part1 inputs =
    let res =
      inputs |> List.map ~f:parse_iline
      |> List.fold ~init:0 ~f:(fun acc is_gut ->
             match is_gut with Good -> acc + 1 | Bad -> acc )
    in
    Stdio.printf "%d\n" res

  (* Run part 2 with parsed inputs *)
  let part2 inputs =
    let res =
      inputs
      |> List.map ~f:(fun line ->
             if phys_equal (parse_iline line) Good then Good
             else
               let rec try_remove_index index =
                 if index >= List.length line then Bad
                 else
                   let without_el =
                     line |> List.filteri ~f:(fun idx _ -> index <> idx)
                   in
                   if phys_equal (parse_iline without_el) Good then Good
                   else try_remove_index (index + 1)
               in
               try_remove_index 0 )
      |> List.fold ~init:0 ~f:(fun acc is_gut ->
             match is_gut with Good -> acc + 1 | Bad -> acc )
    in
    Stdio.printf "%d\n" res
end

include M
include Day.Make (M)

(* Example input *)
let example =
  "7 6 4 2 1\n1 2 7 8 9\n9 7 6 2 1\n1 3 2 4 5\n8 6 4 4 1\n1 3 6 7 9"

(* Expect test for example input *)
let%expect_test _ = run example ; [%expect {|
  2
  4|}]
