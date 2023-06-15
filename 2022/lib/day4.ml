open! Imports
open Base

module M = struct
  (* Type to parse the input into *)
  type t = (string * string) list

  (* Parse the input to type t, invoked for both parts *)
  let parse inputs =
    String.split ~on:'\n' inputs
    |> List.map ~f:(String.split_on_chars ~on:[','])
    |> List.map ~f:(function [a; b] -> (a, b) | _ -> failwith "bad input")

  let str_to_sequence str =
    match String.split_on_chars ~on:['-'] str with
    | [start; end'] ->
        Sequence.range (Int.of_string start) (Int.of_string end' + 1)
    | _ -> failwith "Invalid range string"

  let parse_range str = Set.of_sequence (module Int) (str_to_sequence str)

  let print_set_info prefix set =
    let elements = Set.to_list set in
    let min_value = List.hd_exn elements in
    let max_value = List.last_exn elements in
    Stdio.printf "%s: Min: %d, Max: %d\n" prefix min_value max_value

  let is_range_contained (str1, str2) =
    let set1 = parse_range str1 in
    let set2 = parse_range str2 in
    Set.is_subset set1 ~of_:set2 || Set.is_subset set2 ~of_:set1

  let does_range_overlap (str1, str2) =
    let set1 = parse_range str1 in
    let set2 = parse_range str2 in
    let has_intersection = Set.inter set2 set1 in
    not (Set.is_empty has_intersection)

  (* Run part 1 with parsed inputs *)
  let part1 string_tuple_list =
    string_tuple_list
    |> List.filter ~f:is_range_contained
    |> List.length |> print_endline_int

  (* Run part 2 with parsed inputs *)
  let part2 string_tuple_list =
    string_tuple_list
    |> List.filter ~f:does_range_overlap
    |> List.length |> print_endline_int
end

include M
include Day.Make (M)

(* Example input *)
let example = {|2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8|}

(* Expect test for example input *)
let%expect_test _ = run example ; [%expect {|
    2
    4|}]
