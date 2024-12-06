open! Imports
open Base

module M = struct
  (* Type to parse the input into *)
  type t = int list * int list

  (* Parse the input to type t, invoked for both parts *)
  let parse inputs =
    inputs |> String.split_lines
    |> List.map ~f:(String.substr_replace_all ~pattern:"   " ~with_:" ")
    |> List.map ~f:(String.split ~on:' ')
    |> List.map ~f:(function
         | [a; b] -> (Int.of_string a, Int.of_string b)
         | _ -> failwith "Invalid input" )
    |> List.fold_left ~init:([], []) ~f:(fun (acc1, acc2) (a, b) ->
           (a :: acc1, b :: acc2) )

  (* Run part 1 with parsed inputs *)
  let part1 (a, b) =
    let sorted_a = List.sort ~compare:Int.compare a in
    let sorted_b = List.sort ~compare:Int.compare b in
    let res =
      List.zip_exn sorted_a sorted_b
      |> List.map ~f:(fun (a, b) -> a - b |> abs)
      |> List.fold_left ~init:0 ~f:( + )
    in
    Stdio.printf "%d\n" res

  (* Run part 2 with parsed inputs *)
  let part2 (a, b) =
    let new_thing =
      a
      |> List.map ~f:(fun a ->
             b
             |> List.filter ~f:(fun element_in_b -> element_in_b = a)
             |> List.length )
    in
    let res =
      List.zip_exn a new_thing
      |> List.map ~f:(fun (a, b) -> a * b |> abs)
      |> List.fold_left ~init:0 ~f:( + )
    in
    Stdio.printf "%d\n" res
end

include M
include Day.Make (M)

(* Example input *)
let example = "3   4\n4   3\n2   5\n1   3\n3   9\n3   3"

(* Expect test for example input *)
let%expect_test _ = run example ; [%expect {|
  11
  31|}]
