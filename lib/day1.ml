open! Imports
open Base

module M = struct
  (* Type to parse the input into *)
  type t = int list list

  (* Parse the input to type t, invoked for both parts *)
  let parse inputs =
    Str.split (Str.regexp "\n\n") inputs
    |> List.map ~f:(fun s -> Str.split (Str.regexp "\n") s)
    |> List.map ~f:(fun l -> List.map ~f:(fun s -> Int.of_string s) l)

  (* Run part 1 with parsed inputs *)
  let part1 list_of_data =
    let stuff =
      list_of_data
      |> List.map ~f:(fun elf_food ->
             List.fold_left elf_food ~init:0 ~f:( + ) )
    in
    match List.max_elt stuff ~compare:Int.compare with
    | Some x -> Stdio.printf "%d\n" x
    | None -> ()

  (*Run part 2 with parsed inputs *)
  let part2 list_of_data =
    let stuff =
      list_of_data
      |> List.map ~f:(fun elf_food ->
             List.fold_left elf_food ~init:0 ~f:( + ) )
    in
    let sorted = List.sort stuff ~compare:Int.compare |> List.rev in
    List.take sorted 3
    |> List.fold_left ~init:0 ~f:( + )
    |> Stdio.printf "%d\n"
end

include M
include Day.Make (M)

(* Example input *)
let example =
  "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000"

(* Expect test for example input *)
let%expect_test _ = run example ; [%expect {|24000|}]
