open! Imports
open Base

module M = struct
  (* Type to parse the input into *)

  type t = string

  (* Parse the input into the type *)
  let parse input = input

  let id_and_groups str =
    let id, groups = String.lsplit2_exn str ~on:':' in
    let _, id = String.lsplit2_exn id ~on:' ' in
    let groups = String.split groups ~on:';' in
    let groups = List.map groups ~f:(fun group -> String.strip group) in
    (id, groups)

  let impossibilities = [("red", 12); ("green", 13); ("blue", 14)]

  (* Parse the input into the type by color, if the num is higher than the
     int, its impossible *)
  let check_for_impossible_groups (id, groups) =
    let _ = id in
    (* Assuming 'id' is not used in this function *)

    (* print groups *)
    List.iter groups ~f:(fun group -> Stdio.printf "%s\n" group) ;
    (* print impossibilities *)
    let rest =
      groups
      |> List.count ~f:(fun group ->
             let list =
               group |> String.split ~on:','
               |> List.map ~f:(fun f -> String.lsplit2_exn f ~on:' ')
               |> List.map ~f:(fun (num, color) ->
                      (color, Int.of_string num) )
             in
             List.iter list ~f:(fun (color, num) ->
                 Stdio.printf "%s: %d\n" color num ) ;
             true (* Assuming you always return true; adjust as needed *) )
    in
    (id, rest)

  let print_id_and_group (id, groups) =
    Stdio.printf "%s: %s\n" id (String.concat ~sep:"; " groups)

  (* Run part 1 with parsed inputs *)
  let part1 inputs =
    let _ =
      String.split ~on:'\n' inputs
      |> List.map ~f:id_and_groups
      |> List.map ~f:check_for_impossible_groups
    in
    ()

  (* Run part 2 with parsed inputs *)
  let part2 _inputs = ()
end

include M
include Day.Make (M)

let example =
  {|Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green|}

(* Example of how to run day1 *)

(* Expect test for example input *)
let%expect_test _ = run example ; [%expect {|
15
12|}]
