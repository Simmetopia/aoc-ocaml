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
let%expect_test _ = run example ; [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure "Int.of_string: \"\"")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Aoc__Day2.M.check_for_impossible_groups.(fun) in file "lib/day2.ml", line 37, characters 30-47
  Called from Base__List.count_map in file "src/list.ml", line 483, characters 13-17
  Called from Base__List.map in file "src/list.ml" (inlined), line 510, characters 15-31
  Called from Aoc__Day2.M.check_for_impossible_groups.(fun) in file "lib/day2.ml" (inlined), line 36, characters 18-101
  Called from Aoc__Day2.M.check_for_impossible_groups.(fun) in file "lib/day2.ml", line 34, characters 15-216
  Called from Base__Container.count.(fun) in file "src/container.ml", line 16, characters 56-59
  Called from Base__List0.fold in file "src/list0.ml", line 37, characters 27-37
  Called from Base__List.count in file "src/list.ml" (inlined), line 939, characters 17-43
  Called from Aoc__Day2.M.check_for_impossible_groups in file "lib/day2.ml" (inlined), line 32, characters 9-476
  Called from Aoc__Day2.M.check_for_impossible_groups in file "lib/day2.ml", line 31, characters 6-489
  Called from Base__List.count_map in file "src/list.ml", line 497, characters 13-17
  Called from Base__List.map in file "src/list.ml" (inlined), line 510, characters 15-31
  Called from Aoc__Day2.M.part1 in file "lib/day2.ml" (inlined), line 53, characters 9-48
  Called from Aoc__Day2.M.part1 in file "lib/day2.ml", line 51, characters 6-118
  Called from Aoc__Day.Make.run in file "lib/day.ml", line 17, characters 4-35
  Called from Aoc__Day2.(fun) in file "lib/day2.ml", line 74, characters 20-31
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

  Trailing output
  ---------------
  3 blue, 4 red
  1 red, 2 green, 6 blue
  2 green |}]
