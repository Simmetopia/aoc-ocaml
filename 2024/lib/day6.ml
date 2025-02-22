open! Imports
open Base

module M = struct
  (* Type to parse the input into *)
  type t = char array array

  type direction = Up | Down | Right | Left

  let direction_to_delta = function
    | Up -> (0, 1)
    | Down -> (0, -1)
    | Left -> (-1, 0)
    | Right -> (1, 0)

  let turn_right = function
    | Up -> Right
    | Right -> Down
    | Down -> Left
    | Left -> Up

  let make_move (x, y) dir =
    let dx, dy = direction_to_delta dir in
    (x + dx, y + dy)

  (* Parse the input to type t, invoked for both parts *)
  let parse inputs =
    inputs |> String.split_lines
    |> List.map ~f:String.to_array
    |> Array.of_list

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
