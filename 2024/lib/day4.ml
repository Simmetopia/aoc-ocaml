open! Imports
open Base

module M = struct
  (* Type to parse the input into *)
  type t = char array array

  (* Parse the input to type t, invoked for both parts *)
  let parse inputs =
    inputs |> String.split_lines
    |> List.map ~f:(fun line -> String.to_array line)
    |> Array.of_list

  let is_valid (x, y) = x >= 0 && x < 10 && y >= 0 && y < 10

  let directions =
    [ (0, 1)
    ; (* Right *)
      (0, -1)
    ; (* Left *)
      (1, 0)
    ; (* Down *)
      (-1, 0)
    ; (* Up *)
      (1, 1)
    ; (* Down-right *)
      (1, -1)
    ; (* Down-left *)
      (-1, 1)
    ; (* Up-right *)
      (-1, -1) (* Up-left *) ]

  let count_xmas (grid : char array array) =
    let rows = Array.length grid in
    let cols = Array.length grid.(0) in
    List.init rows ~f:(fun r ->
        List.init cols ~f:(fun c ->
            List.fold ~init:0
              ~f:(fun acc (dr, dc) ->
                try
                  let xmas =
                    List.init 4 ~f:(fun i ->
                        grid.(r + (i * dr)).(c + (i * dc)) )
                    |> String.of_char_list
                    (* Base's version of creating string from chars *)
                  in
                  if String.( = ) xmas "XMAS" then acc + 1 else acc
                with _ -> acc )
              directions ) )
    |> List.join (* Base uses join instead of concat *)
    |> List.fold ~init:0 ~f:( + )

  let find_x_mas (grid : char array array) =
    let rows = Array.length grid in
    let cols = Array.length grid.(0) in
    (* Helper to check if a sequence spells "MAS" starting from (r,c) in
       direction (dr,dc) *)
    let check_mas r c dr dc =
      try
        let mas =
          String.of_char_list
            [ grid.(r).(c)
            ; grid.(r + dr).(c + dc)
            ; grid.(r + (2 * dr)).(c + (2 * dc)) ]
        in
        if String.equal "MAS" mas || String.equal "SAM" mas then true
        else false
      with _ -> false
    in
    (* These are the four diagonal directions for checking X patterns *)
    let x_patterns = [(1, 1); (1, -1)] in
    List.init rows ~f:(fun r ->
        List.init cols ~f:(fun c ->
            if Char.equal grid.(r).(c) 'A' then
              (* For each 'A', check if it's the center of an X made of two
                 "MAS" sequences *)
              List.count x_patterns ~f:(fun (dr1, dc1) ->
                  (* Check both MAS sequences forming the X *)
                  if check_mas (r - dr1) (c - dc1) dr1 dc1 then true
                  else false )
            else 0 ) )
    |> List.join

  (* Run part 1 with parsed inputs *)
  let part1 the_thing =
    let res = the_thing |> count_xmas in
    Stdio.printf "%d\n" res

  (* Run part 2 with parsed inputs *)
  let part2 the_thing =
    let res =
      the_thing |> find_x_mas
      |> List.filter ~f:(fun x -> x = 2)
      |> List.length
    in
    Stdio.printf "%d\n" res
end

include M
include Day.Make (M)

(* Example input *)
let example =
  "MMMSXXMASM\n\
   MSAMXMSMSA\n\
   AMXSXMAAMM\n\
   MSAMASMSMX\n\
   XMASAMXAMM\n\
   XXAMMXXAMA\n\
   SMSMSASXSS\n\
   SAXAMASAAA\n\
   MAMMMXMMMM\n\
   MXMXAXMASX"

(* Expect test for example input *)
let%expect_test _ = run example ; [%expect {|
  18
  9|}]
