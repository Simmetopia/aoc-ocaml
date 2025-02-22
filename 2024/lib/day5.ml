open! Imports
open Base

module M = struct
  (* Type to parse the input into *)
  type res = {lookup: key:int -> int list; list: int list list}

  type t = res

  let create_table pairs =
    let tbl = Hashtbl.create (module Int) in
    List.iter pairs ~f:(fun (key, value) ->
        Hashtbl.update tbl key ~f:(function
          | None -> [value]
          | Some v -> value :: v ) ) ;
    tbl

  let lookup tbl key = Hashtbl.find tbl key |> Option.value ~default:[]

  (* Parse the input to type t, invoked for both parts *)
  let parse inputs =
    match
      inputs
      |> String.substr_replace_all ~pattern:"\n\n" ~with_:"&"
      |> String.split ~on:'&'
    with
    | [hd; tl] ->
        let pairs =
          hd |> String.split_lines
          |> List.map ~f:(fun line ->
                 match String.split ~on:'|' line with
                 | [x; y] -> (Int.of_string x, Int.of_string y)
                 | _ -> failwith "Invalid pair format" )
        in
        let lookup_tbl = create_table pairs in
        { lookup= (fun ~key -> lookup lookup_tbl key)
        ; list=
            tl |> String.split_lines
            |> List.map ~f:(fun line ->
                   String.split ~on:',' line |> List.map ~f:Int.of_string )
        }
    | _ -> failwith "Invalid input"

  (* Run part 1 with parsed inputs *)
  let part1 {lookup; list} =
    let rec check_line =
     fun prev l is_valid ->
      if Bool.equal is_valid false then false
      else
        match l with
        | [] -> is_valid
        | hd :: tail ->
            let never_before = lookup ~key:hd in
            prev
            |> List.for_all ~f:(fun x ->
                   not (List.mem ~equal:( = ) never_before x) )
            |> check_line (hd :: prev) tail
    in
    let res =
      list
      |> List.filter ~f:(fun x -> check_line [] x true)
      |> List.map ~f:(fun x -> List.nth_exn x (List.length x / 2))
      |> List.fold ~f:( + ) ~init:0
    in
    Stdio.printf "%i" res

  (* Run part 2 with parsed inputs *)
  let part2 _ = ()
end

include M
include Day.Make (M)

(* Example input *)
let example =
  "47|53\n\
   97|13\n\
   97|61\n\
   97|47\n\
   75|29\n\
   61|13\n\
   75|53\n\
   29|13\n\
   97|29\n\
   53|29\n\
   61|53\n\
   97|53\n\
   61|29\n\
   47|13\n\
   75|47\n\
   97|75\n\
   47|61\n\
   75|61\n\
   47|29\n\
   75|13\n\
   53|13\n\n\
   75,47,61,53,29\n\
   97,61,53,29,13\n\
   75,29,13\n\
   75,97,47,61,53\n\
   61,13,29\n\
   97,13,75,29,47"

(* Expect test for example input *)
let%expect_test _ = run example ; [%expect {|143|}]
