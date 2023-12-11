module type S = sig
  val run : string -> unit
end

module type Impl = sig
  type t

  val parse : string -> t

  val part1 : t -> unit

  val part2 : t -> unit
end

module Make (Impl : Impl) : S = struct
  let run inputs =
    Impl.parse inputs |> Impl.part1 ;
    Impl.parse inputs |> Impl.part2 ;
    ()
end
