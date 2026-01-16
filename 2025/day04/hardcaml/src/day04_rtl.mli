module I : sig
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; start : 'a
    ; part2 : 'a
    ; in_valid : 'a
    ; in_char : 'a [@bits 8]
    }
  [@@deriving hardcaml]
end

module O : sig
  type 'a t =
    { in_ready : 'a
    ; out_valid : 'a
    ; out_count : 'a [@bits 32]
    }
  [@@deriving hardcaml]
end

val create_part1 : Hardcaml.Scope.t -> Hardcaml.Signal.t I.t -> Hardcaml.Signal.t O.t
val create : Hardcaml.Scope.t -> Hardcaml.Signal.t I.t -> Hardcaml.Signal.t O.t
val hierarchical : Hardcaml.Scope.t -> Hardcaml.Signal.t I.t -> Hardcaml.Signal.t O.t
