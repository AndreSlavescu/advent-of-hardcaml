open! Core
open! Hardcaml
open! Day04

let generate_day04_rtl () =
  let module C = Circuit.With_interface (Day04_rtl.I) (Day04_rtl.O) in
  let scope = Scope.create ~auto_label_hierarchical_ports:true () in
  let circuit = C.create_exn ~name:"day04_top" (Day04_rtl.hierarchical scope) in
  Rtl.print ~database:(Scope.circuit_database scope) Verilog circuit
;;

let () = generate_day04_rtl ()
;;
