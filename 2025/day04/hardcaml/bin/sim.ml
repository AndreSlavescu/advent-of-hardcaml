open! Core
open! Hardcaml
open! Day04
module Sim = Cyclesim.With_interface (Day04_rtl.I) (Day04_rtl.O)

let read_grid () : char array =
  let lines =
    In_channel.input_lines In_channel.stdin |> List.filter ~f:(Fn.non String.is_empty)
  in
  let height = List.length lines in
  let width = String.length (List.hd_exn lines) in
  let grid = Array.create ~len:(height * width) '.' in
  List.iteri lines ~f:(fun row_idx row ->
    String.iteri row ~f:(fun col_idx ch -> grid.((row_idx * width) + col_idx) <- ch));
  grid
;;

let mode =
  match Sys.get_argv () |> Array.to_list with
  | _ :: "--part1" :: _ -> `Part1
  | _ :: "--part2" :: _ -> `Part2
  | _ ->
    eprintf "Usage: sim.exe --part1|--part2 < day4_input.txt\n";
    exit 2
;;

let run () =
  let grid = read_grid () in
  let scope = Scope.create ~auto_label_hierarchical_ports:true () in
  (* Since cyclesim doesn't support module instantiations, we will simulate the flat design *)
  let sim =
    match mode with
    | `Part1 -> Sim.create (fun i -> Day04_rtl.create_part1 scope i)
    | `Part2 -> Sim.create (fun i -> Day04_rtl.create scope i)
  in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let cycle () = Cyclesim.cycle sim in
  inputs.clear := Bits.vdd;
  cycle ();
  inputs.clear := Bits.gnd;
  cycle ();
  (inputs.part2
   := match mode with
      | `Part1 -> Bits.gnd
      | `Part2 -> Bits.vdd);
  inputs.start := Bits.vdd;
  cycle ();
  inputs.start := Bits.gnd;
  let position = ref 0 in
  inputs.in_valid := Bits.gnd;
  inputs.in_char := Bits.of_int ~width:8 0;
  while not (Bits.to_bool !(outputs.out_valid)) do
    if Bits.to_bool !(outputs.in_ready) && !position < Array.length grid
    then (
      inputs.in_valid := Bits.vdd;
      inputs.in_char := Bits.of_int ~width:8 (Char.to_int grid.(!position));
      incr position)
    else (
      inputs.in_valid := Bits.gnd;
      inputs.in_char := Bits.of_int ~width:8 0);
    cycle ()
  done;
  let result = Bits.to_int !(outputs.out_count) in
  printf "%d\n" result
;;

let () = run ()
