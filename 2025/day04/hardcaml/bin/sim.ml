open! Core
open! Hardcaml
open! Day04

module Sim = Cyclesim.With_interface (Day04_rtl.I) (Day04_rtl.O)

let read_grid () : char array =
  let lines = In_channel.input_lines In_channel.stdin |> List.filter ~f:(Fn.non String.is_empty) in
  let h = List.length lines in
  if h <> Day04_rtl.height
  then failwithf "Expected height=%d, got %d" Day04_rtl.height h ();
  let w = String.length (List.hd_exn lines) in
  if w <> Day04_rtl.width
  then failwithf "Expected width=%d, got %d" Day04_rtl.width w ();
  let buf = Array.create ~len:(h * w) '.' in
  List.iteri lines ~f:(fun r row ->
    if String.length row <> w then failwith "non-rectangular grid";
    String.iteri row ~f:(fun c ch -> buf.((r * w) + c) <- ch));
  buf
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
  (* Note: Cyclesim doesn't support module instantiations, so we simulate the *flat* design
     (directly calling [create]) rather than the hierarchical wrapper. *)
  let sim =
    match mode with
    | `Part1 -> Sim.create (fun i -> Day04_rtl.create_part1 scope i)
    | `Part2 -> Sim.create (fun i -> Day04_rtl.create scope i)
  in
  let i = Cyclesim.inputs sim in
  let o = Cyclesim.outputs sim in

  let cycle () = Cyclesim.cycle sim in

  (* Reset. *)
  i.clear := Bits.vdd;
  cycle ();
  i.clear := Bits.gnd;
  cycle ();

  i.part2 := (match mode with
    | `Part1 -> Bits.gnd
    | `Part2 -> Bits.vdd);

  (* Start pulse. *)
  i.start := Bits.vdd;
  cycle ();
  i.start := Bits.gnd;

  (* Drive input when ready. *)
  let pos = ref 0 in
  i.in_valid := Bits.gnd;
  i.in_char := Bits.of_int ~width:8 0;

  while not (Bits.to_bool !(o.out_valid)) do
    if Bits.to_bool !(o.in_ready)
    then (
      if !pos >= Array.length grid then failwith "RTL requested more input than provided";
      i.in_valid := Bits.vdd;
      i.in_char := Bits.of_int ~width:8 (Char.to_int grid.(!pos));
      incr pos)
    else (
      i.in_valid := Bits.gnd;
      i.in_char := Bits.of_int ~width:8 0);
    cycle ()
  done;

  if !pos <> Array.length grid
  then failwithf "RTL consumed %d cells, expected %d" !pos (Array.length grid) ();

  let ans = Bits.to_int !(o.out_count) in
  printf "%d\n" ans
;;

let () = run ()
;;


