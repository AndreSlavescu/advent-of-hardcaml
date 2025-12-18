open! Core
open! Hardcaml
open! Signal

(* Day 4 is a classic 3x3 stencil: for each '@' cell, count 8 neighbors and check < 4.

   A very FPGA-friendly architecture is a streaming stencil with:
   - two line-buffers (previous 2 rows),
   - 3-wide shift registers per row (previous/current/next columns),
   - popcount of the 8 neighbors,
   - an accumulator of how many centers are accessible.

   Part 2 can be viewed as a k-core peel on the 8-neighbor graph: repeatedly remove any
   '@' cell with <4 '@' neighbors until stable. This implementation uses a queue-based
   peel (rather than rescanning the whole frame many times).
*)

(* Fixed-size frame, streamed in as ASCII '.'/'@' with ready/valid.

   Your input is 137x137, so we pick that as the default. The module itself performs
   *implicit zero-padding* on all four sides (top/bottom/left/right) so edge handling is
   automatic.

   Part 1: run one stencil pass and count accessible rolls.
   Part 2: treat part 1's predicate as "removable", remove them, and repeat.
*)

let int_env name ~default =
  match Sys.getenv name with
  | None -> default
  | Some s -> Int.of_string s
;;

let width = int_env "DAY04_WIDTH" ~default:137
let height = int_env "DAY04_HEIGHT" ~default:137

let () =
  if width <= 0 || height <= 0 then failwithf "Invalid grid size: %dx%d" width height ()
;;

let size = width * height
let addr_width = address_bits_for size

(* Popcount tree arity. For FPGA LUTs, arities like 3 or 4 can be a good fit. *)
let popcount_branching_factor = 4

let padded_width = width + 2
let padded_height = height + 2

let coord_width = num_bits_to_represent (Int.max (padded_width - 1) (padded_height - 1))
let row_bits = num_bits_to_represent (height - 1)
let col_bits = num_bits_to_represent (width - 1)
let queue_bits = row_bits + col_bits

module I = struct
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

module O = struct
  type 'a t =
    { in_ready : 'a
    ; out_valid : 'a
    ; out_count : 'a [@bits 32]
    }
  [@@deriving hardcaml]
end

module States = struct
  type t =
    | Idle
    | Load
    | Peel_pop
    | Peel_neighbors
    | Done
  [@@deriving sexp_of, compare ~localize, enumerate]
end

module Part1_states = struct
  type t =
    | Idle
    | Running
    | Done
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let create_part1 scope ({ clock; clear; start; part2 = _; in_valid; in_char } : _ I.t) : _ O.t =
  ignore scope;
  let spec = Reg_spec.create ~clock ~clear () in
  let open Always in
  let sm = State_machine.create (module Part1_states) spec in

  let%hw_var r = Variable.reg spec ~width:coord_width in
  let%hw_var c = Variable.reg spec ~width:coord_width in
  let%hw_var count = Variable.reg spec ~width:32 in

  let running = sm.is Running in

  let r0 = of_int ~width:coord_width 0 in
  let r1 = of_int ~width:coord_width 1 in
  let r2 = of_int ~width:coord_width 2 in
  let r_h = of_int ~width:coord_width height in
  let r_h1 = of_int ~width:coord_width (height + 1) in
  let r_last = of_int ~width:coord_width (padded_height - 1) in

  let c0 = of_int ~width:coord_width 0 in
  let c1 = of_int ~width:coord_width 1 in
  let c2 = of_int ~width:coord_width 2 in
  let c_w = of_int ~width:coord_width width in
  let c_w1 = of_int ~width:coord_width (width + 1) in
  let c_last = of_int ~width:coord_width (padded_width - 1) in

  let one_coord = of_int ~width:coord_width 1 in
  let c_is_last = c.value ==: c_last in

  let need_input_raw =
    running
    &: (r.value >=: r1)
    &: (r.value <=: r_h)
    &: (c.value >=: c1)
    &: (c.value <=: c_w)
  in
  let in_ready = need_input_raw in
  let step = running &: mux2 need_input_raw in_valid vdd in

  let valid_center_raw =
    (r.value >=: r2) &: (r.value <=: r_h1) &: (c.value >=: c2) &: (c.value <=: c_w1)
  in
  let valid_center = step &: valid_center_raw in

  let at_code = of_int ~width:8 (Char.to_int '@') in
  let pix = need_input_raw &: in_valid &: (in_char ==: at_code) in

  let reg_en x = reg spec ~enable:step x in
  let delay n x =
    let rec go i acc = if i = 0 then acc else go (i - 1) (reg_en acc) in
    go n x
  in

  let row_delay = padded_width in
  let bot = pix in
  let mid = delay row_delay pix in
  let top = delay (2 * row_delay) pix in

  let bot_1 = delay 1 bot in
  let bot_2 = delay 2 bot in
  let mid_1 = delay 1 mid in
  let mid_2 = delay 2 mid in
  let top_1 = delay 1 top in
  let top_2 = delay 2 top in

  let neighbor_bits = concat_msb [ top_2; top_1; top; mid_2; mid; bot_2; bot_1; bot ] in
  let deg4 = popcount ~branching_factor:popcount_branching_factor neighbor_bits in
  let accessible = mid_1 &: (deg4 <:. 4) in

  let done_now = step &: (r.value ==: r_last) &: (c.value ==: c_last) in

  compile
    [ sm.switch
        [ ( Idle
          , [ when_
                start
                [ r <-- r0; c <-- c0; count <-- zero 32; sm.set_next Running ]
            ] )
        ; ( Running
          , [ when_ valid_center [ count <-- count.value +: (zero 31 @: accessible) ]
            ; when_ (step &: c_is_last) [ c <-- c0; r <-- r.value +: one_coord ]
            ; when_ (step &: ~:c_is_last) [ c <-- c.value +: one_coord ]
            ; when_ done_now [ sm.set_next Done ]
            ] )
        ; ( Done
          , [ (* one-cycle pulse *)
              sm.set_next Idle
            ] )
        ]
    ];

  { in_ready; out_valid = sm.is Done; out_count = count.value }
;;

let create scope ({ clock; clear; start; part2; in_valid; in_char } : _ I.t) : _ O.t =
  ignore scope;
  let spec = Reg_spec.create ~clock ~clear () in

  let open Always in
  let sm = State_machine.create (module States) spec in

  (* Scan counters over the padded frame. *)
  let%hw_var r = Variable.reg spec ~width:coord_width in
  let%hw_var c = Variable.reg spec ~width:coord_width in
  let%hw_var cell_addr = Variable.reg spec ~width:addr_width in

  (* Part 1 accumulator (only used when [part2=0]). *)
  let%hw_var part1_count = Variable.reg spec ~width:32 in

  (* Part 2 data structures. *)
  let%hw_var q_rptr = Variable.reg spec ~width:addr_width in
  let%hw_var q_wptr = Variable.reg spec ~width:addr_width in
  let%hw_var removed = Variable.reg spec ~width:32 in

  let%hw_var cur_row = Variable.reg spec ~width:row_bits in
  let%hw_var cur_col = Variable.reg spec ~width:col_bits in
  let%hw_var neigh_idx = Variable.reg spec ~width:3 in

  let%hw_var result = Variable.reg spec ~width:32 in

  let load = sm.is Load in
  let peel_pop = sm.is Peel_pop in
  let peel_neighbors = sm.is Peel_neighbors in

  let r0 = of_int ~width:coord_width 0 in
  let r1 = of_int ~width:coord_width 1 in
  let r2 = of_int ~width:coord_width 2 in
  let r_h = of_int ~width:coord_width height in
  let r_h1 = of_int ~width:coord_width (height + 1) in
  let r_last = of_int ~width:coord_width (padded_height - 1) in

  let c0 = of_int ~width:coord_width 0 in
  let c1 = of_int ~width:coord_width 1 in
  let c2 = of_int ~width:coord_width 2 in
  let c_w = of_int ~width:coord_width width in
  let c_w1 = of_int ~width:coord_width (width + 1) in
  let c_last = of_int ~width:coord_width (padded_width - 1) in

  let one_coord = of_int ~width:coord_width 1 in
  let one_addr = of_int ~width:addr_width 1 in
  let c_is_last = c.value ==: c_last in

  (* Address mapping: addr = row*W + col (W is constant for this elaborated circuit). *)
  let addr_of_row_col row col =
    let row = uresize row addr_width in
    let col = uresize col addr_width in
    let w = of_int ~width:addr_width width in
    uresize (row *: w) addr_width +: col
  in

  (* Load phase: scan padded coordinates, accept input only for the interior (1..h,1..w). *)
  let need_input_raw =
    (r.value >=: r1) &: (r.value <=: r_h) &: (c.value >=: c1) &: (c.value <=: c_w)
  in
  let in_ready = load &: need_input_raw in
  let step = load &: mux2 need_input_raw in_valid vdd in

  (* Valid centers correspond to original cells, via our 1-cycle/row-delay windowing:
     center is (r-1,c-1), so r ∈ [2..h+1], c ∈ [2..w+1]. *)
  let valid_center_raw =
    (r.value >=: r2) &: (r.value <=: r_h1) &: (c.value >=: c2) &: (c.value <=: c_w1)
  in
  let valid_center = step &: valid_center_raw in

  let at_code = of_int ~width:8 (Char.to_int '@') in
  let pix = load &: need_input_raw &: in_valid &: (in_char ==: at_code) in

  (* Streaming 3x3 window generator (only advances during load). *)
  let reg_en x = reg spec ~enable:step x in
  let delay n x =
    let rec go i acc = if i = 0 then acc else go (i - 1) (reg_en acc) in
    go n x
  in

  let row_delay = padded_width in
  let bot = pix in
  let mid = delay row_delay pix in
  let top = delay (2 * row_delay) pix in

  let bot_1 = delay 1 bot in
  let bot_2 = delay 2 bot in
  let mid_1 = delay 1 mid in
  let mid_2 = delay 2 mid in
  let top_1 = delay 1 top in
  let top_2 = delay 2 top in

  let neighbor_bits = concat_msb [ top_2; top_1; top; mid_2; mid; bot_2; bot_1; bot ] in
  let deg4 = popcount ~branching_factor:popcount_branching_factor neighbor_bits in
  let accessible = mid_1 &: (deg4 <:. 4) in

  (* Node memory stores per-cell state.
     We encode "dead" as 4'hF to save one bit vs {alive,deg}. *)
  let dead4 = of_int ~width:4 15 in
  let node_data_init = mux2 mid_1 deg4 dead4 in

  (* Queue entry stores {row, col}. *)
  let row_center = uresize (r.value -:. 2) row_bits in
  let col_center = uresize (c.value -:. 2) col_bits in
  let q_data_load = concat_msb [ row_center; col_center ] in

  (* Queue memory (read + single write port). *)
  let q_we_w = wire 1 in
  let q_wr_data_w = wire queue_bits in
  let q_rd =
    (multiport_memory
       ~name:"day04_queue"
       ~attributes:[ Rtl_attribute.Vivado.Ram_style.block ]
       size
       ~write_ports:
         [| { Write_port.write_clock = clock
            ; write_address = q_wptr.value
            ; write_enable = q_we_w
            ; write_data = q_wr_data_w
            }
         |]
       ~read_addresses:[| q_rptr.value |]).(0)
  in

  (* Node memory read. *)
  let node_rd_addr_w = wire addr_width in
  let node_we_w = wire 1 in
  let node_wr_addr_w = wire addr_width in
  let node_wr_data_w = wire 4 in

  let node_rd =
    (multiport_memory
       ~name:"day04_nodes"
       ~attributes:[ Rtl_attribute.Vivado.Ram_style.block ]
       size
       ~write_ports:
         [| { Write_port.write_clock = clock
            ; write_address = node_wr_addr_w
            ; write_enable = node_we_w
            ; write_data = node_wr_data_w
            }
         |]
       ~read_addresses:[| node_rd_addr_w |]).(0)
  in

  let node_val = node_rd in
  let node_alive = node_val <>:. 15 in

  let q_row = select q_rd (queue_bits - 1) col_bits in
  let q_col = select q_rd (col_bits - 1) 0 in
  let addr_pop = addr_of_row_col q_row q_col in

  (* Neighbor selection for peel phase. *)
  let row_gt0 = cur_row.value >:. 0 in
  let row_lt_last = cur_row.value <:. (height - 1) in
  let col_gt0 = cur_col.value >:. 0 in
  let col_lt_last = cur_col.value <:. (width - 1) in

  let nrow0 = cur_row.value -:. 1 in
  let ncol0 = cur_col.value -:. 1 in
  let nvalid0 = row_gt0 &: col_gt0 in

  let nrow1 = cur_row.value -:. 1 in
  let ncol1 = cur_col.value in
  let nvalid1 = row_gt0 in

  let nrow2 = cur_row.value -:. 1 in
  let ncol2 = cur_col.value +:. 1 in
  let nvalid2 = row_gt0 &: col_lt_last in

  let nrow3 = cur_row.value in
  let ncol3 = cur_col.value -:. 1 in
  let nvalid3 = col_gt0 in

  let nrow4 = cur_row.value in
  let ncol4 = cur_col.value +:. 1 in
  let nvalid4 = col_lt_last in

  let nrow5 = cur_row.value +:. 1 in
  let ncol5 = cur_col.value -:. 1 in
  let nvalid5 = row_lt_last &: col_gt0 in

  let nrow6 = cur_row.value +:. 1 in
  let ncol6 = cur_col.value in
  let nvalid6 = row_lt_last in

  let nrow7 = cur_row.value +:. 1 in
  let ncol7 = cur_col.value +:. 1 in
  let nvalid7 = row_lt_last &: col_lt_last in

  let nrow = mux neigh_idx.value [ nrow0; nrow1; nrow2; nrow3; nrow4; nrow5; nrow6; nrow7 ] in
  let ncol = mux neigh_idx.value [ ncol0; ncol1; ncol2; ncol3; ncol4; ncol5; ncol6; ncol7 ] in
  let nvalid =
    mux neigh_idx.value [ nvalid0; nvalid1; nvalid2; nvalid3; nvalid4; nvalid5; nvalid6; nvalid7 ]
  in
  let addr_neigh = addr_of_row_col nrow ncol in

  (* Select which node address we're currently reading. *)
  node_rd_addr_w <==
    mux2 peel_neighbors addr_neigh (mux2 peel_pop addr_pop (zero addr_width));

  (* Write logic (priority by phase). *)
  let load_write = load &: part2 &: valid_center in
  let load_enq = load_write &: accessible in

  let queue_empty = q_rptr.value ==: q_wptr.value in
  let pop_has_item = peel_pop &: ~:queue_empty in
  let pop_remove = pop_has_item &: (node_val <:. 4) in

  let neigh_update = peel_neighbors &: nvalid &: node_alive in
  let neigh_enq = neigh_update &: (node_val ==:. 4) in

  (* Node memory write ports. *)
  node_we_w <== (load_write |: pop_remove |: neigh_update);
  node_wr_addr_w <==
    mux2 load_write cell_addr.value (mux2 peel_neighbors addr_neigh addr_pop);

  let dec_val = mux2 (node_val ==:. 0) (zero 4) (node_val -:. 1) in
  node_wr_data_w <==
    mux2
      load_write
      node_data_init
      (mux2 peel_neighbors dec_val dead4);

  (* Queue write ports. *)
  q_we_w <== (load_enq |: neigh_enq);
  q_wr_data_w <== mux2 load_enq q_data_load (concat_msb [ nrow; ncol ]);

  let done_load = step &: (r.value ==: r_last) &: (c.value ==: c_last) in

  compile
    [ sm.switch
        [ ( Idle
          , [ when_
                start
                [ r <-- r0
                ; c <-- c0
                ; cell_addr <-- zero addr_width
                ; part1_count <-- zero 32
                ; q_rptr <-- zero addr_width
                ; q_wptr <-- zero addr_width
                ; removed <-- zero 32
                ; cur_row <-- zero row_bits
                ; cur_col <-- zero col_bits
                ; neigh_idx <-- zero 3
                ; result <-- zero 32
                ; sm.set_next Load
                ]
            ] )
        ; ( Load
          , [ (* Advance scan counters. *)
              when_ (step &: c_is_last) [ c <-- c0; r <-- r.value +: one_coord ]
            ; when_ (step &: ~:c_is_last) [ c <-- c.value +: one_coord ]
            ; (* When we reach a valid center, either count part1 or populate part2 structures. *)
              when_
                (valid_center &: ~:part2)
                [ part1_count <-- part1_count.value +: (zero 31 @: accessible) ]
            ; when_ load_write [ cell_addr <-- cell_addr.value +: one_addr ]
            ; when_ load_enq [ q_wptr <-- q_wptr.value +: one_addr ]
            ; when_
                done_load
                [ if_
                    part2
                    [ q_rptr <-- zero addr_width
                    ; removed <-- zero 32
                    ; sm.set_next Peel_pop
                    ]
                    [ result <-- part1_count.value; sm.set_next Done ]
                ]
            ] )
        ; ( Peel_pop
          , [ if_
                queue_empty
                [ result <-- removed.value; sm.set_next Done ]
                [ (* Pop one queue entry per cycle. *)
                  q_rptr <-- q_rptr.value +: one_addr
                ; when_
                    pop_remove
                    [ removed <-- removed.value +:. 1
                    ; cur_row <-- q_row
                    ; cur_col <-- q_col
                    ; neigh_idx <-- zero 3
                    ; sm.set_next Peel_neighbors
                    ]
                ]
            ] )
        ; ( Peel_neighbors
          , [ when_ neigh_enq [ q_wptr <-- q_wptr.value +: one_addr ]
            ; if_
                (neigh_idx.value ==:. 7)
                [ neigh_idx <-- zero 3; sm.set_next Peel_pop ]
                [ neigh_idx <-- neigh_idx.value +:. 1 ]
            ] )
        ; ( Done
          , [ (* one-cycle pulse *)
              sm.set_next Idle
            ] )
        ]
    ];

  { in_ready
  ; out_valid = sm.is Done
  ; out_count = result.value
  }
;;

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"day04" create
;;



