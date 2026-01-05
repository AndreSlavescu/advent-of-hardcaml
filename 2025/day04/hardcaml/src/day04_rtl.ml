open! Core
open! Hardcaml
open! Signal

(* Day 4 Part 2: Optimized k-core peeling with parallel neighbor reads.

   Optimization: Use 9 read ports to read center + all 8 neighbors in one cycle,
   then process writes sequentially. This reduces the peel phase from ~9 cycles
   per removal to ~3 cycles (pop + read + write loop with early termination).
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
    | Peel_fetch  (* Wait for node memory read *)
    | Peel_check  (* Check center, cache neighbors *)
    | Peel_write
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
          , [ sm.set_next Idle
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

  let%hw_var r = Variable.reg spec ~width:coord_width in
  let%hw_var c = Variable.reg spec ~width:coord_width in
  let%hw_var cell_addr = Variable.reg spec ~width:addr_width in
  let%hw_var part1_count = Variable.reg spec ~width:32 in

  let%hw_var q_rptr = Variable.reg spec ~width:addr_width in
  let%hw_var q_wptr = Variable.reg spec ~width:addr_width in
  let%hw_var removed = Variable.reg spec ~width:32 in

  let%hw_var cur_row = Variable.reg spec ~width:row_bits in
  let%hw_var cur_col = Variable.reg spec ~width:col_bits in
  let%hw_var write_idx = Variable.reg spec ~width:4 in

  let%hw_var result = Variable.reg spec ~width:32 in

  (* Cached neighbor values after parallel read *)
  let cached_vals = Array.init 8 ~f:(fun _ -> Variable.reg spec ~width:4) in
  let cached_addrs = Array.init 8 ~f:(fun _ -> Variable.reg spec ~width:addr_width) in
  let cached_valid = Array.init 8 ~f:(fun _ -> Variable.reg spec ~width:1) in
  let cached_rows = Array.init 8 ~f:(fun _ -> Variable.reg spec ~width:row_bits) in
  let cached_cols = Array.init 8 ~f:(fun _ -> Variable.reg spec ~width:col_bits) in

  let load = sm.is Load in
  let _peel_pop = sm.is Peel_pop in
  let _peel_fetch = sm.is Peel_fetch in
  let peel_check = sm.is Peel_check in
  let peel_write = sm.is Peel_write in

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

  let addr_of_row_col row col =
    let row = uresize row addr_width in
    let col = uresize col addr_width in
    let w = of_int ~width:addr_width width in
    uresize (row *: w) addr_width +: col
  in

  let need_input_raw =
    (r.value >=: r1) &: (r.value <=: r_h) &: (c.value >=: c1) &: (c.value <=: c_w)
  in
  let in_ready = load &: need_input_raw in
  let step = load &: mux2 need_input_raw in_valid vdd in

  let valid_center_raw =
    (r.value >=: r2) &: (r.value <=: r_h1) &: (c.value >=: c2) &: (c.value <=: c_w1)
  in
  let valid_center = step &: valid_center_raw in

  let at_code = of_int ~width:8 (Char.to_int '@') in
  let pix = load &: need_input_raw &: in_valid &: (in_char ==: at_code) in

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

  let dead4 = of_int ~width:4 15 in
  let node_data_init = mux2 mid_1 deg4 dead4 in

  let row_center = uresize (r.value -:. 2) row_bits in
  let col_center = uresize (c.value -:. 2) col_bits in
  let q_data_load = concat_msb [ row_center; col_center ] in

  (* Queue memory *)
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

  let q_row = select q_rd (queue_bits - 1) col_bits in
  let q_col = select q_rd (col_bits - 1) 0 in

  (* Compute all 8 neighbor coordinates *)
  let row_gt0 = cur_row.value >:. 0 in
  let row_lt_last = cur_row.value <:. (height - 1) in
  let col_gt0 = cur_col.value >:. 0 in
  let col_lt_last = cur_col.value <:. (width - 1) in

  let neighbor_info = [|
    (cur_row.value -:. 1, cur_col.value -:. 1, row_gt0 &: col_gt0);
    (cur_row.value -:. 1, cur_col.value, row_gt0);
    (cur_row.value -:. 1, cur_col.value +:. 1, row_gt0 &: col_lt_last);
    (cur_row.value, cur_col.value -:. 1, col_gt0);
    (cur_row.value, cur_col.value +:. 1, col_lt_last);
    (cur_row.value +:. 1, cur_col.value -:. 1, row_lt_last &: col_gt0);
    (cur_row.value +:. 1, cur_col.value, row_lt_last);
    (cur_row.value +:. 1, cur_col.value +:. 1, row_lt_last &: col_lt_last);
  |] in

  let neighbor_addrs = Array.map neighbor_info ~f:(fun (nr, nc, _) -> addr_of_row_col nr nc) in

  (* Node memory with 9 read ports: 1 for center, 8 for neighbors *)
  let node_we_w = wire 1 in
  let node_wr_addr_w = wire addr_width in
  let node_wr_data_w = wire 4 in

  let center_addr = addr_of_row_col cur_row.value cur_col.value in
  let load_addr = addr_of_row_col row_center col_center in

  (* Read addresses: during load use load_addr, during peel_pop use center, during peel_read use neighbors *)
  let read_addrs =
    Array.init 9 ~f:(fun i ->
      if i = 0 then
        mux2 load load_addr center_addr
      else
        mux2 load load_addr neighbor_addrs.(i - 1))
  in

  let node_rds =
    multiport_memory
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
      ~read_addresses:read_addrs
  in

  let center_val = node_rds.(0) in
  let neighbor_vals = Array.sub node_rds ~pos:1 ~len:8 in

  let queue_empty = q_rptr.value ==: q_wptr.value in
  let center_alive = center_val <>:. 15 in
  let center_should_remove = center_alive &: (center_val <:. 4) in

  (* Current write target from cache *)
  let cur_cached_val = mux write_idx.value (Array.to_list (Array.map cached_vals ~f:(fun v -> v.value))) in
  let cur_cached_addr = mux write_idx.value (Array.to_list (Array.map cached_addrs ~f:(fun v -> v.value))) in
  let cur_cached_valid = mux write_idx.value (Array.to_list (Array.map cached_valid ~f:(fun v -> v.value))) in
  let cur_cached_row = mux write_idx.value (Array.to_list (Array.map cached_rows ~f:(fun v -> v.value))) in
  let cur_cached_col = mux write_idx.value (Array.to_list (Array.map cached_cols ~f:(fun v -> v.value))) in

  let valid_write_idx = write_idx.value <:. 8 in
  let cur_alive = cur_cached_val <>:. 15 in
  let cur_should_write = valid_write_idx &: cur_cached_valid &: cur_alive in
  let cur_should_enq = cur_should_write &: (cur_cached_val ==:. 4) in
  let cur_new_val = mux2 (cur_cached_val ==:. 0) (zero 4) (cur_cached_val -:. 1) in

  (* Load phase writes *)
  let load_write = load &: part2 &: valid_center in
  let load_enq = load_write &: accessible in

  (* Mark center as dead during peel_check *)
  let center_write = peel_check &: center_should_remove in

  (* Write logic: load writes, center death write, or neighbor updates *)
  node_we_w <== (load_write |: center_write |: (peel_write &: cur_should_write));
  node_wr_addr_w <== mux2 load_write load_addr (mux2 center_write center_addr cur_cached_addr);
  node_wr_data_w <== mux2 load_write node_data_init (mux2 center_write dead4 cur_new_val);

  (* Queue writes *)
  q_we_w <== (load_enq |: (peel_write &: cur_should_enq));
  q_wr_data_w <== mux2 load_enq q_data_load (concat_msb [ cur_cached_row; cur_cached_col ]);

  let done_load = step &: (r.value ==: r_last) &: (c.value ==: c_last) in
  let write_done = write_idx.value >=:. 8 in

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
                ; write_idx <-- zero 4
                ; result <-- zero 32
                ; sm.set_next Load
                ]
            ] )
        ; ( Load
          , [ when_ (step &: c_is_last) [ c <-- c0; r <-- r.value +: one_coord ]
            ; when_ (step &: ~:c_is_last) [ c <-- c.value +: one_coord ]
            ; when_
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
                [ q_rptr <-- q_rptr.value +: one_addr
                ; cur_row <-- q_row
                ; cur_col <-- q_col
                ; sm.set_next Peel_fetch  (* Wait for memory read *)
                ]
            ] )
        ; ( Peel_fetch
          , [ (* Just wait for node memory reads to complete *)
              sm.set_next Peel_check
            ] )
        ; ( Peel_check
          , [ (* Memory outputs now valid - cache all neighbor values *)
              proc (List.concat (Array.to_list (Array.mapi neighbor_info ~f:(fun i (nr, nc, valid) ->
                [ cached_vals.(i) <-- neighbor_vals.(i)
                ; cached_addrs.(i) <-- neighbor_addrs.(i)
                ; cached_valid.(i) <-- uresize valid 1
                ; cached_rows.(i) <-- nr
                ; cached_cols.(i) <-- nc
                ]))))
            ; if_
                center_should_remove
                [ removed <-- removed.value +:. 1
                ; write_idx <-- zero 4
                ; sm.set_next Peel_write
                ]
                [ sm.set_next Peel_pop ]
            ] )
        ; ( Peel_write
          , [ when_ cur_should_enq [ q_wptr <-- q_wptr.value +: one_addr ]
            ; write_idx <-- write_idx.value +:. 1
            ; when_ write_done [ sm.set_next Peel_pop ]
            ] )
        ; ( Done
          , [ sm.set_next Idle
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



