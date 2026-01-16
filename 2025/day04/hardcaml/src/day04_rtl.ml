open! Core
open! Hardcaml
open! Signal

(* Day 4 of Advent of Code

   This hardcaml module implements part 1 and part 2 of day 4 which both involve a
   a 2D grid where '@' characters represent nodes and connectivity is defined by
   the Moore neighborhood. Visit this informational wikipedia link for more information
   on the specifics of what a Moore neighborhood looks like:

   https://en.wikipedia.org/wiki/Moore_neighborhood

   
   Part 1 Day 4:

   Problem Statement:
   Count cells that are "@" and have fewer than 4 neighbors.

   Algorithm:
   This is a pure streaming solution, no random access memory needed. We use
   a 2D convolution approach with shift registers to maintain a sliding 3 by 3
   window over the input stream. This is essentially what is needed by the Moore
   neighborhood structure.

   Hardware Design:
   The design uses a chain of shift registers to buffer exactly enough data
   to access a 3 by 3 neighborhood:

   1. Input arrives one character per cycle in raster order (row by row)

   2. We maintain 3 rows of data using delay chains:
      - bot (bottom row): current input
      - mid (middle row): input from "width" cycles ago
      - top (top row): input from "2 * width" cycles ago

   3. Within each row, we keep 3 columns using additional delays:
      - _0 (rightmost): current value
      - _1 (middle): 1 cycle delayed
      - _2 (leftmost): 2 cycles delayed

   3x3 Window Mapping:
   When we are at position (r, c) in the input stream, the shift registers
   give us access to a 3 by 3 window centered at (r-1, c-1). Below is a simple text
   diagram that shows this:

                 col c-2 col c-1 col c
                _______________________
       row r-2  | top_2 │ top_1 │ top │
                |_______|_______|_____|
       row r-1  │ mid_2 │ mid_1 │ mid │
                |_______|_______|_____|
       row r    │ bot_2 │ bot_1 │ bot │
                |_______|_______|_____|

   The 8 neighbors of mid_1 are as shown in the above diagram. That is the visual
   representation of the Moore neighborhood.

   Padding Strategy:
   The input grid is conceptually padded with a 1-cell border of zeros:

   - padded dimensions: (width + 2) * (height + 2)
   - actual input only covers the interior region [1 ... height][1 ... width]
   - padding cells are implicitly 0, not "@"

   This eliminates boundary checking. This is because every interior cell has all 8 neighbors.

   Data Flow per Cycle:
   1. If in interior region and in_valid: read input character

   2. Convert to 1 bit: 
       pix = (char == '@')

   3. Shift pix into the delay chain -> all registers advance

   4. Compute degree = popcount(8 neighbors)

   5. If valid output position:
       count += (mid_1 && degree < 4)

   Timing:
   - Latency: 2 * padded_width + 2 cycles from first input to first valid output
   - Throughput: 1 cell per cycle which is the same as the input rate
   - Total cycles: padded_width * padded_height

   Resource Usage:
   - Shift registers: 2 * padded_width + 6 flip-flops for the delay chains
   - Combinational: 8 input popcount tree, comparators

   

   Part 2 Day 4: K-Core Peeling Algorithm
   
   Problem:
   Given a 2D grid where "@" characters represent nodes and each cell's "degree"
   is the count of its 8 neighbors, we iteratively remove nodes with degree < 4 
   until no more can be removed. The answer is the count of removed nodes.

   Algorithm:
   1. Load Phase: Stream in the grid character by character, computing initial
      degrees for each "@" cell using a 3 row sliding window of shift registers.
      Cells with degree < 4 are immediately enqueued for removal.

   2. Peel Phase: Process the queue using Breadth First Search (BFS) style traversal:
      - Pop a node from queue
      - If still alive and degree < 4: 
         mark as dead, decrement all neighbors
      - Any neighbor whose degree drops to exactly 4 gets enqueued
      - Repeat until queue is empty

   Hardware Optimization:
   The key optimization is using 9 parallel memory read ports, to cover the full stencil:
   - 1 port for reading the center node being processed
   - 8 ports for reading all neighbors at the same time

   This allows us to read center and all 8 neighbors in a single cycle, then
   process the writes sequentially. This reduces the peel phase from ~9 cycles
   per removal, bottlenecked by sequential reads, to ~3 cycles (pop + parallel read + write loop).

   Memory Organization:
   1. Node Memory (size * 4 bits):
      - Stores the current degree of each cell
      - Value 0 to 14: cell is alive with that many neighbors
      - Value 15: cell is dead (either not "@" charactor or already removed)
      - Has 9 read ports (1 center + 8 neighbors) for parallel access
      - Has 1 write port (writes are sequential)

   2. Queue Memory (size * queue_bits):
      - First In First Out (FIFO) queue for BFS traversal
      - Stores packed (row, col) coordinates
      - Simple pointer based circular buffer

   Data Flow:
   Load Phase:
     for each cell in raster order:
       1. Read input character
       2. Compute initial degree using sliding window
       3. Write degree to node memory
       4. If degree < 4, enqueue for removal

   Peel Phase:
     while queue not empty:
       1. Pop (row, col) from queue
       2. Read center + all 8 neighbors in parallel, which is 1 cycle
       3. Wait for memory, which is 1 cycle
       4. If center alive and degree < 4:
          a. Mark center as dead -> write 15
          b. Cache all neighbor values/addresses
          c. For each valid neighbor:
             - Decrement its degree
             - If new degree == 4, enqueue it
       5. Continue to next queue entry *)

(* Configuration: Grid Dimensions
   Grid dimensions can be overridden via environment variables for different
   puzzle inputs. The default 137x137 matches the given AoC Day 4 input size.
   Refer to `2025/day04/day4_input.txt` for the input sample given by AoC.
   
   For scale testing, we scale the input by 5 times in both the h and w dimension,
   as well as 10 times in both the h and w dimension. For more details on the scale
   testing format and code, refer to `2025/day04/scale_test.py`. *)

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

module Part1_states = struct
  type t =
    | Idle
    | Running
    | Done
  [@@deriving sexp_of, compare ~localize, enumerate]
end

module States = struct
  type t =
    | Idle
    | Load
    | Peel_pop
    | Peel_fetch
    | Peel_check
    | Peel_write
    | Done
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let create_part1 scope ({ clock; clear; start; part2 = _; in_valid; in_char } : _ I.t)
  : _ O.t
  =
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
    r.value >=: r2 &: (r.value <=: r_h1) &: (c.value >=: c2) &: (c.value <=: c_w1)
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
          , [ when_ start [ r <-- r0; c <-- c0; count <-- zero 32; sm.set_next Running ] ]
          )
        ; ( Running
          , [ when_ valid_center [ count <-- count.value +: (zero 31 @: accessible) ]
            ; when_ (step &: c_is_last) [ c <-- c0; r <-- r.value +: one_coord ]
            ; when_ (step &: ~:c_is_last) [ c <-- c.value +: one_coord ]
            ; when_ done_now [ sm.set_next Done ]
            ] )
        ; Done, [ sm.set_next Idle ]
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
  let%hw_var part1_count = Variable.reg spec ~width:32 in
  let%hw_var q_rptr = Variable.reg spec ~width:addr_width in
  let%hw_var q_wptr = Variable.reg spec ~width:addr_width in
  let%hw_var removed = Variable.reg spec ~width:32 in
  let%hw_var cur_row = Variable.reg spec ~width:row_bits in
  let%hw_var cur_col = Variable.reg spec ~width:col_bits in
  let%hw_var write_idx = Variable.reg spec ~width:4 in
  let%hw_var result = Variable.reg spec ~width:32 in
  let cached_vals = Array.init 8 ~f:(fun _ -> Variable.reg spec ~width:4) in
  let cached_addrs = Array.init 8 ~f:(fun _ -> Variable.reg spec ~width:addr_width) in
  let cached_valid = Array.init 8 ~f:(fun _ -> Variable.reg spec ~width:1) in
  let cached_rows = Array.init 8 ~f:(fun _ -> Variable.reg spec ~width:row_bits) in
  let cached_cols = Array.init 8 ~f:(fun _ -> Variable.reg spec ~width:col_bits) in
  let load = sm.is Load in
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
    r.value >=: r1 &: (r.value <=: r_h) &: (c.value >=: c1) &: (c.value <=: c_w)
  in
  let in_ready = load &: need_input_raw in
  let step = load &: mux2 need_input_raw in_valid vdd in
  let valid_center_raw =
    r.value >=: r2 &: (r.value <=: r_h1) &: (c.value >=: c2) &: (c.value <=: c_w1)
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
  let row_gt0 = cur_row.value >:. 0 in
  let row_lt_last = cur_row.value <:. height - 1 in
  let col_gt0 = cur_col.value >:. 0 in
  let col_lt_last = cur_col.value <:. width - 1 in
  let neighbor_info =
    [| cur_row.value -:. 1, cur_col.value -:. 1, row_gt0 &: col_gt0
     ; cur_row.value -:. 1, cur_col.value, row_gt0
     ; cur_row.value -:. 1, cur_col.value +:. 1, row_gt0 &: col_lt_last
     ; cur_row.value, cur_col.value -:. 1, col_gt0
     ; cur_row.value, cur_col.value +:. 1, col_lt_last
     ; cur_row.value +:. 1, cur_col.value -:. 1, row_lt_last &: col_gt0
     ; cur_row.value +:. 1, cur_col.value, row_lt_last
     ; cur_row.value +:. 1, cur_col.value +:. 1, row_lt_last &: col_lt_last
    |]
  in
  let neighbor_addrs =
    Array.map neighbor_info ~f:(fun (nr, nc, _) -> addr_of_row_col nr nc)
  in
  let node_we_w = wire 1 in
  let node_wr_addr_w = wire addr_width in
  let node_wr_data_w = wire 4 in
  let center_addr = addr_of_row_col cur_row.value cur_col.value in
  let load_addr = addr_of_row_col row_center col_center in
  let read_addrs =
    Array.init 9 ~f:(fun i ->
      if i = 0
      then mux2 load load_addr center_addr
      else mux2 load load_addr neighbor_addrs.(i - 1))
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
  let cur_cached_val =
    mux write_idx.value (Array.to_list (Array.map cached_vals ~f:(fun v -> v.value)))
  in
  let cur_cached_addr =
    mux write_idx.value (Array.to_list (Array.map cached_addrs ~f:(fun v -> v.value)))
  in
  let cur_cached_valid =
    mux write_idx.value (Array.to_list (Array.map cached_valid ~f:(fun v -> v.value)))
  in
  let cur_cached_row =
    mux write_idx.value (Array.to_list (Array.map cached_rows ~f:(fun v -> v.value)))
  in
  let cur_cached_col =
    mux write_idx.value (Array.to_list (Array.map cached_cols ~f:(fun v -> v.value)))
  in
  let valid_write_idx = write_idx.value <:. 8 in
  let cur_alive = cur_cached_val <>:. 15 in
  let cur_should_write = valid_write_idx &: cur_cached_valid &: cur_alive in
  let cur_should_enq = cur_should_write &: (cur_cached_val ==:. 4) in
  let cur_new_val = mux2 (cur_cached_val ==:. 0) (zero 4) (cur_cached_val -:. 1) in
  let load_write = load &: part2 &: valid_center in
  let load_enq = load_write &: accessible in
  let center_write = peel_check &: center_should_remove in
  node_we_w <== (load_write |: center_write |: (peel_write &: cur_should_write));
  node_wr_addr_w
  <== mux2 load_write load_addr (mux2 center_write center_addr cur_cached_addr);
  node_wr_data_w <== mux2 load_write node_data_init (mux2 center_write dead4 cur_new_val);
  q_we_w <== (load_enq |: (peel_write &: cur_should_enq));
  q_wr_data_w
  <== mux2 load_enq q_data_load (concat_msb [ cur_cached_row; cur_cached_col ]);
  let done_load = step &: (r.value ==: r_last) &: (c.value ==: c_last) in
  let write_done = write_idx.value >=:. 8 in
  compile
    [ sm.switch
        [ ( Idle
          , [ when_
                start
                [ r <-- r0
                ; c <-- c0
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
                ; sm.set_next Peel_fetch
                ]
            ] )
        ; Peel_fetch, [ sm.set_next Peel_check ]
        ; ( Peel_check
          , [ proc
                (List.concat
                   (Array.to_list
                      (Array.mapi neighbor_info ~f:(fun i (nr, nc, valid) ->
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
        ; Done, [ sm.set_next Idle ]
        ]
    ];
  { in_ready; out_valid = sm.is Done; out_count = result.value }
;;

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"day04" create
;;
