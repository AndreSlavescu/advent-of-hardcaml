/*
 * Advent of HardCaml 2025 Day 4 TinyTapeout
 *
 * Pin mapping:
 *   ui_in[7:0]   -> in_char which is ASCII input
 *   uio_in[0]    -> part2 select
 *   uio_in[1]    -> in_valid, data valid strobe
 *   uio_in[2]    -> start, begin processing
 *   uio_in[3]    -> read_result, shift out result byte
 *   rst_n        -> clear, active low reset
 *   clk          -> clock
 *
 *   uo_out[0]    -> in_ready
 *   uo_out[1]    -> out_valid
 *   uo_out[7:2]  -> result_byte[5:0], lower 6 bits of current result byte
 *   uio_out[1:0] -> result_byte[7:6], upper 2 bits of current result byte
 *   uio_out[7:2] -> unused
*/

module tt_um_day04 (
    input  wire [7:0] ui_in,
    output wire [7:0] uo_out,
    input  wire [7:0] uio_in,
    output wire [7:0] uio_out,
    output wire [7:0] uio_oe,
    input  wire       ena,
    input  wire       clk,
    input  wire       rst_n
);

    wire        in_ready;
    wire        out_valid;
    wire [31:0] out_count;

    wire part2     = uio_in[0];
    wire in_valid  = uio_in[1];
    wire start     = uio_in[2];
    wire read_next = uio_in[3];

    reg [31:0] result_shift;
    reg [1:0]  byte_idx;

    always @(posedge clk) begin
        if (!rst_n) begin
            result_shift <= 32'b0;
            byte_idx <= 2'b0;
        end else if (out_valid) begin
            result_shift <= out_count;
            byte_idx <= 2'b0;
        end else if (read_next) begin
            result_shift <= {8'b0, result_shift[31:8]};
            byte_idx <= byte_idx + 1;
        end
    end

    day04 core (
        .in_char   (ui_in),
        .part2     (part2),
        .in_valid  (in_valid),
        .clear     (~rst_n),
        .clock     (clk),
        .start     (start),
        .in_ready  (in_ready),
        .out_valid (out_valid),
        .out_count (out_count)
    );

    assign uo_out[0]   = in_ready;
    assign uo_out[1]   = out_valid;
    assign uo_out[7:2] = result_shift[5:0];

    assign uio_out[1:0] = result_shift[7:6];
    assign uio_out[7:2] = 6'b0;
    assign uio_oe = 8'b0000_0011;

endmodule
