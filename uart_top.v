`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date:    22:02:02 11/13/2022 
// Design Name: 
// Module Name:    uart_top 
// Project Name: 
// Target Devices: 
// Tool versions: 
// Description: 
//
// Dependencies: 
//
// Revision: 
// Revision 0.01 - File Created
// Additional Comments: 
//
//////////////////////////////////////////////////////////////////////////////////

`define IDX(x) (((x)+1)*(32)-1):((x)*(32))

module uart_tx 
  #(parameter CLKS_PER_BIT = 50)
  (
   input       i_Rst_L,
   input       i_Clock,
   input       i_TX_DV,
   input [7:0] i_TX_Byte, 
   output reg  o_TX_Active,
   output reg  o_TX_Serial,
   output reg  o_TX_Done
   );
 
  localparam IDLE         = 2'b00;
  localparam TX_START_BIT = 2'b01;
  localparam TX_DATA_BITS = 2'b10;
  localparam TX_STOP_BIT  = 2'b11;
  
  reg [2:0] r_SM_Main;
  reg [12:0] r_Clock_Count;
  reg [2:0] r_Bit_Index;
  reg [7:0] r_TX_Data;


  // Purpose: Control TX state machine
  always @(posedge i_Clock or negedge i_Rst_L)
  begin
    if (~i_Rst_L)
    begin
      r_SM_Main <= 3'b000;
    end
    else
    begin

      o_TX_Done <= 1'b0;

      case (r_SM_Main)
      IDLE :
        begin
          o_TX_Serial   <= 1'b1;         // Drive Line High for Idle
          r_Clock_Count <= 0;
          r_Bit_Index   <= 0;
          
          if (i_TX_DV == 1'b1)
          begin
            o_TX_Active <= 1'b1;
            r_TX_Data   <= i_TX_Byte;
            r_SM_Main   <= TX_START_BIT;
          end
          else
            r_SM_Main <= IDLE;
        end // case: IDLE
      
      
      // Send out Start Bit. Start bit = 0
      TX_START_BIT :
        begin
          o_TX_Serial <= 1'b0;
          
          // Wait CLKS_PER_BIT-1 clock cycles for start bit to finish
          if (r_Clock_Count < CLKS_PER_BIT-1)
          begin
            r_Clock_Count <= r_Clock_Count + 1'b1;
            r_SM_Main     <= TX_START_BIT;
          end
          else
          begin
            r_Clock_Count <= 0;
            r_SM_Main     <= TX_DATA_BITS;
          end
        end // case: TX_START_BIT
      
      
      // Wait CLKS_PER_BIT-1 clock cycles for data bits to finish         
      TX_DATA_BITS :
        begin
          o_TX_Serial <= r_TX_Data[r_Bit_Index];
          
          if (r_Clock_Count < CLKS_PER_BIT-1)
          begin
            r_Clock_Count <= r_Clock_Count + 1'b1;
            r_SM_Main     <= TX_DATA_BITS;
          end
          else
          begin
            r_Clock_Count <= 0;
            
            // Check if we have sent out all bits
            if (r_Bit_Index < 7)
            begin
              r_Bit_Index <= r_Bit_Index + 1'b1;
              r_SM_Main   <= TX_DATA_BITS;
            end
            else
            begin
              r_Bit_Index <= 0;
              r_SM_Main   <= TX_STOP_BIT;
            end
          end 
        end // case: TX_DATA_BITS
      
      
      // Send out Stop bit.  Stop bit = 1
      TX_STOP_BIT :
        begin
          o_TX_Serial <= 1'b1;
          
          // Wait CLKS_PER_BIT-1 clock cycles for Stop bit to finish
          if (r_Clock_Count < CLKS_PER_BIT-1)
          begin
            r_Clock_Count <= r_Clock_Count + 1'b1;
            r_SM_Main     <= TX_STOP_BIT;
          end
          else
          begin
            o_TX_Done     <= 1'b1;
            r_Clock_Count <= 0;
            r_SM_Main     <= IDLE;
            o_TX_Active   <= 1'b0;
          end 
        end // case: TX_STOP_BIT      
      
      default :
        r_SM_Main <= IDLE;
      
    endcase
    end // else: !if(~i_Rst_L)
  end // always @ (posedge i_Clock or negedge i_Rst_L)

  
endmodule

module uart_rx
		#(parameter CLOCKS_PER_BIT = 50)
		(
			input 		i_Clock,
			input 		i_RX_Serial,
			output		o_RX_DV,
			output[7:0] o_RX_Byte
		);
		
		parameter IDLE = 3'b000;
		parameter RX_START_BIT = 3'b001;
		parameter RX_DATA_BITS = 3'b010;
		parameter RX_STOP_BIT = 3'b011;
		parameter CLEANUP = 3'b100;
		
		reg[12:0] r_Clock_Count;
		reg[2:0] r_Bit_Index;
		reg[7:0] r_RX_Byte;
		reg r_RX_DV;
		reg[2:0] r_SM_Main;
		
		always @(posedge i_Clock)
		begin
			case (r_SM_Main)
				IDLE:
					begin
						r_RX_DV <= 1'b0;
						r_Clock_Count <= 0;
						r_Bit_Index <= 0;
						
						if (i_RX_Serial == 1'b0)
							r_SM_Main <= RX_START_BIT;
						else
							r_SM_Main <= IDLE;
					end
					
				RX_START_BIT:
					begin
						if (r_Clock_Count == CLOCKS_PER_BIT / 2)
						begin
							if (i_RX_Serial == 1'b0)
							begin
								r_Clock_Count <= 0;
								r_SM_Main <= RX_DATA_BITS;
							end
							else
								r_SM_Main <= IDLE;
						end
						else
						begin
							r_Clock_Count <= r_Clock_Count + 1'b1;
							r_SM_Main <= RX_START_BIT;
						end
					end
					
				RX_DATA_BITS:
					begin
						if(r_Clock_Count < CLOCKS_PER_BIT - 1)
						begin
							r_Clock_Count <= r_Clock_Count + 1'b1;
							r_SM_Main <= RX_DATA_BITS;
						end
						else
						begin
							r_Clock_Count <= 0;
							r_RX_Byte[r_Bit_Index] <= i_RX_Serial;
							
							if (r_Bit_Index < 7)
							begin
								r_Bit_Index <= r_Bit_Index + 1'b1;
								r_SM_Main <= RX_DATA_BITS;
							end
							else
							begin
								r_Bit_Index <= 0;
								r_SM_Main <= RX_STOP_BIT;
							end
						end
					end
					
				RX_STOP_BIT:
					begin
						if(r_Clock_Count < CLOCKS_PER_BIT - 1)
						begin
							r_Clock_Count <= r_Clock_Count + 1'b1;
							r_SM_Main <= RX_STOP_BIT;
						end
						else
						begin
							r_RX_DV <= 1'b1;
							r_Clock_Count <= 0;
							r_SM_Main <= CLEANUP;
						end
					end
				
				CLEANUP:
					begin
						r_SM_Main <= IDLE;
						r_RX_DV <= 1'b0;
					end
					
				default:
					r_SM_Main <= IDLE;
					
			endcase
		end
		
		assign o_RX_Byte = r_RX_Byte;
		assign o_RX_DV = r_RX_DV;
endmodule

/*module sevensegment_display(
	input i_Clock,
	input[7:0] word,
	
	output[2:0] SevenSegmentEnable,
	output[7:0] SevenSegment
);
	
	reg [20:0] enable_d;
	reg [2:0] r_SevenSegmentEnable;
	reg [7:0] r_SevenSegment;
	reg [3:0] msg;
	
	assign SevenSegmentEnable = r_SevenSegmentEnable;
	assign SevenSegment = r_SevenSegment;

	always @(posedge i_Clock) begin
	
		if (enable_d[20])
		begin
			msg <= word[3:0];
			r_SevenSegmentEnable <= 3'b110;
		end
		else
		begin
			msg <= word[7:4];
			r_SevenSegmentEnable <= 3'b101;
		end
		
		enable_d <= enable_d + 1'b1;
		
		case (msg)
			0 : r_SevenSegment <= 8'b00000011;
			1 : r_SevenSegment <= 8'b10011111;
			2 : r_SevenSegment <= 8'b00100101;
			3 : r_SevenSegment <= 8'b00001101;
			4 : r_SevenSegment <= 8'b10011001;
			5 : r_SevenSegment <= 8'b01001001;
			6 : r_SevenSegment <= 8'b01000001;
			7 : r_SevenSegment <= 8'b00011111;
			8 : r_SevenSegment <= 8'b00000001;
			9 : r_SevenSegment <= 8'b00011001;
			10 : r_SevenSegment <= 8'b00010001;
			11 : r_SevenSegment <= 8'b11000001;
			12 : r_SevenSegment <= 8'b01100011;
			13 : r_SevenSegment <= 8'b10000101;
			14 : r_SevenSegment <= 8'b01100001;
			15 : r_SevenSegment <= 8'b01110001;
			default : r_SevenSegment <= 8'b11111111;
		endcase
	end
endmodule*/

module serial_receive(clk, RxD, midstate, data2);
   input      clk;
   input      RxD;
   
   wire       RxD_data_ready;
   wire [7:0] RxD_data;

   uart_rx deserializer(.i_Clock(clk), .i_RX_Serial(RxD),
		.o_RX_DV(RxD_data_ready), .o_RX_Byte(RxD_Data));

   output [255:0] midstate;
   output [255:0] data2;
   
   // 256 bits midstate + 256 bits data at the same time = 64 bytes

   // Might be a good idea to add some fixed start and stop sequences,
   // so we really know we got all the data and nothing more. If a
   // test for these fails, should ask for new data, so it needs more
   // logic on the return side too. The check bits could be legible
   // 7seg for quick feedback :)
   
   reg [511:0] input_buffer;
   reg [511:0] input_copy = 512'h000000008a4ca094d0b15296fce5a5baf216ca85dba879b20f6243997b81df0706dcee58604823e867bf6410b703f459;
   reg [6:0]   demux_state;

   assign midstate = input_copy[511:256];
   assign data2 = input_copy[255:0];
   
   // we probably don't need a busy signal here, just read the latest
   // complete input that is available.
   
   /*always @(posedge clk)
     case (demux_state)
       7'b1000000:
	 begin
	    input_copy <= input_buffer;
	    demux_state <= 0;
	 end
       
       default:
	 if(RxD_data_ready)
	  begin
	      input_buffer <= input_buffer << 8;
	      input_buffer[7:0] <= RxD_data;
	      demux_state <= demux_state + 1;
	   end
     endcase*/ // case (demux_state)
   
endmodule // serial_receive

module serial_transmit (clk, TxD, busy, send, word);
   
   // split 4-byte output into bytes

   wire TxD_start;
   wire TxD_busy;
   
   reg [7:0]  out_byte;
   reg        serial_start;
   reg [3:0]  mux_state;

   assign TxD_start = serial_start;

   input      clk;
   output     TxD;
   
   input [31:0] word;
   input 	send;
   output 	busy;

   reg [31:0] 	word_copy;
   
   assign busy = (|mux_state);

   always @(posedge clk)
     begin
	
	case (mux_state)
	  4'b0000:
	    if (send)
	      begin
		 mux_state <= 4'b1000;
		 word_copy <= word;
	      end  
	  4'b1000: out_byte <= word_copy[31:24];
	  4'b1010: out_byte <= word_copy[23:16];
	  4'b1100: out_byte <= word_copy[15:8];
	  4'b1110: out_byte <= word_copy[7:0];
	  default: mux_state <= 4'b0000;
	endcase // case (mux_state)
	 

	// Testing for busy is problematic if we are keeping the
	// module busy all the time :-/ So we need some wait stages
	// between the bytes.

	if (!busy && send)
	  begin
	     mux_state <= 4'b1000;
	     word_copy <= word;
	  end  

	else if (mux_state[3] && ~mux_state[0] && !TxD_busy)
	  begin
	     serial_start <= 1;
	     mux_state <= mux_state + 1;

	     out_byte <= word_copy[31:24];
	     word_copy <= (word_copy << 8);
	  end
	
	// wait stages
	else if (mux_state[3] && mux_state[0])
	  begin
	     serial_start <= 0;
	     if (!TxD_busy) mux_state <= mux_state + 1;
	  end
     end

   uart_tx serializer(.i_Rst_L(1'b1), .i_Clock(clk), .i_TX_DV(TxD_start),
		.i_TX_Byte(out_byte), .o_TX_Serial(TxD), 
		.o_TX_Active(TxD_busy), .o_TX_Done());
endmodule // serial_send

module e0 (x, y);

	input [31:0] x;
	output [31:0] y;

	assign y = {x[1:0],x[31:2]} ^ {x[12:0],x[31:13]} ^ {x[21:0],x[31:22]};

endmodule


module e1 (x, y);

	input [31:0] x;
	output [31:0] y;

	assign y = {x[5:0],x[31:6]} ^ {x[10:0],x[31:11]} ^ {x[24:0],x[31:25]};

endmodule


module ch (x, y, z, o);

	input [31:0] x, y, z;
	output [31:0] o;

	assign o = z ^ (x & (y ^ z));

endmodule


module maj (x, y, z, o);

	input [31:0] x, y, z;
	output [31:0] o;

	assign o = (x & y) | (z & (x | y));

endmodule


module s0 (x, y);

	input [31:0] x;
	output [31:0] y;

	assign y[31:29] = x[6:4] ^ x[17:15];
	assign y[28:0] = {x[3:0], x[31:7]} ^ {x[14:0],x[31:18]} ^ x[31:3];

endmodule


module s1 (x, y);

	input [31:0] x;
	output [31:0] y;

	assign y[31:22] = x[16:7] ^ x[18:9];
	assign y[21:0] = {x[6:0],x[31:17]} ^ {x[8:0],x[31:19]} ^ x[31:10];

endmodule

module sha256_transform #(
	parameter LOOP = 6'd4
) (
	input clk,
	input feedback,
	input [5:0] cnt,
	input [255:0] rx_state,
	input [511:0] rx_input,
	output reg [255:0] tx_hash
);

	// Constants defined by the SHA-2 standard.
	localparam Ks = {
		32'h428a2f98, 32'h71374491, 32'hb5c0fbcf, 32'he9b5dba5,
		32'h3956c25b, 32'h59f111f1, 32'h923f82a4, 32'hab1c5ed5,
		32'hd807aa98, 32'h12835b01, 32'h243185be, 32'h550c7dc3,
		32'h72be5d74, 32'h80deb1fe, 32'h9bdc06a7, 32'hc19bf174,
		32'he49b69c1, 32'hefbe4786, 32'h0fc19dc6, 32'h240ca1cc,
		32'h2de92c6f, 32'h4a7484aa, 32'h5cb0a9dc, 32'h76f988da,
		32'h983e5152, 32'ha831c66d, 32'hb00327c8, 32'hbf597fc7,
		32'hc6e00bf3, 32'hd5a79147, 32'h06ca6351, 32'h14292967,
		32'h27b70a85, 32'h2e1b2138, 32'h4d2c6dfc, 32'h53380d13,
		32'h650a7354, 32'h766a0abb, 32'h81c2c92e, 32'h92722c85,
		32'ha2bfe8a1, 32'ha81a664b, 32'hc24b8b70, 32'hc76c51a3,
		32'hd192e819, 32'hd6990624, 32'hf40e3585, 32'h106aa070,
		32'h19a4c116, 32'h1e376c08, 32'h2748774c, 32'h34b0bcb5,
		32'h391c0cb3, 32'h4ed8aa4a, 32'h5b9cca4f, 32'h682e6ff3,
		32'h748f82ee, 32'h78a5636f, 32'h84c87814, 32'h8cc70208,
		32'h90befffa, 32'ha4506ceb, 32'hbef9a3f7, 32'hc67178f2};


	genvar i;

	generate

		for (i = 0; i < 64/LOOP; i = i + 1) begin : HASHERS
			wire [511:0] W;
			wire [255:0] state;

			if(i == 0)
				sha256_digester U (
					.clk(clk),
					.k(Ks[32*(63-cnt) +: 32]),
					.rx_w(feedback ? W : rx_input),
					.rx_state(feedback ? state : rx_state),
					.tx_w(W),
					.tx_state(state)
				);
			else
				sha256_digester U (
					.clk(clk),
					.k(Ks[32*(63-LOOP*i-cnt) +: 32]),
					.rx_w(feedback ? W : HASHERS[i-1].W),
					.rx_state(feedback ? state : HASHERS[i-1].state),
					.tx_w(W),
					.tx_state(state)
				);
		end

	endgenerate

	always @ (posedge clk)
	begin
		if (!feedback)
		begin
			tx_hash[`IDX(0)] <= rx_state[`IDX(0)] + HASHERS[64/LOOP-6'd1].state[`IDX(0)];
			tx_hash[`IDX(1)] <= rx_state[`IDX(1)] + HASHERS[64/LOOP-6'd1].state[`IDX(1)];
			tx_hash[`IDX(2)] <= rx_state[`IDX(2)] + HASHERS[64/LOOP-6'd1].state[`IDX(2)];
			tx_hash[`IDX(3)] <= rx_state[`IDX(3)] + HASHERS[64/LOOP-6'd1].state[`IDX(3)];
			tx_hash[`IDX(4)] <= rx_state[`IDX(4)] + HASHERS[64/LOOP-6'd1].state[`IDX(4)];
			tx_hash[`IDX(5)] <= rx_state[`IDX(5)] + HASHERS[64/LOOP-6'd1].state[`IDX(5)];
			tx_hash[`IDX(6)] <= rx_state[`IDX(6)] + HASHERS[64/LOOP-6'd1].state[`IDX(6)];
			tx_hash[`IDX(7)] <= rx_state[`IDX(7)] + HASHERS[64/LOOP-6'd1].state[`IDX(7)];
		end
	end


endmodule


module sha256_digester (clk, k, rx_w, rx_state, tx_w, tx_state);

	input clk;
	input [31:0] k;
	input [511:0] rx_w;
	input [255:0] rx_state;

	output reg [511:0] tx_w;
	output reg [255:0] tx_state;


	wire [31:0] e0_w, e1_w, ch_w, maj_w, s0_w, s1_w;


	e0	e0_blk	(rx_state[`IDX(0)], e0_w);
	e1	e1_blk	(rx_state[`IDX(4)], e1_w);
	ch	ch_blk	(rx_state[`IDX(4)], rx_state[`IDX(5)], rx_state[`IDX(6)], ch_w);
	maj	maj_blk	(rx_state[`IDX(0)], rx_state[`IDX(1)], rx_state[`IDX(2)], maj_w);
	s0	s0_blk	(rx_w[63:32], s0_w);
	s1	s1_blk	(rx_w[479:448], s1_w);

	wire [31:0] t1 = rx_state[`IDX(7)] + e1_w + ch_w + rx_w[31:0] + k;
	wire [31:0] t2 = e0_w + maj_w;
	wire [31:0] new_w = s1_w + rx_w[319:288] + s0_w + rx_w[31:0];
	

	always @ (posedge clk)
	begin
		tx_w[511:480] <= new_w;
		tx_w[479:0] <= rx_w[511:32];

		tx_state[`IDX(7)] <= rx_state[`IDX(6)];
		tx_state[`IDX(6)] <= rx_state[`IDX(5)];
		tx_state[`IDX(5)] <= rx_state[`IDX(4)];
		tx_state[`IDX(4)] <= rx_state[`IDX(3)] + t1;
		tx_state[`IDX(3)] <= rx_state[`IDX(2)];
		tx_state[`IDX(2)] <= rx_state[`IDX(1)];
		tx_state[`IDX(1)] <= rx_state[`IDX(0)];
		tx_state[`IDX(0)] <= t1 + t2;
	end

endmodule

module uart_top(
		input i_Clock,
		input UART_RX,
		
		output led1,
		output UART_TX
		// input[7:0] word,
    );
	 
	reg[26:0] clk;
	reg r_TX_DV;
	reg led_val;
	reg [7:0] r_TX_Byte;
	
	wire [7:0] w_RX_Byte;
	wire w_RX_DV;
	wire w_TX_Active;
	
	/* uart_rx receiver(.i_Clock(i_Clock), .i_RX_Serial(UART_RX),
		.o_RX_DV(w_RX_DV), .o_RX_Byte(w_RX_Byte));
		
	uart_tx transmitter(.i_Rst_L(1'b1), .i_Clock(i_Clock), .i_TX_DV(w_RX_DV),
		.i_TX_Byte(w_RX_Byte), .o_TX_Serial(UART_TX),
		.o_TX_Active(w_TX_Active), .o_TX_Done());*/

	/* sevensegment_display display(.i_Clock(i_Clock), .word(8'hd1), 
		.SevenSegmentEnable(SevenSegmentEnable), .SevenSegment(SevenSegment)); */
		
	/* wire [2:0] w_SevenSegmentEnable = 3'b100;
	wire [7:0] w_SevenSegment = 8'b11000001;
	
	assign SevenSegmentEnable = w_SevenSegmentEnable;
	assign SevenSegment = w_SevenSegment; */
	
	/*always @(posedge i_Clock) begin
		if (clk[26])
		begin
			r_TX_DV <= 1'b1;
			r_TX_Byte <= 8'h61;
		end
		else
		begin
			r_TX_DV <= 1'b0;
		end
		clk <= clk + 1;
	end*/
	
    reg[24:0] cnt_led;
	assign led1 = led_val;
	parameter LOOP_LOG2 = 0;

	// No need to adjust these parameters
	localparam [5:0] LOOP = (6'd1 << LOOP_LOG2);
	// The nonce will always be larger at the time we discover a valid
	// hash. This is its offset from the nonce that gave rise to the valid
	// hash (except when LOOP_LOG2 == 0 or 1, where the offset is 131 or
	// 66 respectively).
	localparam [31:0] GOLDEN_NONCE_OFFSET = (32'd1 << (7 - LOOP_LOG2)) + 32'd1;

	//// 
	reg [255:0] state = 0;
	reg [511:0] data = 0;
   reg [31:0] 	    nonce = 32'h00000000;

	//// PLL
	wire hash_clk;
	assign hash_clk = i_Clock;

	//// Hashers
	wire [255:0] hash, hash2;
	reg [5:0] cnt = 6'd0;
	reg feedback = 1'b0;

	sha256_transform #(.LOOP(LOOP)) uut (
		.clk(hash_clk),
		.feedback(feedback),
		.cnt(cnt),
		.rx_state(state),
		.rx_input(data),
		.tx_hash(hash)
	);
	sha256_transform #(.LOOP(LOOP)) uut2 (
		.clk(hash_clk),
		.feedback(feedback),
		.cnt(cnt),
		.rx_state(256'h5be0cd191f83d9ab9b05688c510e527fa54ff53a3c6ef372bb67ae856a09e667),
		.rx_input({256'h0000010000000000000000000000000000000000000000000000000080000000, hash}),
		.tx_hash(hash2)
	);


	//// Virtual Wire Control
	reg [255:0] midstate_buf = 0, data_buf = 0;
	wire [255:0] midstate_vw, data2_vw;
   
   serial_receive serrx (.clk(hash_clk), .RxD(UART_RX), .midstate(midstate_vw), .data2(data2_vw));
   
	//// Virtual Wire Output
	reg [31:0] golden_nonce = 0;
   reg 		   serial_send;
   wire 	   serial_busy;

   serial_transmit sertx (.clk(hash_clk), .TxD(UART_TX), .send(serial_send), .busy(serial_busy), .word(golden_nonce));
   

	//// Control Unit
	reg is_golden_ticket = 1'b0;
	reg feedback_d1 = 1'b1;
	wire [5:0] cnt_next;
	wire [31:0] nonce_next;
	wire feedback_next;
	reg reset = 1'b0;	// NOTE: Reset is not currently used in the actual FPGA; for simulation only.

	assign cnt_next =  reset ? 6'd0 : (LOOP == 1) ? 6'd0 : (cnt + 6'd1) & (LOOP-1);
	// On the first count (cnt==0), load data from previous stage (no feedback)
	// on 1..LOOP-1, take feedback from current stage
	// This reduces the throughput by a factor of (LOOP), but also reduces the design size by the same amount
	assign feedback_next = (LOOP == 1) ? 1'b0 : (cnt_next != {(LOOP_LOG2){1'b0}});
	assign nonce_next =
		reset ? 32'd0 :
		feedback_next ? nonce : (nonce + 32'd1);

	
	always @ (posedge hash_clk)
	begin
		midstate_buf <= midstate_vw;
		data_buf <= data2_vw;

		cnt <= cnt_next;
		feedback <= feedback_next;
		feedback_d1 <= feedback;

		// Give new data to the hasher
		state <= midstate_buf;
		data <= {384'h000002800000000000000000000000000000000000000000000000000000000000000000000000000000000080000000, nonce_next, data_buf[95:0]};
		nonce <= nonce_next;


		// Check to see if the last hash generated is valid.
		is_golden_ticket <= (hash2[255:224] == 32'h00000000) && !feedback_d1;
		if(is_golden_ticket)
		begin
			// TODO: Find a more compact calculation for this
			if (LOOP == 1)
				golden_nonce <= nonce - 32'd131;
			else if (LOOP == 2)
				golden_nonce <= nonce - 32'd66;
			else
				golden_nonce <= nonce - GOLDEN_NONCE_OFFSET;

		   if (!serial_busy) serial_send <= 1;
		end // if (is_golden_ticket)
		else
		  serial_send <= 0;
         
        if (cnt_led[24]) begin
            cnt_led <= 0;
            led_val <= ~led_val;
        end
        else
            cnt_led <= cnt_led + 1;
	end
endmodule
