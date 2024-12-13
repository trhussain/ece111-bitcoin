
module bitcoin_hash (input logic        clk, reset_n, start,
                     input logic [15:0] message_addr, output_addr,
                    output logic        done, mem_clk, mem_we,
                    output logic [15:0] mem_addr,
                    output logic [31:0] mem_write_data,
                     input logic [31:0] mem_read_data);

parameter num_nonces = 16;
enum logic [2:0] {IDLE, READ, PHASE_1, PHASE_2_BLOCK, PHASE_3, COMPUTE, WRITE} state;
logic [31:0] hout[num_nonces];

logic   [31:0] h0_my[num_nonces];
logic   [31:0] h1_my[num_nonces];
logic   [31:0] h2_my[num_nonces];
logic   [31:0] h3_my[num_nonces];
logic   [31:0] h4_my[num_nonces];
logic   [31:0] h5_my[num_nonces];
logic   [31:0] h6_my[num_nonces];
logic   [31:0] h7_my[num_nonces];

logic [31:0] a_my[num_nonces];
logic [31:0] b_my[num_nonces];
logic [31:0] c_my[num_nonces];
logic [31:0] d_my[num_nonces];
logic [31:0] e_my[num_nonces];
logic [31:0] f_my[num_nonces];
logic [31:0] g_my[num_nonces];
logic [31:0] h_my[num_nonces];

logic [31:0] w2_outputs[num_nonces][16];
logic [31:0] h_phase2[num_nonces][8];
logic [31:0] h_phase3[num_nonces][8];

logic   done_flags_phase2[num_nonces];
logic   done_flags_phase3[num_nonces];

logic   [31:0] h_phase1[8];

logic   [31:0] h0const;
logic   [31:0] h1const;
logic   [31:0] h2const;
logic   [31:0] h3const;
logic   [31:0] h4const;
logic   [31:0] h5const;
logic   [31:0] h6const;
logic   [31:0] h7const;

assign 			h0const = 32'h6a09e667;
assign 			h1const = 32'hbb67ae85;
assign 			h2const = 32'h3c6ef372;
assign 			h3const = 32'ha54ff53a;
assign 			h4const = 32'h510e527f;
assign 			h5const = 32'h9b05688c;
assign 			h6const = 32'h1f83d9ab;
assign 			h7const = 32'h5be0cd19;

logic   [31:0] s1_my, s0_my;
logic   [31:0] w_my[num_nonces][16];
logic [31:0] m1[16]; // message block 1 (only original data)
logic [31:0] m2[16]; // message block 2 
logic [15:0] offset; // iterate through message
logic [31:0] message[20]; // read in the input data given message_addr and offset

int m, n, t, i,x;
int phase2_onoff = 0;
logic phase3_onoff = 0;

int nonce_counter = 0;

logic        cur_we;
logic [15:0] cur_addr;
logic [31:0] cur_write_data;


assign mem_clk = clk;
assign mem_addr = cur_addr + offset;
assign mem_we = cur_we;
parameter int k[64] = '{
    32'h428a2f98,32'h71374491,32'hb5c0fbcf,32'he9b5dba5,32'h3956c25b,32'h59f111f1,32'h923f82a4,32'hab1c5ed5,
    32'hd807aa98,32'h12835b01,32'h243185be,32'h550c7dc3,32'h72be5d74,32'h80deb1fe,32'h9bdc06a7,32'hc19bf174,
    32'he49b69c1,32'hefbe4786,32'h0fc19dc6,32'h240ca1cc,32'h2de92c6f,32'h4a7484aa,32'h5cb0a9dc,32'h76f988da,
    32'h983e5152,32'ha831c66d,32'hb00327c8,32'hbf597fc7,32'hc6e00bf3,32'hd5a79147,32'h06ca6351,32'h14292967,
    32'h27b70a85,32'h2e1b2138,32'h4d2c6dfc,32'h53380d13,32'h650a7354,32'h766a0abb,32'h81c2c92e,32'h92722c85,
    32'ha2bfe8a1,32'ha81a664b,32'hc24b8b70,32'hc76c51a3,32'hd192e819,32'hd6990624,32'hf40e3585,32'h106aa070,
    32'h19a4c116,32'h1e376c08,32'h2748774c,32'h34b0bcb5,32'h391c0cb3,32'h4ed8aa4a,32'h5b9cca4f,32'h682e6ff3,
    32'h748f82ee,32'h78a5636f,32'h84c87814,32'h8cc70208,32'h90befffa,32'ha4506ceb,32'hbef9a3f7,32'hc67178f2
};
function logic [255:0] sha256_op(input logic [31:0] a, b, c, d, e, f, g, h, w,
                                 input logic [7:0] t);
    logic [31:0] S1, S0, ch, maj, t1, t2; // internal signals
		begin
    S1 = rightrotate(e, 6) ^ rightrotate(e, 11) ^ rightrotate(e, 25);
    // Student to add remaning code below
    // Refer to SHA256 discussion slides to get logic for this function
    ch = (e & f) ^ ((~e) & g);
    t1 = h + S1 + ch + k[t] + w;
    S0 = rightrotate(a, 2) ^ rightrotate(a, 13) ^ rightrotate(a, 22);
    maj = (a & b) ^ (a & c) ^ (b & c);
    t2 = S0 + maj;

    sha256_op = {t1 + t2, a, b, c, d + t1, e, f, g};
end
endfunction




function logic [31:0] rightrotate(input logic [31:0] x,
                                  input logic [ 7:0] r);
											 

   rightrotate = (x >> r) | (x << (32 - r));
endfunction

function logic [31:0] wtnew (input logic [15:0] myVar); // function with no inputs uhhhh
logic [31:0] s0, s1;
	s0 = rightrotate(w_my[myVar][1],7)^rightrotate(w_my[myVar][1],18)^(w_my[myVar][1]>>3);
	s1 = rightrotate(w_my[myVar][14],17)^rightrotate(w_my[myVar][14],19)^(w_my[myVar][14]>>10);
	wtnew = w_my[myVar][0] + s0 + w_my[myVar][9] + s1;
endfunction
// Student  to add rest of the code here


genvar q;
generate
    for (q = 0; q < num_nonces; q++) begin : generate_256_blocks
        // Instantiate the phase2 module for each nonce
			phase2 phase2_inst (
				 .clk(clk),
				 .reset_n(reset_n),
				 .start(start),
				 .nonce_value(q),
				 .on_off(phase2_onoff),
				 .w_in(w_my),
				 .hphase1(h_phase1),
				 .finished(done_flags_phase2[q]),
             .w_output(w2_outputs[q]),
             .hX_my_loc(h_phase2[q])				 
			);
			phase3 phase3_inst ( 
				 .clk(clk),
				 .reset_n(reset_n),			
				 .nonce_value(q),
				 .on_off(phase3_onoff),
             .hphase2(h_phase2[q]),
				 .finished(done_flags_phase3[q]),
				 .hphase3(h_phase3[q])
			);
        initial begin
            $display("Instantiated phase2 for nonce: %x", q);
        end
		  
    end
endgenerate


// okay, serial portion done... idle and read seem to be okay..? 

// maybe redo read...  
	// okay how about making this method more efficient with vectorization instead of just parallelization of modules  
always_ff @(posedge clk, negedge reset_n)
begin
  if (!reset_n) begin
	 
    state <= IDLE;
  end 
  else case (state)

  IDLE: begin 
       if(start) begin
				phase2_onoff <=0;
            cur_we <= 0; 
			   cur_addr <= message_addr; 
				
				for(int m=0; m<num_nonces; m++) begin
					h0_my[m] <= h0const;
					h1_my[m] <= h1const;
					h2_my[m] <= h2const;
					h3_my[m] <= h3const;
					h4_my[m] <= h4const;
					h5_my[m] <= h5const;
					h6_my[m] <= h6const;
					h7_my[m] <= h7const;
				
					a_my[m] <= h0const;
					b_my[m] <= h1const;
					c_my[m] <= h2const;
					d_my[m] <= h3const;
					e_my[m] <= h4const;
					f_my[m] <= h5const;
					g_my[m] <= h6const;
					h_my[m] <= h7const;
				end
			offset <= 0;
			state <= READ;
		 end
		 else begin 
			state <= IDLE;
		 end 
       
    end
  READ: begin 
	 
	   // Ask about the compute time for this algorithm // creates m1 and m2, m1 just for phase 1 stuff 
		if (offset < 21) begin 
			message[offset-1] <= mem_read_data;
			offset <= offset + 1;
		end
		else begin 
			for(x = 0; x < num_nonces; x++) begin
				w_my[0][x] <= message[x];

			end
			
				state <= PHASE_1;
				offset <= 0;
			
	   end 
  end
  PHASE_1: begin 

			// WORD EXPANSION errrrr
			if (t <= 64) begin
					  if (t < 16) begin
							{a_my[0], b_my[0], c_my[0], d_my[0], e_my[0], f_my[0], g_my[0], h_my[0]} = sha256_op(a_my[0], b_my[0], c_my[0], d_my[0], e_my[0], f_my[0], g_my[0], h_my[0], w_my[0][t], t);
					  end else begin
							for(int x = 0; x < 15; x++) begin
									w_my[0][x] <= w_my[0][x+1];
							end							
							w_my[0][15] <= wtnew(0);
							if (t > 16) begin
								{a_my[0], b_my[0], c_my[0], d_my[0], e_my[0], f_my[0], g_my[0], h_my[0]} = sha256_op(a_my[0], b_my[0], c_my[0], d_my[0], e_my[0], f_my[0], g_my[0], h_my[0], w_my[0][15], t-1);
							end 
					  end
	
					  t <= t + 1; // Increment t for the next clock cycle
					  state <= PHASE_1;
			end	 	
			else begin 
					  h0_my[0] <= h0_my[0] + a_my[0];
					  h1_my[0] <= h1_my[0] + b_my[0];
					  h2_my[0] <= h2_my[0] + c_my[0];
					  h3_my[0] <= h3_my[0] + d_my[0];
					  h4_my[0] <= h4_my[0] + e_my[0];
					  h5_my[0] <= h5_my[0] + f_my[0];
					  h6_my[0] <= h6_my[0] + g_my[0];
					  h7_my[0] <= h7_my[0] + h_my[0];
					  
					  h_phase1[0] <= h0_my[0] + a_my[0];
					  h_phase1[1] <= h1_my[0] + b_my[0];
					  h_phase1[2] <= h2_my[0] + c_my[0];
					  h_phase1[3] <= h3_my[0] + d_my[0];
					  h_phase1[4] <= h4_my[0] + e_my[0];
					  h_phase1[5] <= h5_my[0] + f_my[0];
					  h_phase1[6] <= h6_my[0] + g_my[0];
					  h_phase1[7] <= h7_my[0] + h_my[0];
					  i <= 0;
					  t <= 0;
					  state <= PHASE_2_BLOCK;

		  end
			
	end	
	
	
   PHASE_2_BLOCK: begin 
	// okay now to create that blockify of that you previously did just for m2, but now wihtin 
	// m2, and to also assign a-h and h0-h7 
	// computations SHOULD all be done within phase2 block after this, need 
	// to raise a flag in here that when checked in phase2, once high, itll phase2 computes stop

	// need to keep track of some iterator within phase2 so once rounds of computation are over, 
	// THEN the flag will raise high and THEN itll stop 
	
	// okay blockification first

		// while phase2 is still going, keep phase2 flag high
		if (phase2_onoff == 0) begin
			for(int xx = 0; xx < num_nonces; xx++)begin
				for(int y = 0; y < 16; y++)begin
					if (y < 3) begin 
						w_my[xx][y] <= message[y+16]; // to be replaced by nonce value (? check this)
					end
					else if (y==3) begin
						w_my[xx][3] <= 32'h00000000; // REPLACED BY NONCE VALUE 
					end
					else if (y == 4) begin 
						w_my[xx][4] <= 32'h80000000; // 1 padding
					end
					else if(y>4 && y<15) begin
						w_my[xx][y] <= 0;
					end
					else if(y == 15) begin
						w_my[xx][y] = 32'd640;
						phase2_onoff <= 1; // tells phase2 blocks to start!! 
						state <= PHASE_2_BLOCK;
					end									
				end
			end
		end
		if (done_flags_phase2[1] == 1) begin

			state <= PHASE_3;
		end
	end 
	
	PHASE_3: begin
		phase3_onoff <= 1; 
		if (done_flags_phase3[1] == 1) begin
			//phase3_onoff <= 0; 

			state <= WRITE;
		end
		else begin 
			state <= PHASE_3;
		end
	
	end
	WRITE: begin 
	 
//		 $display("My hash results");
//		 $display("Phase 2 Data");
////		 $display("---------------------------");	 	 
////			for (int xx = 0; xx < 15; xx++) begin
////				for(int x = 0; x < 15; x++) begin
////						$display("wmy[%x][%x]: %x", xx,x,w_my[xx][x]);
////				 end
////			end
//				 for(int x = 0; x < 15; x++) begin
//						$display("h_phase1[%x]: %x", x,h_phase1[x]);
//				 end
			 done <= 1;
		   
		 
	end 
	
	
	
	
	
	
	
	endcase
	end
endmodule