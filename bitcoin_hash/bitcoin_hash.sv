module bitcoin_hash (input logic        clk, reset_n, start,
                     input logic [15:0] message_addr, output_addr,
                    output logic        done, mem_clk, mem_we,
                    output logic [15:0] mem_addr,
                    output logic [31:0] mem_write_data,
                     input logic [31:0] mem_read_data);

parameter num_nonces = 16;

enum logic [2:0] {IDLE, READ, PHASE_1, PHASE_2, PHASE_3, COMPUTE, WRITE} state;
logic [31:0] hout[num_nonces];

logic   [31:0] h0_my[num_nonces];
logic   [31:0] h1_my[num_nonces];
logic   [31:0] h2_my[num_nonces];
logic   [31:0] h3_my[num_nonces];
logic   [31:0] h4_my[num_nonces];
logic   [31:0] h5_my[num_nonces];
logic   [31:0] h6_my[num_nonces];
logic   [31:0] h7_my[num_nonces];

logic   [31:0] a_my[num_nonces];
logic   [31:0] b_my[num_nonces];
logic   [31:0] c_my[num_nonces];
logic   [31:0] d_my[num_nonces];
logic   [31:0] e_my[num_nonces];
logic   [31:0] f_my[num_nonces];
logic   [31:0] g_my[num_nonces];
logic   [31:0] h_my[num_nonces];


logic   [31:0] fh0_my, fh1_my, fh2_my, fh3_my, fh4_my, fh5_my, fh6_my, fh7_my;

logic [31:0] h0_phase1, h1_phase1, h2_phase1, h3_phase1, h4_phase1, h5_phase1, h6_phase1, h7_phase1;
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

int m, n, t, i, q;
int b1_2 = 1;
int nonce_counter = 0;
int b2_flag = 0;

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


//genvar q; 
//generate
	//for (q = 0; q < num_nonces; q++) begin : generate_256_blocks
	//	$display("hihihi: %x", q);
//   end 
//endgenerate


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
			for(int x = 0; x < 16; x++) begin
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
							if (t > 16) {a_my[0], b_my[0], c_my[0], d_my[0], e_my[0], f_my[0], g_my[0], h_my[0]} = sha256_op(a_my[0], b_my[0], c_my[0], d_my[0], e_my[0], f_my[0], g_my[0], h_my[0], w_my[0][15], t-1);
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
					  
					  h0_phase1 <= h0_my[0] + a_my[0];
					  h1_phase1 <= h1_my[0] + b_my[0];
					  h2_phase1 <= h2_my[0] + c_my[0];
					  h3_phase1 <= h3_my[0] + d_my[0];
					  h4_phase1 <= h4_my[0] + e_my[0];
					  h5_phase1 <= h5_my[0] + f_my[0];
					  h6_phase1 <= h6_my[0] + g_my[0];
					  h7_phase1 <= h7_my[0] + h_my[0];
					  i <= 0;
					  t <= 0;
					  state <= WRITE;
					  
			   
		  end
			
	end	
	
	
	COMPUTE: begin 


	end 
	WRITE: begin 
	 
		 $display("My hash results");
		 $display("Phase 1 Data");
		 $display("---------------------------");	 
		 $display("h0_phase1: %x", n, h0_phase1);
		 $display("---------------------------");	 
		 for (int y = 0; y < num_nonces; y++) begin
			 for(int x = 0; x < 15; x++) begin
					$display("wmy[%x][%x]: %x", y, x, w_my[y][x]);
			 end
		 end
			 done <= 1;
		 
	end 
	
	
	
	
	
	
	
	endcase
	end
endmodule