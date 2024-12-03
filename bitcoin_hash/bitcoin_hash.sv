module bitcoin_hash (input logic        clk, reset_n, start,
                     input logic [15:0] message_addr, output_addr,
                    output logic        done, mem_clk, mem_we,
                    output logic [15:0] mem_addr,
                    output logic [31:0] mem_write_data,
                     input logic [31:0] mem_read_data);

parameter num_nonces = 16;

enum logic [2:0] {IDLE, READ, BLOCK, BLOCK_3, COMPUTE, WRITE} state;
logic [31:0] hout[num_nonces];

logic   [31:0] h0_my[num_nonces];
logic   [31:0] h1_my[num_nonces];
logic   [31:0] h2_my[num_nonces];
logic   [31:0] h3_my[num_nonces];
logic   [31:0] h4_my[num_nonces];
logic   [31:0] h5_my[num_nonces];
logic   [31:0] h6_my[num_nonces];
logic   [31:0] h7_my[num_nonces];
logic   [31:0] fh0_my, fh1_my, fh2_my, fh3_my, fh4_my, fh5_my, fh6_my, fh7_my;
logic   [31:0] a_my, b_my, c_my, d_my, e_my, f_my, g_my, h_my;

logic   [31:0] s1_my, s0_my;
logic   [31:0] w_my[64];
logic [31:0] m1[16]; // message block 1 (only original data)
logic [31:0] m2[16]; // message block 2 
logic [15:0] offset; // iterate through message
logic [31:0] message[20]; // read in the input data given message_addr and offset

int m, n, t, i;
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


// Student  to add rest of the code here


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
 
			offset <= 16'b0;
			fh0_my <= 32'h0x6a09e667;
			fh1_my <= 32'h0xbb67ae85;
			fh2_my <= 32'h0x3c6ef372;
			fh3_my <= 32'h0xa54ff53a;
			fh4_my <= 32'h0x510e527f;
			fh5_my <= 32'h0x9b05688c;
			fh6_my <= 32'h0x1f83d9ab;
			fh7_my <= 32'h0x5be0cd19;
			a_my <= 32'h0x6a09e667;
			b_my <= 32'h0xbb67ae85;
			c_my <= 32'h0x3c6ef372;
			d_my <= 32'h0xa54ff53a;
			e_my <= 32'h0x510e527f;
			f_my <= 32'h0x9b05688c;
			g_my <= 32'h0x1f83d9ab;
			h_my <= 32'h0x5be0cd19;
			
			state <= READ;
		 end
		 else begin 
			state <= IDLE;
		 end 
       
    end
	 READ: begin 
		if (offset < 21) begin // message
			message[offset-1] <= mem_read_data;
			offset <= offset + 1;
		end
		else begin 
			for (int q = 0; q < 16; q++) begin 
				 m1[q] <= message[q];
				 if (q < 3) begin
					m2[q] <= message[q+16];
				 end	
			end
			m2[3] <= 32'h00000000; // replaced by NONCE value
			m2[4] <= 32'h80000000; // 1 padding
			for (int qq = 5;  qq < 15; qq++) begin
				  m2[qq] = 32'h0; // 0 padding 
			end
			m2[15] <= 32'd640;   // Least significant 32 bits okay check if its 640 -> 00000 ... 11 0000
			state <= BLOCK;
		end 
		
	 end 
	BLOCK: begin 
		// -----------------------
		// Block 1 computation 
		// -----------------------
		if (b1_2 == 1) begin
			// WORD EXPANSION
			if (t < 64) begin
					  if (t < 16) begin
							w_my[t] <=  m1[t]; // Assign directly from m1 or m2
					  end else begin
							s0_my = rightrotate(w_my[t-15], 7) ^ rightrotate(w_my[t-15], 18) ^ (w_my[t-15] >> 3);
							s1_my = rightrotate(w_my[t-2], 17) ^ rightrotate(w_my[t-2], 19) ^ (w_my[t-2] >> 10);
							w_my[t] = w_my[t-16] + s0_my + w_my[t-7] + s1_my;
					  end
	
					  t <= t + 1; // Increment t for the next clock cycle
			end	 	
			else begin 
				state <= COMPUTE;
			end
		end
		// -----------------------
		// End block 1 computation 
		// -----------------------
		
		// -----------------------
		// Block 2 computation -- First hash for each Nonce
		// ---------------------
		else begin 
			// Flags to switch between word expansion, hash rounds, & hash reset
		   // or have nonce_rounds counter, hash reset flag. once nonce_rounds > num_nonces, reset_hash & nonce_rounds
				if (nonce_counter > num_nonces) begin
							 $display("Phase 2 end ");
							 $display("---------------------------");	 
							 for (int n = 0; n < num_nonces; n++) begin
									$display("h0_my[%x]: %x", n, h0_my[n]);

							 end
							b2_flag <= 1;
							nonce_counter <= 0;
							state <= BLOCK_3;
				end
			   else begin 
					m2[3] = nonce_counter;
					if (t < 64) begin
					  if (t < 16) begin
							w_my[t] <=  m2[t]; // Assign directly from m1 or m2
					  end else begin
							s0_my = rightrotate(w_my[t-15], 7) ^ rightrotate(w_my[t-15], 18) ^ (w_my[t-15] >> 3);
							s1_my = rightrotate(w_my[t-2], 17) ^ rightrotate(w_my[t-2], 19) ^ (w_my[t-2] >> 10);
							w_my[t] = w_my[t-16] + s0_my + w_my[t-7] + s1_my;
					  end
			
					  t <= t + 1; // Increment t for the next clock cycle
					end	 	
					
					else begin 
						  h0_my[nonce_counter] <= fh0_my;
						  h1_my[nonce_counter] <= fh1_my;
						  h2_my[nonce_counter] <= fh2_my;
						  h3_my[nonce_counter] <= fh3_my;
						  h4_my[nonce_counter] <= fh4_my;
						  h5_my[nonce_counter] <= fh5_my;
						  h6_my[nonce_counter] <= fh6_my;
						  h7_my[nonce_counter] <= fh7_my;
				
						  a_my <= fh0_my;
						  b_my <= fh1_my;
						  c_my <= fh2_my;
						  d_my <= fh3_my;
						  e_my <= fh4_my;
						  f_my <= fh5_my;
						  g_my <= fh6_my;
						  h_my <= fh7_my;				
							
					
						  t <= 0;
						  state <= COMPUTE;
					end
				end
			end
		
		
		// -----------------------
		// End block 2 computation 
		// -----------------------
	end	
	
	
	BLOCK_3: begin 
	 if (nonce_counter >= num_nonces) begin
				$display("Phase 3 end ");
				 $display("---------------------------");	 
				 for (int n = 0; n < num_nonces; n++) begin
						$display("h0_my[%x]: %x", n, h0_my[n]);

				 end
				 state <= WRITE;
	 end
	  w_my[0] <= h0_my[nonce_counter];
	  w_my[1] <= h1_my[nonce_counter];
	  w_my[2] <= h2_my[nonce_counter];
	  w_my[3] <= h3_my[nonce_counter];
	  w_my[4] <= h4_my[nonce_counter];
	  w_my[5] <= h5_my[nonce_counter];
	  w_my[6] <= h6_my[nonce_counter];
	  w_my[7] <= h7_my[nonce_counter];
	  w_my[8] <= 32'h80000000; // padding
	  w_my[9] <= 32'h00000000;
	  w_my[10] <= 32'h00000000;
	  w_my[11] <= 32'h00000000;
	  w_my[12] <= 32'h00000000;
	  w_my[13] <= 32'h00000000;
	  w_my[14] <= 32'h00000000;
	  w_my[15] <= 32'd256; 
	  
	  h0_my[nonce_counter] <= 32'h6a09e667;
	  h1_my[nonce_counter] <= 32'hbb67ae85;
	  h2_my[nonce_counter] <= 32'h3c6ef372;
	  h3_my[nonce_counter] <= 32'ha54ff53a;
	  h4_my[nonce_counter] <= 32'h510e527f;
	  h5_my[nonce_counter] <= 32'h9b05688c;
	  h6_my[nonce_counter] <= 32'h1f83d9ab;
	  h7_my[nonce_counter] <= 32'h5be0cd19;

	  a_my <= 32'h6a09e667;
	  b_my <= 32'hbb67ae85;
	  c_my <= 32'h3c6ef372;
	  d_my <= 32'ha54ff53a;
	  e_my <= 32'h510e527f;
	  f_my <= 32'h9b05688c;
	  g_my <= 32'h1f83d9ab;
	  h_my <= 32'h5be0cd19;
	  if (t < 64) begin

			s0_my = rightrotate(w_my[t-15], 7) ^ rightrotate(w_my[t-15], 18) ^ (w_my[t-15] >> 3);
			s1_my = rightrotate(w_my[t-2], 17) ^ rightrotate(w_my[t-2], 19) ^ (w_my[t-2] >> 10);
			w_my[t] = w_my[t-16] + s0_my + w_my[t-7] + s1_my;
         t <= t + 1; // Increment t for the next clock cycle

	  end
	  else begin 
			state <= COMPUTE;
	  end
	
	end
	COMPUTE: begin 
		// -----------------------
		// Block 1 computation 
		// -----------------------
		if (b1_2 == 1) begin
			  if (i < 64) begin
					  {a_my, b_my, c_my, d_my, e_my, f_my, g_my, h_my} = sha256_op(a_my, b_my, c_my, d_my, e_my, f_my, g_my, h_my, w_my[i], i);
					  i <= i + 1;
			  end 
			  else begin 
					  fh0_my <= fh0_my + a_my;
					  fh1_my <= fh1_my + b_my;
					  fh2_my <= fh2_my + c_my;
					  fh3_my <= fh3_my + d_my;
					  fh4_my <= fh4_my + e_my;
					  fh5_my <= fh5_my + f_my;
					  fh6_my <= fh6_my + g_my;
					  fh7_my <= fh7_my + h_my;
					  i <= 0;
					  b1_2 <= 2;
					  t <= 0;
					  state <= BLOCK;
					  
			  end 
		end
		// -----------------------
		// End block 1 computation 
		// -----------------------
		
		
	   // -----------------------
		// Block 2 computation 
		// ---------------------
		
		else begin 
			  if (i < 64) begin
					  {a_my, b_my, c_my, d_my, e_my, f_my, g_my, h_my} = sha256_op(a_my, b_my, c_my, d_my, e_my, f_my, g_my, h_my, w_my[i], i);
					  i <= i + 1;
			  end 
			  else begin 
					  h0_my[nonce_counter] <= h0_my[nonce_counter] + a_my;
					  h1_my[nonce_counter] <= h1_my[nonce_counter] + b_my;
					  h2_my[nonce_counter] <= h2_my[nonce_counter] + c_my;
					  h3_my[nonce_counter] <= h3_my[nonce_counter] + d_my;
					  h4_my[nonce_counter] <= h4_my[nonce_counter] + e_my;
					  h5_my[nonce_counter] <= h5_my[nonce_counter] + f_my;
					  h6_my[nonce_counter] <= h6_my[nonce_counter] + g_my;
					  h7_my[nonce_counter] <= h7_my[nonce_counter] + h_my;
					  i <= 0;
					  b1_2 <= 2;
					  t <= 0;
					  nonce_counter <= nonce_counter + 1;
					 // Flags to see in phase 2 or phase 3 (slide 15)		

					 if (b2_flag == 0) begin
						state <= BLOCK;
					 end
					 else begin 
					 	
						  t <= 16;	 
						  state <= BLOCK_3;
					 end
					  
			  end 			
		end
		// -----------------------
		// End block 2 computation 
		// -----------------------
	end 
	WRITE: begin 
		$display("in write state, nothing");
		done <= 1;
	end 
	
	endcase
	end
endmodule