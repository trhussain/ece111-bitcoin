`include "types_pkg.sv"



module phase2(
    input  logic        clk,
    input  logic        reset_n,
    input  logic        start,
	 input  logic [31:0]       nonce_value,
	 input  logic [31:0]       on_off,
    input  logic [31:0] w_in[16][16],    // Input array (row of w_in)
    input  hash_arrays_t h_all_in,       // Input struct for hash arrays
	 input  logic   [31:0] hphase1[8],

    input  letter_arr    letter_in,      // Input struct for letter arrays
    output hash_arrays_t h_all_out,      // Output struct for updated hash arrays
    output letter_arr    letter_out,     // Output struct for updated letter arrays
    output logic         done
);
    logic [31:0] w_temp, h_temp;
int t = 0;

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
function logic [255:0] sha256_op(input logic [31:0] a, b, c, d, e, f, g, h, w, input logic [7:0] t);
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




function logic [31:0] rightrotate(input logic [31:0] x, input logic [ 7:0] r);								
   rightrotate = (x >> r) | (x << (32 - r));
endfunction

function logic [31:0] wtnew (input logic [15:0] myVar); // function with no inputs uhhhh
	logic [31:0] s0, s1;
	s0 = rightrotate(w_in[myVar][1],7)^rightrotate(w_in[myVar][1],18)^(w_in[myVar][1]>>3);
	s1 = rightrotate(w_in[myVar][14],17)^rightrotate(w_in[myVar][14],19)^(w_in[myVar][14]>>10);
	wtnew = w_in[myVar][0] + s0 + w_in[myVar][9] + s1;
endfunction



logic   [31:0] w_internal[16];

hash_arrays_t h_internal;  // Example of struct usage
letter_arr letter_internal;


always_ff @(posedge clk or negedge reset_n) begin
    if (!reset_n) begin
		  t <= 0;
		  
     end else if (on_off) begin
			if (t == 0) begin
			   w_internal[0] = w_in[nonce_value][0];
			   w_internal[1] = w_in[nonce_value][1];
				w_internal[2] = w_in[nonce_value][2];
				w_internal[3] = w_in[nonce_value][3];
				w_internal[4] = w_in[nonce_value][4];
				w_internal[5] = w_in[nonce_value][5];
				w_internal[6] = w_in[nonce_value][6];
				w_internal[7] = w_in[nonce_value][7];
				w_internal[8] = w_in[nonce_value][8];
				w_internal[9] = w_in[nonce_value][9];
				w_internal[10] = w_in[nonce_value][10];
				w_internal[11] = w_in[nonce_value][11];
				w_internal[12] = w_in[nonce_value][12];
				w_internal[13] = w_in[nonce_value][13];
				w_internal[14] = w_in[nonce_value][14];
				w_internal[15] = w_in[nonce_value][15];
				
			   letter_internal.a_my[nonce_value] <= hphase1[0];
            letter_internal.b_my[nonce_value] <= hphase1[1];
            letter_internal.c_my[nonce_value] <= hphase1[2];
            letter_internal.d_my[nonce_value] <= hphase1[3];
            letter_internal.e_my[nonce_value] <= hphase1[4];
            letter_internal.f_my[nonce_value] <= hphase1[5];
            letter_internal.g_my[nonce_value] <= hphase1[6];
            letter_internal.h_my[nonce_value] <= hphase1[7];
			   h_internal.h0_my[nonce_value] <= hphase1[0];
				h_internal.h1_my[nonce_value] <= hphase1[1];
				h_internal.h2_my[nonce_value] <= hphase1[2];
				h_internal.h3_my[nonce_value] <= hphase1[3];
				h_internal.h4_my[nonce_value] <= hphase1[4];
				h_internal.h5_my[nonce_value] <= hphase1[5];
				h_internal.h6_my[nonce_value] <= hphase1[6];
				h_internal.h7_my[nonce_value] <= hphase1[7];	
				t <= t + 1;
			end
			else if (t <= 64) begin
					  if (t < 16) begin
							{letter_internal.a_my[nonce_value], letter_internal.b_my[nonce_value], letter_internal.c_my[nonce_value], letter_internal.d_my[nonce_value], letter_internal.e_my[nonce_value], letter_internal.f_my[nonce_value], letter_internal.g_my[nonce_value], letter_internal.h_my[nonce_value]} = sha256_op(letter_internal.a_my[nonce_value], letter_internal.b_my[nonce_value], letter_internal.c_my[nonce_value], letter_internal.d_my[nonce_value], letter_internal.e_my[nonce_value], letter_internal.f_my[nonce_value], letter_internal.g_my[nonce_value], letter_internal.h_my[nonce_value], w_internal[t], t);
					  end 
					  else begin
						  for(int x = 0; x < 15; x++) begin
										w_internal[x] <= w_internal[x+1];
						  end							
								w_internal[15] <= wtnew(nonce_value);
						  if (t > 16) begin
								{letter_internal.a_my[nonce_value], letter_internal.b_my[nonce_value], letter_internal.c_my[nonce_value], letter_internal.d_my[nonce_value], letter_internal.e_my[nonce_value], letter_internal.f_my[nonce_value], letter_internal.g_my[nonce_value], letter_internal.h_my[nonce_value]} = sha256_op(letter_internal.a_my[nonce_value], letter_internal.b_my[nonce_value], letter_internal.c_my[nonce_value], letter_internal.d_my[nonce_value], letter_internal.e_my[nonce_value], letter_internal.f_my[nonce_value], letter_internal.g_my[nonce_value], letter_internal.h_my[nonce_value], w_internal[15], t-1);
						  end 
					  end
	
					  t <= t + 1; // Increment t for the next clock cycle
		   end
		   else begin 

			   h_internal.h0_my[nonce_value] <= h_internal.h0_my[nonce_value] + letter_internal.a_my[nonce_value];
				h_internal.h1_my[nonce_value] <= h_internal.h1_my[nonce_value] + letter_internal.b_my[nonce_value];
				h_internal.h2_my[nonce_value] <= h_internal.h2_my[nonce_value] + letter_internal.c_my[nonce_value];
				h_internal.h3_my[nonce_value] <= h_internal.h3_my[nonce_value] + letter_internal.d_my[nonce_value];
				h_internal.h4_my[nonce_value] <= h_internal.h4_my[nonce_value] + letter_internal.e_my[nonce_value];
				h_internal.h5_my[nonce_value] <= h_internal.h5_my[nonce_value] + letter_internal.f_my[nonce_value];
				h_internal.h6_my[nonce_value] <= h_internal.h6_my[nonce_value] + letter_internal.g_my[nonce_value];
				h_internal.h7_my[nonce_value] <= h_internal.h7_my[nonce_value] + letter_internal.h_my[nonce_value];
				$display("over here internal 0 nonce(%x): %x", nonce_value, h_internal.h0_my[nonce_value]);

				//on_off <= 0;
		   end
        $display("on");
		  
    end else begin // when its off 
			
        $display("off");
		  $display("h internal 0 nonce(%x): %x", nonce_value, h_internal.h0_my[nonce_value]);
    end
end



endmodule
