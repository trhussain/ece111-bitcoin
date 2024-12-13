module phase3(
    input  logic        clk,
    input  logic        reset_n,
    input  logic [31:0] nonce_value,
	 input  logic        on_off,
    input  logic [31:0] hphase2[8],
    output logic        finished,
    output logic [31:0] hphase3[8]
);


int t = -1;

logic   [31:0] w_3[16];



logic [31:0] a_my_loc;
logic [31:0] b_my_loc;
logic [31:0] c_my_loc;
logic [31:0] d_my_loc;
logic [31:0] e_my_loc;
logic [31:0] f_my_loc;
logic [31:0] g_my_loc;
logic [31:0] h_my_loc;


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
	s0 = rightrotate(w_3[1],7)^rightrotate(w_3[1],18)^(w_3[1]>>3);
	s1 = rightrotate(w_3[14],17)^rightrotate(w_3[14],19)^(w_3[14]>>10);
	wtnew = w_3[0] + s0 + w_3[9] + s1;
endfunction





always_ff @(posedge clk or negedge reset_n) begin
    if (!reset_n) begin

    end else if (on_off) begin
	     if (t == -1) begin 
			  finished <= 0;
			  // Default initialization for hphase3 to avoid latches
			  for (int i = 0; i < 8; i++) begin
					hphase3[i] <= 32'h0;
			  end
			  // Initialize w_3 values
			  w_3[0] <= hphase2[0];
			  w_3[1] <= hphase2[1];
			  w_3[2] <= hphase2[2];
			  w_3[3] <= hphase2[3];
			  w_3[4] <= hphase2[4];
			  w_3[5] <= hphase2[5];
			  w_3[6] <= hphase2[6];
			  w_3[7] <= hphase2[7];
			  w_3[8] <= 32'h80000000; // Padding
			  for (int x = 9; x < 15; x++) begin
					w_3[x] <= 32'h00000000;
			  end
			  w_3[15] <= 32'd256; // SIZE = 256 BITS		 
				
			   a_my_loc <= 32'h6a09e667;
            b_my_loc <= 32'hbb67ae85;
            c_my_loc <=  32'h3c6ef372;
            d_my_loc <= 32'ha54ff53a;
            e_my_loc <= 32'h510e527f;
            f_my_loc <= 32'h9b05688c;
            g_my_loc <=  32'h1f83d9ab;
            h_my_loc <= 32'h5be0cd19;


			  t <= t + 1;

		  end 
        else if (t <= 64) begin
            if ((t == 0 || t == 1 || t ==2) && nonce_value == 0) begin
					  $display("--------------------------------");
					  $display("MY %X Phase3 Nonce value: %x", t,nonce_value);
					  for (int ii = 0; ii < 16; ii++) begin
							 $display("w_3[%0d] = %h", ii, w_3[ii]);
					  end
					  $display("a value: %x", a_my_loc);

				end
            if (t < 16) begin
                {a_my_loc, b_my_loc, c_my_loc, d_my_loc, e_my_loc, f_my_loc, g_my_loc, h_my_loc} = sha256_op(a_my_loc, b_my_loc, c_my_loc, d_my_loc, e_my_loc, f_my_loc, g_my_loc, h_my_loc, w_3[t], t);
            end else begin
                for (int x = 0; x < 15; x++) begin
                    w_3[x] <= w_3[x + 1];
                end
                w_3[15] <= wtnew(0);
                if (t > 16) begin
                    {a_my_loc, b_my_loc, c_my_loc, d_my_loc, e_my_loc, f_my_loc, g_my_loc, h_my_loc} = sha256_op(a_my_loc, b_my_loc, c_my_loc, d_my_loc, e_my_loc, f_my_loc, g_my_loc, h_my_loc, w_3[15], t - 1);
                end
            end

            t <= t + 1; // Increment t for the next clock cycle
        end else begin
            // Update hphase3 values when t > 64
            hphase3[0] <= 32'h6a09e667 + a_my_loc;
            hphase3[1] <= 32'hbb67ae85 + b_my_loc;
            hphase3[2] <= 32'h3c6ef372 + c_my_loc;
            hphase3[3] <= 32'ha54ff53a + d_my_loc;
            hphase3[4] <= 32'h510e527f + e_my_loc;
            hphase3[5] <= 32'h9b05688c + f_my_loc;
            hphase3[6] <= 32'h1f83d9ab + g_my_loc;
            hphase3[7] <= 32'h5be0cd19 + h_my_loc;
            finished <= 1;
				if (nonce_value == 0) begin
					  for (int ii = 0; ii < 8; ii++) begin
					  
							 $display("hphase3[%0d] = %h", ii, hphase3[ii]);
							
						end
				end
        end
    end else begin
        // Default values for hphase3 in case on_off is not set
        for (int i = 0; i < 8; i++) begin
            hphase3[i] <= hphase3[i];
        end
		  
    end
end




endmodule