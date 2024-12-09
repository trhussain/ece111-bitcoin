//module phase2(
//    input  logic        clk,
//    input  logic        reset_n,
//    input  logic        start,
//	 input  logic [3:0]       nonce_value,
//	 input  logic [3:0]       on_off,
//    input  logic [31:0] w_in[16][16],    // Input array (row of w_my)
//    input  hash_arrays_t h_all_in,       // Input struct for hash arrays
//    input  letter_arr    letter_in,      // Input struct for letter arrays
//    output hash_arrays_t h_all_out,      // Output struct for updated hash arrays
//    output letter_arr    letter_out,     // Output struct for updated letter arrays
//    output logic         done
//);
//    logic [31:0] w_temp, h_temp;
//int t = 0;
//always_ff @(posedge clk or negedge reset_n) begin
//    if (!reset_n) begin
//        $display("oop");
//    end else if (on_off) begin
//	 
//			if (t <= 64) begin
//					  if (t < 16) begin
//							{l_all.a_my[nonce_value], l_all.b_my[nonce_value], l_all.c_my[nonce_value], l_all.d_my[nonce_value], l_all.e_my[nonce_value], l_all.f_my[nonce_value], l_all.g_my[nonce_value], l_all.h_my[nonce_value]} = sha256_op(l_all.a_my[nonce_value], l_all.b_my[nonce_value], l_all.c_my[nonce_value], l_all.d_my[nonce_value], l_all.e_my[nonce_value], l_all.f_my[nonce_value], l_all.g_my[nonce_value], l_all.h_my[nonce_value], w_my[nonce_value][t], t);
//					  end else begin
//							for(int x = 0; x < 15; x++) begin
//									w_my[nonce_value][x] <= w_my[nonce_value][x+1];
//							end							
//							w_my[nonce_value][15] <= wtnew(nonce_value);
//							if (t > 16) begin
//							{l_all.a_my[nonce_value], l_all.b_my[nonce_value], l_all.c_my[nonce_value], l_all.d_my[nonce_value], l_all.e_my[nonce_value], l_all.f_my[nonce_value], l_all.g_my[nonce_value], l_all.h_my[nonce_value]} = sha256_op(l_all.a_my[nonce_value], l_all.b_my[nonce_value], l_all.c_my[nonce_value], l_all.d_my[nonce_value], l_all.e_my[nonce_value], l_all.f_my[nonce_value], l_all.g_my[nonce_value], l_all.h_my[nonce_value], w_my[nonce_value][15], t-1);
//							end 
//					  end
//	
//					  t <= t + 1; // Increment t for the next clock cycle
//		  end
//        $display("on");
//    end else begin
//        $display("off");
//    end
//end
//
//
//
//endmodule
