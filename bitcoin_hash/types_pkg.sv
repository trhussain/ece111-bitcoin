package types_pkg;

    parameter num_nonces = 16;

    // Define the hash_arrays_t struct
    typedef struct {
        logic [31:0] h0_my[num_nonces];
        logic [31:0] h1_my[num_nonces];
        logic [31:0] h2_my[num_nonces];
        logic [31:0] h3_my[num_nonces];
        logic [31:0] h4_my[num_nonces];
        logic [31:0] h5_my[num_nonces];
        logic [31:0] h6_my[num_nonces];
        logic [31:0] h7_my[num_nonces];
    } hash_arrays_t;

    // Define the letter_arr struct
    typedef struct {
        logic [31:0] a_my[num_nonces];
        logic [31:0] b_my[num_nonces];
        logic [31:0] c_my[num_nonces];
        logic [31:0] d_my[num_nonces];
        logic [31:0] e_my[num_nonces];
        logic [31:0] f_my[num_nonces];
        logic [31:0] g_my[num_nonces];
        logic [31:0] h_my[num_nonces];
    } letter_arr;

endpackage
