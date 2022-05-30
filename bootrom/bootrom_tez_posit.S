#define DRAM_BASE 0x80000000
#include "accumulator.h"
#include "riscv_test_rocc.h"

.section .text.start, "ax", @progbits
.globl _start
_start:
  //fsrmi 0x1 // fcsr rounding mode 1
  li a1,0x80000000
  csrw mtvec,a1
  li a3,0
  li a4,50
  li a1, 20
  li a2,0x0
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_INT_TO_POSIT) //k11
  li a1, 10
  li a2,0x1
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_INT_TO_POSIT) //k12
  li a1, 0
  li a2,0x2
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_INT_TO_POSIT) //k13
  li a1, 11
  li a2,0x3
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_INT_TO_POSIT) //k21
  li a1, 5
  li a2,0x4
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_INT_TO_POSIT) //k22
  li a1, 3
  li a2,0x5
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_INT_TO_POSIT) //k23
  li a1, 0
  li a2,0x6
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_INT_TO_POSIT) //k31
  li a1, 8
  li a2,0x7
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_INT_TO_POSIT) //k32
  li a1, 6
  li a2,0x8
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_INT_TO_POSIT) //k33
  li a1, 0
  li a2,0x9
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_INT_TO_POSIT) //zero_ref
  loop_qr:
  li a1, 0x0000 
  li a2, 0x0a 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) // mul_of_a1_p_1 0x0a 
  li a1, 0x0303 
  li a2, 0x0b 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) // mul_of_a1_p_2 0x0b 
  li a1, 0x0606 
  li a2, 0x0c 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) // mul_of_a1_p_3 0x0c 
  li a1, 0x0b0a 
  li a2, 0x0d 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_ADD) // length_a1_p_power_2_1 0x0d 
  li a1, 0x0c0d 
  li a2, 0x0e 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_ADD) // length_a1_p_power_2 0x0e 
  li a1, 0x0e 
  li a2, 0x0f 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_SQRT) // length_a1_p 0x0f 
  li a1, 0x0f00 
  li a2, 0x10 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_DIV) // q1_1 0x10 
  li a1, 0x0f03 
  li a2, 0x11 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_DIV) // q1_2 0x11 
  li a1, 0x0f06 
  li a2, 0x12 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_DIV) // q1_3 0x12 
  li a1, 0x0110 
  li a2, 0x13 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) // dot_q1_a2_1 0x13 
  li a1, 0x0411 
  li a2, 0x14 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) // dot_q1_a2_2 0x14 
  li a1, 0x0712 
  li a2, 0x15 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) // dot_q1_a2_3 0x15 
  li a1, 0x1413 
  li a2, 0x16 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_ADD) // dot_q1_a2_sum_1 0x16 
  li a1, 0x1516 
  li a2, 0x17 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_ADD) // dot_q1_a2_sum 0x17 
  li a1, 0x1710 
  li a2, 0x18 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) // dot_q1_a2_mul_q1_1 0x18 
  li a1, 0x1711 
  li a2, 0x19 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) // dot_q1_a2_mul_q1_2 0x19 
  li a1, 0x1712 
  li a2, 0x1a 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) // dot_q1_a2_mul_q1_3 0x1a 
  li a1, 0x1801 
  li a2, 0x1b 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_SUB) // a2_p_1 0x1b 
  li a1, 0x1904 
  li a2, 0x1c 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_SUB) // a2_p_2 0x1c 
  li a1, 0x1a07 
  li a2, 0x1d 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_SUB) // a2_p_3 0x1d 
  li a1, 0x1b1b 
  li a2, 0x1e 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) // a2_p_1_power_2 0x1e 
  li a1, 0x1c1c 
  li a2, 0x1f 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) // a2_p_2_power_2 0x1f 
  li a1, 0x1d1d 
  li a2, 0x20 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) // a2_p_3_power_2 0x20 
  li a1, 0x1f1e 
  li a2, 0x21 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_ADD) // length_a2_p_1 0x21 
  li a1, 0x2021 
  li a2, 0x22 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_ADD) // length_a2_p 0x22 
  li a1, 0x22 
  li a2, 0x23 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_SQRT) // sqrt_length_a2_p 0x23 
  li a1, 0x231b 
  li a2, 0x24 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_DIV) // q2_1 0x24 
  li a1, 0x231c 
  li a2, 0x25 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_DIV) // q2_2 0x25 
  li a1, 0x231d 
  li a2, 0x26 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_DIV) // q2_3 0x26 
  li a1, 0x0210 
  li a2, 0x27 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) // dot_q1_a3_1 0x27 
  li a1, 0x0511 
  li a2, 0x28 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) // dot_q1_a3_2 0x28 
  li a1, 0x0812 
  li a2, 0x29 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) // dot_q1_a3_3 0x29 
  li a1, 0x2827 
  li a2, 0x2a 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_ADD) // dot_q1_a3_sum_1 0x2a 
  li a1, 0x292a 
  li a2, 0x2b 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_ADD) // dot_q1_a3_sum 0x2b 
  li a1, 0x0224 
  li a2, 0x2c 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) // dot_q2_a3_1 0x2c 
  li a1, 0x0525 
  li a2, 0x2d 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) // dot_q2_a3_2 0x2d 
  li a1, 0x0826 
  li a2, 0x2e 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) // dot_q2_a3_3 0x2e 
  li a1, 0x2d2c 
  li a2, 0x2f 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_ADD) // dot_q2_a3_sum_1 0x2f 
  li a1, 0x2e2f 
  li a2, 0x30 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_ADD) // dot_q2_a3_sum 0x30 
  li a1, 0x102b 
  li a2, 0x31 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) // dot_q1_a3_mul_q1_1 0x31 
  li a1, 0x112b 
  li a2, 0x32 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) // dot_q1_a3_mul_q1_2 0x32 
  li a1, 0x122b 
  li a2, 0x33 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) // dot_q1_a3_mul_q1_3 0x33 
  li a1, 0x2430 
  li a2, 0x34 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) // dot_q2_a3_mul_q2_1 0x34 
  li a1, 0x2530 
  li a2, 0x35 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) // dot_q2_a3_mul_q2_2 0x35 
  li a1, 0x2630 
  li a2, 0x36 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) // dot_q2_a3_mul_q2_3 0x36 
  li a1, 0x3102 
  li a2, 0x37 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_SUB) // a3_p_1_sub_1 0x37 
  li a1, 0x3437 
  li a2, 0x38 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_SUB) // a3_p_1 0x38 
  li a1, 0x3205 
  li a2, 0x39 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_SUB) // a3_p_2_sub_1 0x39 
  li a1, 0x3539 
  li a2, 0x3a 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_SUB) // a3_p_2 0x3a 
  li a1, 0x3308 
  li a2, 0x3b 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_SUB) // a3_p_3_sub_1 0x3b 
  li a1, 0x363b 
  li a2, 0x3c 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_SUB) // a3_p_3 0x3c 
  li a1, 0x3838 
  li a2, 0x3d 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) // a3_p_1_power_2 0x3d 
  li a1, 0x3a3a 
  li a2, 0x3e 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) // a3_p_2_power_2 0x3e 
  li a1, 0x3c3c 
  li a2, 0x3f 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) // a3_p_3_power_2 0x3f 
  li a1, 0x3e3d 
  li a2, 0x40 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_ADD) // length_a3_p_1 0x40 
  li a1, 0x3f40 
  li a2, 0x41 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_ADD) // length_a3_p 0x41 
  li a1, 0x41 
  li a2, 0x42 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_SQRT) // sqrt_length_a3_p 0x42 
  li a1, 0x4238 
  li a2, 0x43 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_DIV) // q3_1 0x43 
  li a1, 0x423a 
  li a2, 0x44 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_DIV) // q3_2 0x44 
  li a1, 0x423c 
  li a2, 0x45 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_DIV) // q3_3 0x45 
  li a1, 0x0010 
  li a2, 0x46 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) // dot_q1_a1_1 0x46 
  li a1, 0x0311 
  li a2, 0x47 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) // dot_q1_a1_2 0x47 
  li a1, 0x0612 
  li a2, 0x48 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) // dot_q1_a1_3 0x48 
  li a1, 0x4746 
  li a2, 0x49 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_ADD) // dot_q1_a1_sum_1 0x49 
  li a1, 0x4849 
  li a2, 0x4a 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_ADD) // r11 0x4a 
  li a1, 0x0917 
  li a2, 0x4b 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_ADD) // r12 0x4b 
  li a1, 0x092b 
  li a2, 0x4c 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_ADD) // r13 0x4c 
  li a1, 0x0124 
  li a2, 0x4d 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) // dot_q2_a2_1 0x4d 
  li a1, 0x0425 
  li a2, 0x4e 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) // dot_q2_a2_2 0x4e 
  li a1, 0x0726 
  li a2, 0x4f 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) // dot_q2_a2_3 0x4f 
  li a1, 0x4e4d 
  li a2, 0x50 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_ADD) // dot_q2_a2_sum_1 0x50 
  li a1, 0x4f50 
  li a2, 0x51 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_ADD) // r22 0x51 
  li a1, 0x0930 
  li a2, 0x52 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_ADD) // r23 0x52 
  li a1, 0x0243 
  li a2, 0x53 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) // dot_q3_a3_1 0x53 
  li a1, 0x0544 
  li a2, 0x54 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) // dot_q3_a3_2 0x54 
  li a1, 0x0845 
  li a2, 0x55 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) // dot_q3_a3_3 0x55 
  li a1, 0x5453 
  li a2, 0x56 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_ADD) // dot_q3_a3_sum_1 0x56 
  li a1, 0x5556 
  li a2, 0x57 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_ADD) // r33 0x57 
  li a1, 0x104a 
  li a2, 0x58 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) // mul_r11_q11 0x58 
  li a1, 0x114b 
  li a2, 0x59 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) // mul_r12_q21 0x59 
  li a1, 0x124c 
  li a2, 0x5a 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) // mul_r13_q31 0x5a 
  li a1, 0x5958 
  li a2, 0x5b 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_ADD) // k11_add_1 0x5b 
  li a1, 0x5a5b 
  li a2, 0x5c 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_ADD) // k11 0x5c 
  li a1, 0x244a 
  li a2, 0x5d 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) // mul_r11_q12 0x5d 
  li a1, 0x254b 
  li a2, 0x5e 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) // mul_r12_q22 0x5e 
  li a1, 0x264c 
  li a2, 0x5f 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) // mul_r13_q32 0x5f 
  li a1, 0x5e5d 
  li a2, 0x60 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_ADD) // k12_add_1 0x60 
  li a1, 0x5f60 
  li a2, 0x61 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_ADD) // k12 0x61 
  li a1, 0x434a 
  li a2, 0x62 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) // mul_r11_q13 0x62 
  li a1, 0x444b 
  li a2, 0x63 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) // mul_r12_q23 0x63 
  li a1, 0x454c 
  li a2, 0x64 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) // mul_r13_q33 0x64 
  li a1, 0x6362 
  li a2, 0x65 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_ADD) // k13_add_1 0x65 
  li a1, 0x6465 
  li a2, 0x66 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_ADD) // k13 0x66 
  li a1, 0x1151 
  li a2, 0x67 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) // mul_r22_q21 0x67 
  li a1, 0x1252 
  li a2, 0x68 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) // mul_r23_q31 0x68 
  li a1, 0x6867 
  li a2, 0x69 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_ADD) // k21 0x69 
  li a1, 0x2551 
  li a2, 0x6a 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) // mul_r22_q22 0x6a 
  li a1, 0x2652 
  li a2, 0x6b 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) // mul_r23_q32 0x6b 
  li a1, 0x6b6a 
  li a2, 0x6c 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_ADD) // k22 0x6c 
  li a1, 0x4451 
  li a2, 0x6d 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) // mul_r22_q23 0x6d 
  li a1, 0x4552 
  li a2, 0x6e 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) // mul_r23_q33 0x6e 
  li a1, 0x6e6d 
  li a2, 0x6f 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_ADD) // k23 0x6f 
  li a1, 0x1257 
  li a2, 0x70 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) // k31 0x70 
  li a1, 0x2657 
  li a2, 0x71 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) // k32 0x71 
  li a1, 0x4557 
  li a2, 0x72 
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) // k33 0x72 
  li a1, 0x095c 
  li a2, 0x00
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_ADD) // k11 moves to m11 
  li a1, 0x0961
  li a2, 0x01
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_ADD) // k12 moves to m12
  li a1, 0x0966
  li a2, 0x02
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_ADD) // k13 moves to m13 
  li a1, 0x0969
  li a2, 0x03
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_ADD) // k21 moves to m21
  li a1, 0x096C
  li a2, 0x04
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_ADD) // k22 moves to m22
  li a1, 0x096F
  li a2, 0x05
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_ADD) // k23 moves to m23
  li a1, 0x0970
  li a2, 0x06
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_ADD) // k31 moves to m31
  li a1, 0x0971
  li a2, 0x07
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_ADD) // k32 moves to m32
  li a1, 0x0972
  li a2, 0x08
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_ADD) // k33 moves to m33


  addi a3,a3,0x1
  bne a3,a4,loop_qr
  //birinci eigenvalue reg 4'de , ikinci sekizde
  //csrwi 0x7c1, 0 // disable chicken bits
  //li s0, DRAM_BASE
  //csrr a0, mhartid
  //la a1, _dtb
  la a1,loop
  loop:  
    jr a1
  jr s0

.section .text.hang, "ax", @progbits
.globl _hang
_hang:
  csrwi 0x7c1, 0 // disable chicken bits
  csrr a0, mhartid
  la a1, _dtb
  csrwi mie, 0
1:
  wfi
  j 1b

.section .rodata.dtb, "a", @progbits
.globl _dtb
.align 5, 0
_dtb:
.ascii "DTB goes here"
