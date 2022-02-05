#define DRAM_BASE 0x80000000
#include "accumulator.h"
#include "riscv_test_rocc.h"

.section .text.start, "ax", @progbits
.globl _start
_start:
  //fsrmi 0x1 // fcsr rounding mode 1
  li a1,0x80000000
  csrw mtvec,a1
  li a1, 123
  li a3,0
  li a4,50
  li a2,0x0
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_INT_TO_POSIT) //a
  li a1, 16
  li a2,0x1
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_INT_TO_POSIT) //b
  li a1, 8
  li a2,0x2
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_INT_TO_POSIT) //c
  li a1, 23
  li a2,0x3
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_INT_TO_POSIT) //d
  loop_qr:
  //    posit32_t mul_of_a1_p_1 = p32_mul(a,a);
  li a1, 0x0000
  li a2,4
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) //mul_of_a1_p_1 4
  //    posit32_t mul_of_a1_p_2 = p32_mul(c,c);
  li a1, 0x0202
  li a2,5
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) //mul_of_a1_p_2 5
  //  posit32_t length_a1_p_power_2=p32_add(mul_of_a1_p_1,mul_of_a1_p_2);
  li a1, 0x0504
  li a2,6
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_ADD) //length_a1_p_power_2 6
  
  //  posit32_t length_a1_p=p32_sqrt(length_a1_p_power_2);
  li a1, 0x06
  li a2, 0x07
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_SQRT) //length_a1_p 7
  // posit32_t q1_1=p32_div(a,length_a1_p);
  li a1, 0x0700
  li a2, 0x08
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_DIV) //q1_1 8  
  // posit32_t q1_2=p32_div(c,length_a1_p); 
  li a1, 0x0702
  li a2, 0x09
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_DIV) //q1_2 9 
  //     posit32_t dot_q1_a2_1=p32_mul(q1_1,b); //a2(0)=b a2(1)=d 
  li a1, 0x0801
  li a2, 0x0a
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) //dot_q1_a2_1 a 
  //    posit32_t dot_q1_a2_2=p32_mul(q1_2,d); //a2(0)=b a2(1)=d
  li a1, 0x0903
  li a2, 0x0b
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) //dot_q1_a2_2 b  
  //     posit32_t dot_q1_a2=p32_add(dot_q1_a2_1,dot_q1_a2_2); //a2(0)=b a2(1)=d 
  li a1, 0x0b0a
  li a2, 0x0c
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_ADD) //dot_q1_a2 c  
  //     posit32_t dot_q1_a2_product_q1_1=p32_mul(dot_q1_a2,q1_1);  
  li a1, 0x0c08
  li a2, 0x0d
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) //dot_q1_a2_product_q1_1 d  
  //    posit32_t dot_q1_a2_product_q1_2=p32_mul(dot_q1_a2,q1_2);
  li a1, 0x0c09
  li a2, 0x0e
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) //dot_q1_a2_product_q1_2 e
  //     posit32_t a2_p_1=p32_sub(b,dot_q1_a2_product_q1_1); 
  li a1, 0x0d01
  li a2, 0x0f
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_SUB) //a2_p_1 f
  //    posit32_t a2_p_2=p32_sub(d,dot_q1_a2_product_q1_2); 
  li a1, 0x0e03
  li a2, 0x10
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_SUB) //a2_p_2 0x10
  //     posit32_t mul_of_a2_p_1 = p32_mul(a2_p_1,a2_p_1);
  li a1, 0x0f0f
  li a2, 0x11
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) //mul_of_a2_p_1 0x11
  //     posit32_t mul_of_a2_p_2 = p32_mul(a2_p_2,a2_p_2);
  li a1, 0x1010
  li a2, 0x12
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) //mul_of_a2_p_2 0x12
  //    posit32_t length_a2_p_power_2=p32_add(mul_of_a2_p_1,mul_of_a2_p_2);
  li a1, 0x1211
  li a2, 0x13
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_ADD) //length_a2_p_power_2 0x13
  //    posit32_t length_a2_p=p32_sqrt(length_a2_p_power_2);
  li a1, 0x13
  li a2, 0x14
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_SQRT) //length_a2_p 0x14
  //     posit32_t q2_1=p32_div(a2_p_1,length_a2_p);
  li a1, 0x140f
  li a2, 0x15
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_DIV) //q2_1 0x15
  //        posit32_t q2_2=p32_div(a2_p_2,length_a2_p); 
  li a1, 0x1410
  li a2, 0x16
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_DIV) //q2_2 0x16
  //posit32_t r21=convertFloatToP32(0.0);
  li a1, 0
  li a2,0x19
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_INT_TO_POSIT) // r21 is zero
  //posit32_t r11=length_a1_p;
  li a1, 0x0719
  li a2,0x17
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_ADD) // r11 0x17
  //posit32_t r12=dot_q1_a2;
  li a1, 0x0c19
  li a2, 0x18
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_ADD) //r12 0x18
  
  //posit32_t r22=length_a2_p;
  li a1, 0x1419
  li a2, 0x1a
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_ADD) //r22 0x1a
  //posit32_t q11=q1_1;
  li a1, 0x0819
  li a2, 0x1b
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_ADD) //q11 0x1b
  //posit32_t q12=q2_1;
  li a1, 0x1519
  li a2, 0x1c
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_ADD) //q12 0x1c
  //posit32_t q21=q1_2;
  li a1, 0x0919
  li a2, 0x1d
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_ADD) //q21 0x1d
  //posit32_t q22=q2_2; 
  li a1, 0x1619
  li a2, 0x1e
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_ADD) //q22 0x1e
  //posit32_t k11_1 = p32_mul(r11,q11);
  li a1, 0x171b
  li a2, 0x1f
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) //k11_1 0x1f
  //posit32_t k11_2 = p32_mul(r12,q21);
  li a1, 0x181d
  li a2, 0x20
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) //k11_2 0x20
  //posit32_t k11 = p32_add(k11_1,k11_2);
  li a1, 0x201f
  li a2, 0x0
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_ADD) //k11 is a
  //posit32_t k12_1 = p32_mul(r11,q12);
  li a1, 0x171C
  li a2, 0x1f
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) 
  //posit32_t k12_2 = p32_mul(r12,q22);
  li a1, 0x181E
  li a2, 0x20
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) 

  //posit32_t k12 = p32_add(k12_1,k12_2);
  li a1, 0x201f
  li a2, 0x1
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_ADD) //k12 is 1

  //posit32_t k21_1 = p32_mul(r21,q11);
  li a1, 0x191B
  li a2, 0x1f
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) 
  //posit32_t k21_2 = p32_mul(r22,q21);
  li a1, 0x1A1D
  li a2, 0x20
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) 
  
  //posit32_t k21 = p32_add(k21_1,k21_2);
  li a1, 0x201f
  li a2, 0x2
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_ADD) //k21 is 2

  //posit32_t k22_1 = p32_mul(r21,q12);
  li a1, 0x191C
  li a2, 0x1f
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) 

  //posit32_t k22_2 = p32_mul(r22,q22);
  li a1, 0x1A1E
  li a2, 0x20
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) 

  //posit32_t k22 = p32_add(k22_1,k22_2);
  
  li a1, 0x201f
  li a2, 0x3
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_ADD) //k22 is 3
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
