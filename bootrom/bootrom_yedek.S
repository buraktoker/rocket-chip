#define DRAM_BASE 0x80000000
#include "accumulator.h"
#include "riscv_test_rocc.h"

.section .text.start, "ax", @progbits
.globl _start
_start:
  //fsrmi 0x1 // fcsr rounding mode 1
  li a1,0x80000000
  csrw mtvec,a1
  //csrwi 0x2,0x1
  //li  a0, 0 
  //li a1, 16
  //li a2,0x0
  //ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_INT_TO_POSIT) //a
  //li a1, 8
  //li a2,0x1
  //ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_INT_TO_POSIT) //b
  //li a1, 16
  //li a2,0x2
  //ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_INT_TO_POSIT) //c
  //li a1, 64
  //li a2,0x3
  //ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_INT_TO_POSIT) //d   
  //li a1, 0x30
  //li a2,4
  //ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) //mul1
  //li a1, 0x21
  //li a2,5
  //ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) //mul2
  //li a1, 0x54
  //li a2,6
  //ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_SUB) //c_matrice_posit
  //li a1, 0x30
  //li a2,7
  //ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_ADD) //a_plus_d
  //li a1, 0x0
  //li a2,8
  //ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_WRITE)
  //li a1, 0x78
  //li a2,9
  //ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_SUB) //b_matrice_posit
  //li a1, 0x1
  //li a2,0xa
  //ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_INT_TO_POSIT) //a_matrice_posit
  //li a1, 0x6a
  //li a2,0xb
  //ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) //a_matrice_mul_c_matrice
  //li a1, 0x4
  //li a2,0xc
  //ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_INT_TO_POSIT) //convert_4_posit
  //li a1, 0xbc
  //li a2,0xd
  //ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) //mul_4_a_matrice_c_posit
  //li a1, 0x99
  //li a2,0xe
  //ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) //b_matrice_square
  //li a1, 0xde
  //li a2,0xf
  //ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_SUB) //det_posit
  //li a1, 0xf
  //li a2,0x0
  //ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_SQRT) //sqrt_det_posit
  //li a1, 0x2
  //li a2,0x1
  //ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_INT_TO_POSIT) //convert_2_posit
  //li a1, 0x90
  //li a2,0x2
  //ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_SUB) //sub_sqrt_det_b_matrice
  //li a1, 0xa1
  //li a2,0x3
  //ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL) //mul_2_a_matrice
  //li a1, 0x32
  //li a2,0x4
  //ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_DIV) //x1_posit
  //li a1, 0x90
  //li a2,5
  //ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_ADD) //add_sqrt_det_b_matrice
  //li a1, 0x0
  //li a2,0x6
  //ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_INT_TO_POSIT) //posit_0
  //li a1, 0x56
  //li a2,0x7
  //ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_SUB) //sub_0_add_sqrt_det_b_matrice
  //li a1, 0x37
  //li a2,0x8
  //ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_DIV) //x2_posit
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
