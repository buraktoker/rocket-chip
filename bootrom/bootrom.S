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
  li  a0, 0 
  li a1, 0x60000000
  li a2,0
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_WRITE)
  li a1, 0x64000000
  li a2,1
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_WRITE)
  li a1, 0x10
  li a2, 2
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_ADD)
  li a1, 0x10
  li a2,3
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_MUL)
  li a1, 0x64000000
  li a2,0
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_WRITE)
  li a1,0x4
  //fcvt.s.w f10,a1
  li a1, 0x64000000
  li a2,0
  ROCC_INSTRUCTION_RAW_R_R_R(0,x0,a1,a2,k_DO_POSIT_WRITE)

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
