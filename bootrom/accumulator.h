// See LICENSE for license details.

#ifndef SRC_MAIN_C_ACCUMULATOR_H
#define SRC_MAIN_C_ACCUMULATOR_H

#include "xcustom.h"
#define k_DO_INT_TO_POSIT 0
#define k_DO_POSIT_TO_INT 1
#define k_DO_FLOAT_TO_POSIT 2
#define k_DO_POSIT_TO_FLOAT 3
#define k_DO_POSIT_WRITE 4
#define k_DO_POSIT_ADD 5
#define k_DO_POSIT_SUB 6
#define k_DO_POSIT_MUL 7
#define k_DO_POSIT_DIV 8
#define k_DO_POSIT_SQRT 9

#define XCUSTOM_ACC 0

#define doWrite(y, rocc_rd, data)                                       \
  ROCC_INSTRUCTION(XCUSTOM_ACC, y, data, rocc_rd, k_DO_WRITE);
#define doRead(y, rocc_rd)                                              \
  ROCC_INSTRUCTION(XCUSTOM_ACC, y, 0, rocc_rd, k_DO_READ);
#define doLoad(y, rocc_rd, mem_addr)                                    \
  ROCC_INSTRUCTION(XCUSTOM_ACC, y, mem_addr, rocc_rd, k_DO_LOAD);
#define doAccum(y, rocc_rd, data) \
  ROCC_INSTRUCTION(XCUSTOM_ACC, y, data, rocc_rd, k_DO_ACCUM);

#endif  // SRC_MAIN_C_ACCUMULATOR_H