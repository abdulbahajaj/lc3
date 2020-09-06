#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

enum {
  R_R0 = 0,
  R_R1,
  R_R2,
  R_R3,
  R_R4,
  R_R5,
  R_R6,
  R_R7,
  R_PC, /* program counter */
  R_COND,
  R_RUN, /* terminate the program if value equal to 1 */
  R_COUNT,
};

enum {
  OP_BR = 0, /* branch */
  OP_ADD,    /* add  */
  OP_LD,     /* load */
  OP_ST,     /* store */
  OP_JSR,    /* jump register */
  OP_AND,    /* bitwise and */
  OP_LDR,    /* load register */
  OP_STR,    /* store register */
  OP_RTI,    /* unused */
  OP_NOT,    /* bitwise not */
  OP_LDI,    /* load indirect */
  OP_STI,    /* store indirect */
  OP_JMP,    /* jump */
  OP_RES,    /* reserved (unused) */
  OP_LEA,    /* load effective address */
  OP_TRAP,   /* execute trap */
  OP_COUNT,  /* The number of instructions */
};

enum {
    FL_POS = 1 << 0, /* P */
    FL_ZRO = 1 << 1, /* Z */
    FL_NEG = 1 << 2, /* N */
};

typedef uint16_t lcword;
typedef lcword memory[UINT16_MAX];
typedef lcword registers[R_COUNT];

void init_mem(memory mem){}
void init_regs(registers regs) {
  regs[R_RUN]=1;
}

void fail(char *message) {
  printf("Fatal> %s\n", message);
  exit(1);
}

// operations

void op_fn_add(memory mem, registers regs) {}
void op_fn_br(memory mem, registers regs) {}
void op_fn_ld(memory mem, registers regs) {}
void op_fn_st(memory mem, registers regs) {}
void op_fn_jsr(memory mem, registers regs) {}
void op_fn_and(memory mem, registers regs) {}
void op_fn_ldr(memory mem, registers regs) {}
void op_fn_str(memory mem, registers regs) {}
void op_fn_rti(memory mem, registers regs) {}
void op_fn_not(memory mem, registers regs) {}
void op_fn_ldi(memory mem, registers regs) {}
void op_fn_sti(memory mem, registers regs) {}
void op_fn_jmp(memory mem, registers regs) {}
void op_fn_res(memory mem, registers regs) {}
void op_fn_lea(memory mem, registers regs) {}
void op_fn_trap(memory mem, registers regs) {}

// Holds a list of functions that are used to execute instructions
typedef void (*inst_funcs[OP_COUNT])(memory, registers);

void init_inst_fns(inst_funcs inst_fns) {
  inst_fns[OP_ADD] = &op_fn_add;
  inst_fns[OP_BR] = &op_fn_br;
  inst_fns[OP_ADD] = &op_fn_add;
  inst_fns[OP_LD] = &op_fn_ld;
  inst_fns[OP_ST] = &op_fn_st;
  inst_fns[OP_JSR] = &op_fn_jsr;
  inst_fns[OP_AND] = &op_fn_and;
  inst_fns[OP_LDR] = &op_fn_ldr;
  inst_fns[OP_STR] = &op_fn_str;
  inst_fns[OP_RTI] = &op_fn_rti;
  inst_fns[OP_NOT] = &op_fn_not;
  inst_fns[OP_LDI] = &op_fn_ldi;
  inst_fns[OP_STI] = &op_fn_sti;
  inst_fns[OP_JMP] = &op_fn_jmp;
  inst_fns[OP_RES] = &op_fn_res;
  inst_fns[OP_LEA] = &op_fn_lea;
  inst_fns[OP_TRAP] = &op_fn_trap;
}

void printBinary(lcword i) {
  while(i != 0){
    printf("%d", i%2);
    i = i / 2;
  }
  printf("\n");
}

// Executes the program until it terminates
void exec_program(inst_funcs inst_fns, memory mem, registers regs) {
  while(regs[R_RUN]){
    lcword inst = mem[regs[R_PC]++];
    printf("INST START> \n");
    printBinary(2);
    lcword op = inst >> 12;
    if(OP_COUNT <= op){
      fail("Unrecognized instruction");
    }
    inst_fns[op](mem, regs);
  }
}

int main(int argc, const char *argv[]) {
  inst_funcs inst_fns;
  memory mem;
  registers regs;

  init_inst_fns(inst_fns);
  init_mem(mem);
  init_regs(regs);

  exec_program(inst_fns, mem, regs);

  return 0;
}
