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

void init_regs(registers regs) {
  regs[R_RUN]=1;
}

void fail(char *message) {
  printf("Fatal> %s\n", message);
  exit(1);
}

lcword sub_binary(lcword word, lcword start, lcword bits_count) {
  lcword end = bits_count + start;
  lcword word_bit_size = sizeof(lcword) * CHAR_BIT;
  lcword shift_left = word_bit_size - end;

  word = (word << shift_left);
  return word >> (shift_left + start);
}

// instructions
void op_fn_br(lcword inst, memory mem, registers regs) {
  printf("Branching\n");
}

void op_fn_add(lcword inst, memory mem, registers regs) {
  lcword src1 = sub_binary(inst, 6, 3);
  lcword num1 = regs[src1];
  
  lcword mode = sub_binary(inst, 5, 1);
  lcword num2;
  if(!mode){
    lcword src2 = sub_binary(inst, 0, 3);
    num2 = regs[src2];
  } else {
    num2 = sub_binary(inst, 0, 5);
  }
  
  lcword dest = sub_binary(inst, 9, 3);
  regs[dest] = num1 + num2;
  printf("op_fn_add\n");
}

void op_fn_ld(lcword inst, memory mem, registers regs) {
  printf(" op_fn_ld\n");
}
void op_fn_st(lcword inst, memory mem, registers regs) {printf("op_fn_st\n");}
void op_fn_jsr(lcword inst, memory mem, registers regs) {printf("op_fn_jsr\n");}
void op_fn_and(lcword inst, memory mem, registers regs) {printf("op_fn_and\n");}
void op_fn_ldr(lcword inst, memory mem, registers regs) {printf("op_fn_ldr\n");}
void op_fn_str(lcword inst, memory mem, registers regs) {printf("op_fn_str\n");}
void op_fn_rti(lcword inst, memory mem, registers regs) {printf("op_fn_rti\n");}
void op_fn_not(lcword inst, memory mem, registers regs) {printf("op_fn_not\n");}
void op_fn_ldi(lcword inst, memory mem, registers regs) {printf("op_fn_ldi\n");}
void op_fn_sti(lcword inst, memory mem, registers regs) {printf("op_fn_sti\n");}
void op_fn_jmp(lcword inst, memory mem, registers regs) {printf("op_fn_jmp\n");}
void op_fn_res(lcword inst, memory mem, registers regs) {printf("op_fn_res\n");}
void op_fn_lea(lcword inst, memory mem, registers regs) {printf("op_fn_lea\n");}
void op_fn_trap(lcword inst, memory mem, registers regs) {printf("op_fn_trap\n");}

// Holds a list of functions that are used to execute instructions
typedef void (*inst_funcs[OP_COUNT])(lcword, memory, registers);

void init_inst_fns(inst_funcs inst_fns) {
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

void print_binary(lcword dec_num) {
  int max_size = sizeof(lcword) * CHAR_BIT + 1;
  char binary[max_size];
  binary[--max_size]='\0';
  
  while(dec_num != 0){
    binary[--max_size] = '0' + dec_num%2;
    dec_num = dec_num / 2;
  }

  while(max_size > 0){
    binary[--max_size]='0';
  }

  printf("%s\n", binary);
}


// Executes the program until it terminates
void exec_program(inst_funcs inst_fns, memory mem, registers regs) {
  while(regs[R_RUN]){
    lcword inst = mem[regs[R_PC]++];
    printf("INST START> \n");
    print_binary(inst);
    lcword op = inst >> 12;
    if(OP_COUNT <= op){
      fail("Unrecognized instruction");
    }
    inst_fns[op](inst, mem, regs);
  }
}

lcword swap_endian_16(lcword word) {
  return word >> 8 | word << 8;
}

void copy_image_to_memory(FILE *file, memory mem, registers regs) {
  lcword origin;
  fread(&origin, sizeof(origin), 1, file);
  origin = swap_endian_16(origin);

  lcword* mem_start = mem + origin;

  printf("origin %d ", origin);
  regs[R_PC] = origin;

  lcword max_read = UINT16_MAX - origin;
  size_t read = fread(mem_start, sizeof(lcword), max_read, file);
  regs[R_COUNT] = read;

  while (read-- > 0){
    *mem_start = swap_endian_16(*mem_start);
    mem_start++;
  }
}

int load_image(const char *image_path, memory mem, registers regs) {
  FILE * file = fopen(image_path, "rb");
  if(!file) return 0; 
  
  copy_image_to_memory(file, mem, regs);

  fclose(file);
  return 1;
}

void init_mem(memory mem){
}

void print_memory(memory mem, registers regs) {
  for(int cursor=regs[R_PC]; cursor < regs[R_PC] + regs[R_COUNT]; cursor++){
    print_binary(mem[cursor]);
  }
}

int main(int argc, const char *argv[]) {
  if(argc < 2){
    printf("USAGE: lc3 [image1]*\n");
    exit(2);
  }
  
  inst_funcs inst_fns;
  memory mem;
  registers regs;

  if(!load_image(argv[1], mem, regs)){
    fail("Loading image has failed");
  }

  init_inst_fns(inst_fns);
  init_mem(mem);
  init_regs(regs);

  /* print_memory(mem, regs); */

  exec_program(inst_fns, mem, regs);

  return 0;
}
