#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <string.h>
#include <fcntl.h>
#include <math.h>
#include <sys/mman.h>
#include <sys/termios.h>
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>

/* #define DEBUG t */

/* start def types */

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
  TRAP_GETC =
      0x20, /* get character from keyboard, not echoed onto the terminal */
  TRAP_OUT = 0x21,   /* output a character */
  TRAP_PUTS = 0x22,  /* output a word string */
  TRAP_IN = 0x23,    /* get character from keyboard, echoed onto the terminal */
  TRAP_PUTSP = 0x24, /* output a byte string */
  TRAP_HALT = 0x25   /* halt the program */
};

enum {
  MR_KBSR = 0xFE00, /* keyboard status */
  MR_KBDR = 0xFE02  /* keyboard data */
};

enum {
  FL_POS = 1,
  FL_ZRO = 1 << 1, 
  FL_NEG = 1 << 2,
};

typedef uint16_t lcword;
typedef lcword memory[UINT16_MAX];
typedef lcword registers[R_COUNT];

/* END DEF Types */

/* HELP FUNCTIONS */

uint16_t check_key() {
  fd_set readfds;
  FD_ZERO(&readfds);
  FD_SET(STDIN_FILENO, &readfds);

  struct timeval timeout;
  timeout.tv_sec = 0;
  timeout.tv_usec = 0;
  return select(1, &readfds, NULL, NULL, &timeout) != 0;
}

struct termios original_tio;

void disable_input_buffering() {
  tcgetattr(STDIN_FILENO, &original_tio);
  struct termios new_tio = original_tio;
  new_tio.c_lflag &= ~ICANON & ~ECHO;
  tcsetattr(STDIN_FILENO, TCSANOW, &new_tio);
}

void restore_input_buffering() {
  tcsetattr(STDIN_FILENO, TCSANOW, &original_tio);
}

void handle_interrupt(int signal) {
  restore_input_buffering();
  printf("\n");
  exit(-2);
}

void mem_write(memory mem, lcword address, lcword val) {
  mem[address] = val;
}

lcword bin_to_dec(const char *binary) {
  int count = 0;
  while (binary[count++] != '\0') {
  }
  count--;
  lcword sum = 0;
  for (int cursor = 0; cursor < count; cursor++) {
    int power = count - cursor - 1;
    char current = binary[cursor];
    if (current == '1') {
      sum += pow(2, power);
    }
  }
  return sum;
}

void update_cond_reg(lcword val, registers regs) {
  if (0 == val) {
    regs[R_COND] = FL_ZRO;
  } else if (1 == (val >> 15)) {
    regs[R_COND] = FL_NEG;
  } else {
    regs[R_COND] = FL_POS;
  }
}

void init_regs(registers regs) {
  for (int cursor = 0; cursor < R_COUNT; cursor++) {
    regs[cursor] = 0;
  }
  regs[R_RUN] = 1;
}

void fail(char *message) {
  printf("Fatal> %s\n", message);
  exit(1);
}

lcword sign_extend(lcword num, int bit_count) {
  if ((num >> (bit_count - 1)) & 1) {
    num |= (0xFFFF << bit_count);
  }
  return num;
}

lcword read_mem(memory mem, lcword address) {
  if (address == MR_KBSR) {
    if (check_key()) {
      mem[MR_KBSR] = (1 << 15);
      mem[MR_KBDR] = getchar();
    } else {
      mem[MR_KBSR] = 0;
    }
  }
  return mem[address];
}

lcword sub_bin(lcword word, lcword start, lcword bits_count) {
  lcword end = bits_count + start;
  lcword word_bit_size = sizeof(lcword) * CHAR_BIT;
  lcword shift_left = word_bit_size - end;

  word = (word << shift_left);
  return word >> (shift_left + start);
}

void print_binary(lcword dec_num) {
  int max_size = sizeof(lcword) * CHAR_BIT + 1;
  char binary[max_size];
  binary[--max_size] = '\0';

  while (dec_num != 0) {
    binary[--max_size] = '0' + dec_num % 2;
    dec_num = dec_num / 2;
  }

  while (max_size > 0) {
    binary[--max_size] = '0';
  }

  printf("%s\n", binary);
}

void print_regs(registers regs) {
  printf("\n *printing regs \n");
  printf("R_R0 is: %d\n", regs[R_R0]);
  print_binary(R_R0);
  printf("R_R1 is: %d\n", regs[R_R1]);
  print_binary(R_R1);
  printf("R_R2 is: %d\n", regs[R_R2]);
  print_binary(R_R2);
  printf("R_R3 is: %d\n", regs[R_R3]);
  print_binary(R_R3);
  printf("R_R4 is: %d\n", regs[R_R4]);
  print_binary(R_R4);
  printf("R_R5 is: %d\n", regs[R_R5]);
  print_binary(R_R5);
  printf("R_R6 is: %d\n", regs[R_R6]);
  print_binary(R_R6);
  printf("R_R7 is: %d\n", regs[R_R7]);
  print_binary(R_R7);
  printf("R_PC is: %d\n", regs[R_PC]);
  print_binary(R_PC);
  printf("R_COND is: %d\n", regs[R_COND]);
  print_binary(R_COND);
  printf("R_RUN is: %d\n", regs[R_RUN]);
  print_binary(R_RUN);
  printf("R_COUNT is: %d\n", regs[R_COUNT]);
  print_binary(R_COUNT);
  printf("\n *end printing regs \n");
}

lcword swap_endian_16(lcword word) { return word >> 8 | word << 8; }

void print_memory(memory mem, registers regs) {
  for (int cursor = regs[R_PC]; cursor < regs[R_PC] + regs[R_COUNT]; cursor++) {
    print_binary(mem[cursor]);
  }
}

void init_mem(memory mem) {
  for (int cursor = 0; cursor < UINT16_MAX; cursor++) {
    mem[cursor] = 0;
  }
}

/* END HELP FUNCTIONS */

/* START DEF OPERATIONS */

// instructions
void op_fn_br(lcword inst, memory mem, registers regs) {
  lcword pc_offset = sign_extend(sub_bin(inst, 0, 9), 9);
  lcword cond_flags = sub_bin(inst, 9, 3);
  lcword p = cond_flags & 0x1;
  lcword z = (cond_flags >> 1) & 0x1;
  lcword n = (cond_flags >> 2) & 0x1;

  if ((p && (regs[R_COND] & FL_POS)) || (z && (regs[R_COND] & FL_ZRO)) ||
      (n && (regs[R_COND] & FL_NEG))) {
    regs[R_PC] += pc_offset;
  }
#ifdef DEBUG
  printf("Branching\n");
#endif
}

void op_fn_add(lcword inst, memory mem, registers regs) {
  lcword src1 = sub_bin(inst, 6, 3);
  lcword num1 = regs[src1];

  lcword mode = sub_bin(inst, 5, 1);
  lcword num2;
  if (mode) {
    num2 = sign_extend(sub_bin(inst, 0, 5), 5);
  } else {
    lcword src2 = sub_bin(inst, 0, 3);
    num2 = regs[src2];
  }

  lcword dest = sub_bin(inst, 9, 3);
  regs[dest] = num1 + num2;
  update_cond_reg(regs[dest], regs);

#ifdef DEBUG
  printf("op_fn_add\n");
#endif
}

void op_fn_ld(lcword inst, memory mem, registers regs) {
  lcword dest = sub_bin(inst, 9, 3);
  lcword offset = sign_extend(sub_bin(inst, 0, 9), 9);
  regs[dest] = read_mem(mem, regs[R_PC] + offset);
  update_cond_reg(regs[dest], regs);
#ifdef DEBUG
  printf("op_fn_ld\n");
#endif
}
void op_fn_st(lcword inst, memory mem, registers regs) {
  lcword src = sub_bin(inst, 9, 3);
  lcword offset = sign_extend(sub_bin(inst, 0, 9), 9);
  mem_write(mem, regs[R_PC] + offset, regs[src]);
#ifdef DEBUG
  printf("op_fn_st\n");
#endif
}
void op_fn_jsr(lcword inst, memory mem, registers regs) {
  regs[R_R7] = regs[R_PC];

  lcword mode = sub_bin(inst, 11, 1);
  if (mode == 1) {
    lcword offset = sign_extend(sub_bin(inst, 0, 11), 11);
    regs[R_PC] += offset;
  } else {
    regs[R_PC] = regs[sub_bin(inst, 6, 3)];
  }

#ifdef DEBUG
  printf("op_fn_jsr\n");
#endif
}
void op_fn_and(lcword inst, memory mem, registers regs) {
  lcword src1 = sub_bin(inst, 6, 3);
  lcword num1 = regs[src1];

  lcword num2;
  lcword mode = sub_bin(inst, 5, 1);
  if (mode == 1) {
    num2 = sign_extend(sub_bin(inst, 0, 5), 5);
  } else {
    lcword src2 = sub_bin(inst, 0, 2);
    num2 = regs[src2];
  }

  lcword dest = sub_bin(inst, 9, 3);
  regs[dest] = num1 & num2;
  update_cond_reg(regs[dest], regs);
#ifdef DEBUG
  printf("op_fn_and\n");
#endif
}

void op_fn_ldr(lcword inst, memory mem, registers regs) {
  lcword dest = sub_bin(inst, 9, 3);
  lcword base_r = sub_bin(inst, 6, 3);
  lcword offset = sign_extend(sub_bin(inst, 0, 6), 6);

  regs[dest] = read_mem(mem, regs[base_r] + offset);
  update_cond_reg(regs[dest], regs);
#ifdef DEBUG
  printf("op_fn_ldr\n");
#endif
}
void op_fn_str(lcword inst, memory mem, registers regs) {
  lcword src = sub_bin(inst, 9, 3);
  lcword base_r = sub_bin(inst, 6, 3);
  lcword offset = sign_extend(sub_bin(inst, 0, 6), 6);

  mem_write(mem, regs[base_r] + offset, regs[src]);
#ifdef DEBUG
  printf("op_fn_str\n");
#endif
}
void op_fn_rti(lcword inst, memory mem, registers regs) { fail("Invalid op"); }
void op_fn_not(lcword inst, memory mem, registers regs) {
  lcword src = sub_bin(inst, 6, 3);
  lcword dest = sub_bin(inst, 9, 3);
  regs[dest] = ~regs[src];
  update_cond_reg(regs[dest], regs);
#ifdef DEBUG
  printf("op_fn_not\n");
#endif
}
void op_fn_ldi(lcword inst, memory mem, registers regs) {
  lcword dest = sub_bin(inst, 9, 3);
  lcword pc_offset = sign_extend(sub_bin(inst, 0, 8), 8);
  regs[dest] = read_mem(mem, read_mem(mem, regs[R_PC] + pc_offset));
  update_cond_reg(regs[dest], regs);
#ifdef DEBUG
  printf("op_fn_ldi\n");
#endif
}
void op_fn_sti(lcword inst, memory mem, registers regs) {
  lcword src = sub_bin(inst, 9, 3);
  lcword offset = sign_extend(sub_bin(inst, 0, 9), 9);
  mem_write(mem, read_mem(mem, regs[R_PC] + offset), regs[src]);
#ifdef DEBUG
  printf("op_fn_sti\n");
#endif
}
void op_fn_jmp(lcword inst, memory mem, registers regs) {
  regs[R_PC] = regs[sub_bin(inst, 6, 3)];
#ifdef DEBUG
  printf("op_fn_jmp\n");
#endif
}
void op_fn_res(lcword inst, memory mem, registers regs) { fail("Invalid op"); }
void op_fn_lea(lcword inst, memory mem, registers regs) {
  lcword dest = sub_bin(inst, 9, 3);
  lcword offset = sign_extend(sub_bin(inst, 0, 9), 9);
  lcword tmp = regs[R_PC] + offset;
  regs[dest] = tmp;
  update_cond_reg(regs[dest], regs);
#ifdef DEBUG
  printf("op_fn_lea\n");
#endif
}


void op_fn_trap(lcword inst, memory mem, registers regs) {
  lcword trapvec = inst & 0xFF;
  switch (trapvec) {
  case TRAP_GETC:
    regs[R_R0] = getchar();
    break;
  case TRAP_OUT:
    putc(regs[R_R0] & 0xFF, stdout);
    break;
  case TRAP_PUTS:
    {
      lcword* cursor = mem + regs[R_R0];
      while(*cursor){
	putc(*cursor & 0xFF, stdout);
	cursor++;
      }
      fflush(stdout);
    }
    break;
  case TRAP_IN: 
    fprintf(stdout, "Enter a character: ");
    fflush(stdout);
    {
      lcword character = getc(stdin);
      putc(character, stdout);
      fflush(stdout);
      regs[R_R0] = character;
    }
    break;
  case TRAP_PUTSP:
    {
      lcword* cursor = mem + regs[R_R0];
      while(*cursor){
	putc((char)(*cursor & 0xFF), stdout);

	char character = (*cursor >> 8) & 0xFF;
	if(character) {
	  putc(character, stdout);
	}
	cursor++;
	fflush(stdout);
      }
    }
    break;
  case TRAP_HALT:
    fprintf(stdout, "Program halting...");
    fflush(stdout);
    regs[R_RUN]=0;
    break;
  default:
    fail("Unrecognized trapvec");
  }
}

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

/* END DEF OPERATIONS */

/* LOADING AND EXECUTING THE PROGRAM */

// Executes the program until it terminates
void exec_program(inst_funcs inst_fns, memory mem, registers regs) {
  while (regs[R_RUN]) {
    lcword inst = read_mem(mem, regs[R_PC]++);
    lcword op = inst >> 12;
#ifdef DEBUG
    printf("\nINST START> \n");
    print_binary(inst);
    printf("op is: %d \n", op);
    print_regs(regs);
#endif
    if (OP_COUNT <= op) {
      fail("Unrecognized instruction");
    }
    inst_fns[op](inst, mem, regs);
#ifdef DEBUG
    printf("done running\n");
    print_regs(regs);
    printf("\nINST END> \n");
#endif
  }
}

void copy_image_to_memory(FILE *file, memory mem, registers regs) {
  lcword origin;
  fread(&origin, sizeof(origin), 1, file);
  origin = swap_endian_16(origin);

  lcword *mem_start = mem + origin;

#ifdef DEBUG
  printf("origin %d ", origin);
#endif
  regs[R_PC] = origin;

  lcword max_read = UINT16_MAX - origin;
  size_t read = fread(mem_start, sizeof(lcword), max_read, file);
  regs[R_COUNT] = read;

  while (read-- > 0) {
    *mem_start = swap_endian_16(*mem_start);
    mem_start++;
  }
}

int load_image(const char *image_path, memory mem, registers regs) {
  FILE *file = fopen(image_path, "rb");
  if (!file)
    return 0;

  copy_image_to_memory(file, mem, regs);

  fclose(file);
  return 1;
}

/* END LOADING AND EXECUTING THE PROGRAM */
/* START TESTS */
void expect(lcword result, lcword expected, int *is_match) {
  if (result != expected) {
    printf("#### Expected result to be %d but got %d\n", expected, result);
    *is_match = 0;
  }
}

void expect_signcheck(lcword result, lcword expected, registers regs,
		      int *is_match) {
  expect(result, expected, is_match);
  lcword sign_check = (result > 0 && regs[R_COND] == FL_POS) ||
		      (result == 0 && regs[R_COND] == FL_ZRO) ||
		      (result < 0 && regs[R_COND] == FL_NEG);
  if ((result >> 15) == 1) {
    if (regs[R_COND] != FL_NEG) {
      printf(
	  "expected R_COND to be negative but it wasn't. result is %d, R_COND "
	  "is %d \n",
	  result, regs[R_COND]);
      *is_match = 0;
    }
  } else if (result == 0) {
    if (regs[R_COND] != FL_ZRO) {
      printf(
	  "expected R_COND to be zero but it wasn't. result is %d, R_COND is "
	  "%d \n",
	  result, regs[R_COND]);
      *is_match = 0;
    }
  } else if (regs[R_COND] != FL_POS) {
    printf("expected R_COND to be positive but it wasn't. result is %d, R_COND "
	   "is %d \n",
	   result, regs[R_COND]);
    *is_match = 0;
  }
}

lcword test_helper_br(int flag, int r_cond, lcword start, lcword offset,
		      memory mem, registers regs) {
  lcword inst = flag << 9 | (0x1FF & offset);
  regs[R_PC] = start;
  regs[R_COND] = r_cond;
  op_fn_br(inst, mem, regs);
  return regs[R_PC];
}

int test_op_fn_br(memory mem, registers regs) {
  int result = 1;
  expect(test_helper_br(FL_POS, FL_POS, 4, 20, mem, regs), 24, &result);
  expect(test_helper_br(FL_NEG, FL_NEG, 4, 20, mem, regs), 24, &result);
  expect(test_helper_br(FL_ZRO, FL_ZRO, 4, 20, mem, regs), 24, &result);
  expect(test_helper_br(FL_ZRO, FL_ZRO | FL_POS, 4, 20, mem, regs), 24,
	 &result);
  expect(test_helper_br(FL_ZRO, FL_ZRO | FL_POS, 30, -20, mem, regs), 10,
	 &result);

  expect(test_helper_br(FL_ZRO, FL_NEG, 30, -20, mem, regs), 30, &result);
  expect(test_helper_br(FL_POS, FL_NEG, 30, -20, mem, regs), 30, &result);
  expect(test_helper_br(FL_NEG, FL_POS, 30, -20, mem, regs), 30, &result);
  expect(test_helper_br(FL_NEG | FL_ZRO, FL_POS, 30, -20, mem, regs), 30, &result);
  return result;
}

lcword test_helper_add1(lcword num1, lcword num2, memory mem, registers regs) {
  regs[R_R0] = num1;
  regs[R_R1] = num2;
  lcword inst = 1 << 12 | R_R2 << 9 | R_R1 << 6 | R_R0;
  op_fn_add(inst, mem, regs);
  return regs[R_R2];
}

lcword test_helper_add2(lcword num1, lcword num2, memory mem, registers regs) {
  regs[R_R1] = num1;
  lcword inst = 1 << 12 | R_R2 << 9 | R_R1 << 6 | 1 << 5 | (0x1F & num2);
  op_fn_add(inst, mem, regs);
  return regs[R_R2];
}

int test_op_fn_add(memory mem, registers regs) {
  int result = 1;
  // Add 1
  expect_signcheck(test_helper_add1(11, 21, mem, regs), 32, regs, &result);
  expect_signcheck(test_helper_add1(30, 9, mem, regs), 39, regs, &result);
  expect_signcheck(test_helper_add1(28, -7, mem, regs), 21, regs, &result);
  expect_signcheck(test_helper_add1(4, -7, mem, regs), -3, regs, &result);
  expect_signcheck(test_helper_add1(0, -7, mem, regs), -7, regs, &result);
  expect_signcheck(test_helper_add1(0, 1, mem, regs), 1, regs, &result);
  expect_signcheck(test_helper_add1(-5, 1, mem, regs), -4, regs, &result);
  expect_signcheck(test_helper_add1(-5, 0, mem, regs), -5, regs, &result);

  // Add 2
  expect_signcheck(test_helper_add2(11, 3, mem, regs), 14, regs, &result);
  expect_signcheck(test_helper_add2(30, 15, mem, regs), 45, regs, &result);
  expect_signcheck(test_helper_add2(28, -9, mem, regs), 19, regs, &result);
  expect_signcheck(test_helper_add2(4, -7, mem, regs), -3, regs, &result);
  expect_signcheck(test_helper_add2(0, -7, mem, regs), -7, regs, &result);
  expect_signcheck(test_helper_add2(-7, 0, mem, regs), -7, regs, &result);

  return result;
}

int test_helper_ld(lcword dest, lcword pc, lcword offset, lcword value, memory mem, registers regs) {
  mem_write(mem, offset, value);
  lcword inst = 2 << 12 | (0x7 & dest) << 9 | (0xFF & offset);
  op_fn_ld(inst, mem, regs);
  return regs[dest];
}
int test_op_fn_ld(memory mem, registers regs) {
  int result = 1;
  expect_signcheck(test_helper_ld(R_R0, 2, 42, 20, mem, regs), 20, regs, &result);
  expect_signcheck(test_helper_ld(R_R0, 550, 102, 19, mem, regs), 19, regs, &result);
  expect_signcheck(test_helper_ld(R_R0, 550, 102, -19, mem, regs), -19, regs, &result);
  expect_signcheck(test_helper_ld(R_R0, 550, 102, 0, mem, regs), 0, regs, &result);
  return result;
}

int test_helper_st(lcword pc, lcword src, lcword offset, lcword val, memory mem, registers regs) {
  regs[R_PC] = pc;
  regs[src] = val;
  lcword inst = 3 << 12 | (7 & src) << 9 | (0x1FF & offset);
  op_fn_st(inst, mem, regs);

  return read_mem(mem, regs[R_PC] + offset);
}

int test_op_fn_st(memory mem, registers regs) {
  int result = 1;
  expect(test_helper_st(20, R_R1, 30, 200, mem, regs), 200, &result);
  expect(test_helper_st(20, R_R1, 30, -200, mem, regs), -200, &result);
  expect(test_helper_st(50, R_R1, -30, -200, mem, regs), -200, &result);
  expect(test_helper_st(50, R_R1, -30, -200, mem, regs), -200, &result);
  expect(test_helper_st(50, R_R1, -30, 0, mem, regs), 0, &result);
  return result;
}

int test_helper_jsr1(lcword offset, memory mem, registers regs) {
  lcword inst = bin_to_dec("0100") << 12 | 1 << 11 | (0x7FF & offset);
  op_fn_jsr(inst, mem, regs);
  return regs[R_PC];
}

int test_helper_jsr2(int base_r, memory mem, registers regs) {
  lcword inst = bin_to_dec("0100") << 12 | (base_r & 0x7) << 6;
  op_fn_jsr(inst, mem, regs);
  return regs[R_PC];
}

void test_helper_jsr(int base_r, lcword offset, int* result, memory mem, registers regs) {
  lcword pre = regs[R_PC];
  regs[base_r] = offset;
  expect(test_helper_jsr2(base_r, mem, regs), offset, result);
  expect(regs[R_R7], pre, result);

  expect(test_helper_jsr1(offset, mem, regs), 2*offset, result);
  expect(regs[R_R7], offset, result);
}

int test_op_fn_jsr(memory mem, registers regs) {
  int result = 1;
  test_helper_jsr(R_R3, 4, &result, mem, regs);
  test_helper_jsr(R_R0, 281, &result, mem, regs);
  test_helper_jsr(R_R5, 380, &result, mem, regs);

  int pre = 329;
  lcword offset = -55;
  printf("offset is %hu", offset);

  regs[R_PC] = pre;
  expect(test_helper_jsr1(offset, mem, regs), pre + offset, &result);
  expect(regs[R_R7], pre, &result);
  return result;
}

int test_helper_and1(lcword num1, lcword num2, memory mem, registers regs) {
  init_regs(regs);
  regs[R_R0] = num1;
  regs[R_R1] = num2;
  lcword inst = bin_to_dec("0101") << 12 | R_R2 << 9 | R_R0 << 6 | R_R1;
  op_fn_and(inst, mem, regs);
  return regs[R_R2];
}

int test_helper_and2(lcword num1, lcword num2, memory mem, registers regs) {
  /* init_regs(regs); */
  regs[R_R0] = num1;
  lcword inst =
      bin_to_dec("0101") << 12 | R_R2 << 9 | R_R0 << 6 | 1 << 5 | (0x1F & num2);
  op_fn_and(inst, mem, regs);
  return regs[R_R2];
}

int test_op_fn_and(memory mem, registers regs) {
  int result = 1;

  // Add 1
  expect_signcheck(test_helper_and1(11, 21, mem, regs), 1, regs, &result);
  expect_signcheck(test_helper_and1(30, 9, mem, regs), 8, regs, &result);
  expect_signcheck(test_helper_and1(28, -7, mem, regs), 24, regs, &result);
  expect_signcheck(test_helper_and1(4, -7, mem, regs), 0, regs, &result);
  expect_signcheck(test_helper_and1(0, -7, mem, regs), 0, regs, &result);
  expect_signcheck(test_helper_and1(0, 1, mem, regs), 0, regs, &result);
  expect_signcheck(test_helper_and1(-5, 1, mem, regs), 1, regs, &result);
  expect_signcheck(test_helper_and1(-5, 0, mem, regs), 0, regs, &result);

  // Add 2
  expect_signcheck(test_helper_and2(11, 3, mem, regs), 3, regs, &result);
  expect_signcheck(test_helper_and2(30, 15, mem, regs), 14, regs, &result);
  expect_signcheck(test_helper_and2(28, -9, mem, regs), 20, regs, &result);
  expect_signcheck(test_helper_and2(4, -7, mem, regs), 0, regs, &result);
  expect_signcheck(test_helper_and2(0, -7, mem, regs), 0, regs, &result);
  expect_signcheck(test_helper_and2(-7, 0, mem, regs), 0, regs, &result);

  return result;
}

int test_helper_ldr(lcword dest, lcword base_r, lcword base_r_val, lcword offset, lcword val, memory mem, registers regs) {
  regs[base_r] = base_r_val;
  mem_write(mem, regs[base_r] + offset, val);
  lcword inst = 6 << 12 | (0x7 & dest) << 9 | (0x7 & base_r) << 6 | (0x3f & offset);
  op_fn_ldr(inst, mem, regs);
  return regs[dest];
}

int test_op_fn_ldr(memory mem, registers regs) {
  int result = 1;
  expect_signcheck(test_helper_ldr(R_R0, R_R2, 85, 20, 200, mem, regs), 200, regs, &result);
  expect_signcheck(test_helper_ldr(R_R0, R_R2, 85, -20, 200, mem, regs), 200, regs, &result);
  expect_signcheck(test_helper_ldr(R_R0, R_R2, 85, -20, 0, mem, regs), 0, regs, &result);
  expect_signcheck(test_helper_ldr(R_R0, R_R2, 85, -20, -29, mem, regs), -29, regs, &result);
  return result;
}
int test_op_fn_str(memory mem, registers regs) { return 1; }
int test_op_fn_rti(memory mem, registers regs) { return 1; }

int test_helper_not(lcword src, lcword dest, lcword val, memory mem, registers regs) {
  regs[src] = val;
  lcword inst = 9 << 12 | dest << 9 | src << 6 | 0x3F;
  op_fn_not(inst, mem, regs);
  return regs[dest];
}

int test_op_fn_not(memory mem, registers regs) {
  int result = 1;
  expect_signcheck(test_helper_not(R_R0, R_R1, 20, mem, regs), ~20, regs, &result);
  expect_signcheck(test_helper_not(R_R0, R_R1, 30, mem, regs), ~30, regs, &result);
  expect_signcheck(test_helper_not(R_R0, R_R1, -30, mem, regs), ~-30, regs, &result);
  expect_signcheck(test_helper_not(R_R0, R_R1, -0, mem, regs), ~0, regs, &result);
  return result;
}

int test_helper_ldi(lcword dest, lcword pc, lcword poffset, lcword voffset, lcword value, memory mem, registers regs) {
  regs[R_PC] = pc;
  mem_write(mem, regs[R_PC] + poffset, voffset);
  mem_write(mem, voffset, value);

  lcword inst = 10 << 12 | (0x7 & dest) << 9 | ( 0x1FF & poffset);
  op_fn_ldi(inst, mem, regs);
  
  return regs[dest];
}

int test_op_fn_ldi(memory mem, registers regs) {
  int result = 1;
  expect_signcheck(test_helper_ldi(R_R0, 43, 60, 123, 200, mem, regs), 200, regs, &result);
  expect_signcheck(test_helper_ldi(R_R0, 43, 60, 123, -200, mem, regs), -200, regs, &result);
  expect_signcheck(test_helper_ldi(R_R0, 43, 60, 123, 0, mem, regs), 0, regs, &result);
  return result;
}
int test_op_fn_sti(memory mem, registers regs) { return 1; }

int test_helper_jmp(lcword base_r, lcword location, memory mem, registers regs) {
  regs[base_r] = location;
  lcword inst = bin_to_dec("1100") << 12 | base_r << 6;
  op_fn_jmp(inst, mem, regs);
  return regs[R_PC];
}
int test_op_fn_jmp(memory mem, registers regs) {
  int result = 1;
  expect(test_helper_jmp(R_R0, 4, mem, regs), 4, &result);
  expect(test_helper_jmp(R_R7, 0, mem, regs), 0, &result);
  expect(test_helper_jmp(R_R5, 291, mem, regs), 291, &result);
  
  return result;
}

int test_helper_lea(lcword dest, lcword pc, lcword offset,  memory mem, registers regs) {
  regs[R_PC] = pc;
  lcword inst = 14 << 12 | (0x7 & dest) << 9 | (0x1FF & offset);
  op_fn_lea(inst, mem, regs);
  return regs[dest];
}

int test_op_fn_lea(memory mem, registers regs) {
  int result = 1;
  expect_signcheck(test_helper_lea(R_R0, 10,  20, mem, regs), 10 + 20 , regs, &result);
  expect_signcheck(test_helper_lea(R_R0, 60,  -20, mem, regs), 60 - 20, regs, &result);
  return result;
}

void run_all_tests() {
  int (*all_tests[OP_COUNT])(memory, registers) = {
      test_op_fn_br,  test_op_fn_add, test_op_fn_ld,  test_op_fn_st,
      test_op_fn_jsr, test_op_fn_and, test_op_fn_ldr, test_op_fn_str,
       test_op_fn_not, test_op_fn_ldi, test_op_fn_sti,
      test_op_fn_jmp, test_op_fn_lea };

  int all_passed = 1;

  for (int cursor = 0; cursor < OP_COUNT - 3; cursor++) {
    memory mem;
    registers regs;
    init_mem(mem);
    init_regs(regs);
    printf("## Started test #%d\n", cursor);
    if (all_tests[cursor](mem, regs) == 0) {
      printf("#### TEST FAILED\n");
      all_passed = 0;
    } else {
      printf("#### TEST SUCCEEDED\n");
    }
  }
  if (all_passed != 1)
    fail("Some tests have failed");
}

/* END TESTS */

int main(int argc, const char *argv[]) {

#ifdef DEBUG
  run_all_tests();
#endif

  if (argc < 2) {
    printf("USAGE: lc3 [image1]*\n");
    exit(2);
  }

  inst_funcs inst_fns;
  memory mem;
  registers regs;

  init_inst_fns(inst_fns);
  init_mem(mem);
  init_regs(regs);

  if (!load_image(argv[1], mem, regs)) {
    fail("Loading image has failed");
  }

  signal(SIGINT, handle_interrupt);
  disable_input_buffering();
  exec_program(inst_fns, mem, regs);
  restore_input_buffering();

  return 0;
}
