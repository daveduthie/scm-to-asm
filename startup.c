/* A driver for scheme_entry */
#include <stdio.h>
#include <ctype.h>
#include <string.h>

// fixnums
#define fixnum_mask  0x03 // 11
#define fixnum_tag   0x00 // 00
// empty list
#define fixnum_shift 2
// booleans
#define bool_t       0x9F // 10011111
#define bool_f       0x1F // 00011111
// characters
#define char_mask    0xFF // 11110000
#define char_tag     0x0F // 11110000
#define char_shift   8

#define empty_list   0x2F // 00101111

extern int scheme_entry();


const char *ascii_table[0x7F] = {
  "nul",    "soh",    "stx",     "etx",    "eot",    "enq",    "ack",    "bel",
  "bs",     "tab",    "newline", "vt",     "ff",     "return", "so",     "si",
  "dle",    "dc1",    "dc2",     "dc3",    "dc4",    "nak",    "syn",    "etb",
  "can",    "em",     "sub",     "esc",    "fs",     "gs",     "rs",     "us",
  "space"
};

static void prn(int val){
  if((val & fixnum_mask) == fixnum_tag){
    printf("%d", val >> fixnum_shift);
  } else if (val == bool_t){
    printf("#t");
  } else if (val == bool_f){
    printf("#f");
  } else if (val == empty_list){
    printf("()");
  } else if ((val & char_mask) == char_tag){
    char c = (val >> char_shift);
    if (iscntrl(c) || isspace(c)) {
      if ((int)c == 127) {
        printf("#\\del");
      } else {
        printf("#\\%s", ascii_table[(unsigned char)c]);
      }
    } else if (!(strcmp(&c, "\\\\"))) {
      printf("#\\\\");
    } else {
      printf("#\\%c", c);
    }
  } else {
    printf("FAIL!");
  }
  printf("\n");

}

int main(int argc, char** argv) {
  int val = scheme_entry();
  prn(val);
  return 0;
}
