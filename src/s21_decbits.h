#ifndef S21_DECBITS_H_
#define S21_DECBITS_H_

#include "s21_decimal.h"

enum dec_bits { LOW, MID, HIGH, SCALE };

enum limits { EXP_MAX = 28, MUL10_BORDER = 93, LAST_BIT_MANTISSA = 95 };

enum masks {
  SCALE_MASK = 0xFF,
  SCALE_1 = 0x10000,
  FLOAT_23TH_BIT = 0x400000,
  MAX_32_BIT = 0xFFFFFFFF
};

enum int_32_bits { INT_LAST_BIT = 31, INT_BIT_WIDTH };

enum dec_bit_idx {
  SCALE_INDEX_0 = 16,
  LAST_BIT_LOW = 31,
  FIRST_BIT_MID,
  LAST_BIT_MID = 63,
  FIRST_BIT_HIGH
};

static inline int get_bit(s21_decimal value, int bit) {
  unsigned int mask = 1u << (bit % INT_BIT_WIDTH);
  return (value.bits[bit / INT_BIT_WIDTH] & (int)mask) != 0;
}

static inline void set_bit(s21_decimal *value, int bit) {
  unsigned int mask = 1u << (bit % INT_BIT_WIDTH);
  value->bits[bit / INT_BIT_WIDTH] |= (int)mask;
}

static inline int get_sign(s21_decimal value) {
  unsigned int mask = 1u << INT_LAST_BIT;
  return (value.bits[SCALE] & (int)mask) != 0;
}

static inline void set_sign(s21_decimal *value, int sign) {
  unsigned int mask = 1u << INT_LAST_BIT;
  if (sign) {
    value->bits[SCALE] |= (int)mask;
  } else {
    value->bits[SCALE] &= (int)~mask;
  }
}

static inline int get_scale(s21_decimal value) {
  return (char)(value.bits[SCALE] >> SCALE_INDEX_0);
}

static inline void set_scale(s21_decimal *value, int scale) {
  int clear_mask = ~(SCALE_MASK << SCALE_INDEX_0);
  value->bits[SCALE] &= clear_mask;
  int mask = scale << SCALE_INDEX_0;
  value->bits[SCALE] |= mask;
}

static inline s21_decimal init_bits() {
  return (s21_decimal) {};
}

static inline int last_bit(s21_decimal value) {
  int last_b = LAST_BIT_MANTISSA;
  for (; last_b && !get_bit(value, last_b); last_b--)
    ;
  return last_b;
}

static inline void shift_left(s21_decimal *value) {
  int bit_31 = get_bit(*value, LAST_BIT_LOW);
  int bit_63 = get_bit(*value, LAST_BIT_MID);
  for (int j = LOW; j <= HIGH; value->bits[j] <<= 1, j++)
    ;
  if (bit_31) {
    set_bit(value, FIRST_BIT_MID);
  }
  if (bit_63) {
    set_bit(value, FIRST_BIT_HIGH);
  }
}

static inline int offset_left(s21_decimal *value, int offset) {
  int ret = OK;
  if (last_bit(*value) + offset > LAST_BIT_MANTISSA) {
    ret = INF;
  } else {
    for (int i = 0; i < offset; i++) {
      shift_left(value);
    }
  }
  return ret;
}

#endif // S21_DECBITS_H_