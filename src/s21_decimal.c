#include "s21_decimal.h"
#include "s21_decbits.h"
#include <math.h>

#define EXP_MAX_FLOAT 1e-28
#define DEC_MAX_FLOAT 79228162514264337593543950335.0f
#define FLOAT_FRAC_PRECISION 1e-7

enum float_frac { FLOAT_PRECISION = 8 };
enum sign { POSITIVE, NEGATIVE };
enum compare { LESS, EQUAL, GREATER };

static int bit_add(s21_decimal value_1, s21_decimal value_2, s21_decimal* result) {
  *result = init_bits();
  int ret = OK, rem = 0;
  for (int i = 0; i <= LAST_BIT_MANTISSA; i++) {
    int bit_val_1 = get_bit(value_1, i);
    int bit_val_2 = get_bit(value_2, i);
    if (!bit_val_1 && !bit_val_2) {
      if (rem) {
        set_bit(result, i);
        rem = 0;
      }
    } else if (bit_val_1 != bit_val_2) {
      if (rem) {
        rem = 1;
      } else {
        set_bit(result, i);
      }
    } else {
      if (rem) {
        set_bit(result, i);
      }
      rem = 1;
    }
    if (i == LAST_BIT_MANTISSA && rem) {
      ret = INF;
    }
  }
  return ret;
}

static int bit_sub(s21_decimal value_1, s21_decimal value_2, s21_decimal* result) {
  *result = init_bits();
  int ret = OK;
  if (!s21_is_equal(value_1, value_2)) {
    int rem = 0;
    for (int i = 0; i <= last_bit(value_1); i++) {
      int bit_val_1 = get_bit(value_1, i);
      int bit_val_2 = get_bit(value_2, i);
      if (!bit_val_1 && !bit_val_2) {
        if (rem) {
          rem = 1;
          set_bit(result, i);
        }
      } else if (bit_val_1 && !bit_val_2) {
        if (rem) {
          rem = 0;
        } else {
          set_bit(result, i);
        }
      } else if (!bit_val_1) {
        if (!rem) {
          set_bit(result, i);
        }
        rem = 1;
      } else if (rem) {
        rem = 1;
        set_bit(result, i);
      }
      if (i == LAST_BIT_MANTISSA && rem) {
        ret = INF;
      }
    }
  }
  return ret;
}

static void bit_div(s21_decimal dividend, s21_decimal divisor, s21_decimal* result,
             s21_decimal* buf) {
  *buf = init_bits();
  *result = init_bits();
  for (int i = last_bit(dividend); i >= 0; i--) {
    if (get_bit(dividend, i)) {
      set_bit(buf, 0);
    }
    offset_left(result, 1);
    if (s21_is_greater_or_equal(*buf, divisor)) {
      bit_sub(*buf, divisor, buf);
      set_bit(result, 0);
    }
    if (i) {
      offset_left(buf, 1);
    }
  }
}

static inline
int get_float_exp(float src) {
  int f_exp;
  frexpf(src, &f_exp);
  return --f_exp;
}

static inline
int count_digits(double f) { return (int)log10(f) + 1; }

static inline
void dec_mul_10(s21_decimal *value) {
  s21_decimal ten = {{10}};
  s21_mul(*value, ten, value);
}

static inline
void dec_div_10(s21_decimal *value) {
  s21_decimal ten = {{10}}, rem = init_bits();
  bit_div(*value, ten, value, &rem);
}

static inline
int dec_is_zero(s21_decimal *value) {
  int is_zero = 1;
  for (int i = LOW; i <= HIGH; i++) {
    if (value->bits[i]) {
      is_zero = 0;
      break;
    }
  }
  return is_zero;
}

static inline
int f_dec_border(float src) {
  return (fabsf(src) > 0 && fabsf(src) < EXP_MAX_FLOAT) || src > DEC_MAX_FLOAT;
}

static void equal_scale(s21_decimal *value_1, s21_decimal *value_2) {
  int scale_1 = get_scale(*value_1);
  int scale_2 = get_scale(*value_2);
  int scale_dif = scale_1 - scale_2;
  if (scale_dif < 0) {
    if (last_bit(*value_2) < MUL10_BORDER) {
      for (; scale_dif; scale_dif++) {
        dec_mul_10(value_1);
      }
      set_scale(value_1, scale_2);
    } else {
      for (; scale_dif; scale_dif++) {
        dec_div_10(value_2);
      }
      set_scale(value_2, scale_1);
    }
  } else if (scale_dif > 0) {
    if (last_bit(*value_1) < MUL10_BORDER) {
      for (; scale_dif; scale_dif--) {
        dec_mul_10(value_2);
      }
      set_scale(value_2, scale_1);
    } else {
      for (; scale_dif; scale_dif--) {
        dec_div_10(value_1);
      }
      set_scale(value_1, scale_2);
    }
  }
}

static int dec_cmp(s21_decimal value_1, s21_decimal value_2) {
  int ret = EQUAL;
  if (dec_is_zero(&value_1)) {
    set_sign(&value_1, POSITIVE);
  }
  if (dec_is_zero(&value_2)) {
    set_sign(&value_2, POSITIVE);
  }
  int sign_1 = get_sign(value_1), sign_2 = get_sign(value_2);
  int neg_both = (sign_1 && sign_2);
  if (sign_1 && !sign_2) {
    ret = LESS;
  } else if (!sign_1 && sign_2) {
    ret = GREATER;
  } else {
    equal_scale(&value_1, &value_2);
    for (int i = LAST_BIT_MANTISSA; i >= 0; i--) {
      if (get_bit(value_1, i) && !get_bit(value_2, i)) {
        if (neg_both) {
          ret = LESS;
        } else {
          ret = GREATER;
        }
        break;
      } else if (!get_bit(value_1, i) && get_bit(value_2, i)) {
        if (neg_both) {
          ret = GREATER;
        } else {
          ret = LESS;
        }
        break;
      }
    }
  }
  return ret;
}

int s21_is_less(s21_decimal value_1, s21_decimal value_2) {
  return dec_cmp(value_1, value_2) == LESS;
}

int s21_is_less_or_equal(s21_decimal value_1, s21_decimal value_2) {
  return dec_cmp(value_1, value_2) <= EQUAL;
}

int s21_is_greater(s21_decimal value_1, s21_decimal value_2) {
  return dec_cmp(value_1, value_2) == GREATER;
}

int s21_is_greater_or_equal(s21_decimal value_1, s21_decimal value_2) {
  return dec_cmp(value_1, value_2) >= EQUAL;
}

int s21_is_equal(s21_decimal value_1, s21_decimal value_2) {
  return dec_cmp(value_1, value_2) == EQUAL;
}

int s21_is_not_equal(s21_decimal value_1, s21_decimal value_2) {
  return dec_cmp(value_1, value_2) != EQUAL;
}

int s21_add(s21_decimal value_1, s21_decimal value_2, s21_decimal *result) {
  *result = init_bits();
  int ret = OK;
  if (!get_sign(value_1) && !get_sign(value_2)) {
    equal_scale(&value_1, &value_2);
    int bit_err;
    s21_decimal tmp = init_bits();
    bit_err = bit_add(value_1, value_2, &tmp);
    if (bit_err == INF) {
      ret = INF;
    } else {
      *result = tmp;
      result->bits[SCALE] = value_2.bits[SCALE];
    }
  } else if (get_sign(value_1) && !get_sign(value_2)) {
    set_sign(&value_1, POSITIVE);
    ret = s21_sub(value_2, value_1, result);
  } else if (!get_sign(value_1) && get_sign(value_2)) {
    set_sign(&value_2, POSITIVE);
    ret = s21_sub(value_1, value_2, result);
  } else {
    set_sign(&value_1, POSITIVE);
    set_sign(&value_2, POSITIVE);
    ret = s21_add(value_1, value_2, result);
    if (ret == INF) {
      ret = N_INF;
    } else {
      set_sign(result, NEGATIVE);
    }
  }
  return ret;
}

int s21_sub(s21_decimal value_1, s21_decimal value_2, s21_decimal *result) {
  *result = init_bits();
  int ret = OK;
  if (get_scale(value_1) != get_scale(value_2)) {
    equal_scale(&value_1, &value_2);
  }
  int sign;
  if (get_sign(value_1) != get_sign(value_2)) {
    sign = get_sign(value_1);
    set_sign(&value_1, POSITIVE);
    set_sign(&value_2, POSITIVE);
    ret = s21_add(value_1, value_2, result);
    if (ret == INF) {
      ret = N_INF;
    } else {
      set_sign(result, sign);
    }
  } else if (!s21_is_equal(value_1, value_2)) {
    int sign_1 = get_sign(value_1);
    int sign_2 = get_sign(value_2);
    set_sign(&value_1, POSITIVE);
    set_sign(&value_2, POSITIVE);
    s21_decimal *ptr_less, *ptr_gr;
    if (s21_is_less(value_1, value_2)) {
      ptr_less = &value_1;
      ptr_gr = &value_2;
      sign = !sign_2;
    } else {
      ptr_less = &value_2;
      ptr_gr = &value_1;
      sign = sign_1;
    }
    bit_sub(*ptr_gr, *ptr_less, result);
    set_scale(result, get_scale(value_1));
    set_sign(result, sign);
  }
  return ret;
}

int s21_mul(s21_decimal value_1, s21_decimal value_2, s21_decimal *result) {
  *result = init_bits();
  int ret = OK, add_err = OK, off_err = OK;
  int sign = (get_sign(value_1) != get_sign(value_2));
  s21_decimal tmp = {{0}};
  for (int i = 0; i <= last_bit(value_1); i++) {
    tmp = init_bits();
    int val_bit_1 = get_bit(value_1, i);
    if (val_bit_1) {
      tmp = value_2;
      off_err = offset_left(&tmp, i);
      add_err = bit_add(*result, tmp, result);
    }
  }
  if (add_err == INF || off_err == INF) {
    ret = (sign) ? N_INF : INF;
    *result = init_bits();
  } else {
    int scale = get_scale(value_1) + get_scale(value_2);
    set_scale(result, scale);
    set_sign(result, sign);
  }
  return ret;
}



int s21_div(s21_decimal dividend, s21_decimal divisor, s21_decimal *result) {
  int ret = OK;
  *result = init_bits();
  if (dec_is_zero(&divisor)) {
    ret = N_NAN;
  } else {
    int sign = (get_sign(dividend) != get_sign(divisor));
    int diff_scale = get_scale(dividend) - get_scale(divisor);
    s21_decimal rem = {{0}}, tmp = {{0}};
    dividend.bits[SCALE] = divisor.bits[SCALE] = 0;
    bit_div(dividend, divisor, &tmp, &rem);
    *result = tmp;
    const s21_decimal DEC_DIV_MAX = {
        {[LOW... HIGH] = (int)MAX_32_BIT, [SCALE] = SCALE_1}};
    int num_scale = 0;
    for (; num_scale < EXP_MAX && !dec_is_zero(&rem); num_scale++) {
      if (s21_is_greater_or_equal(*result, DEC_DIV_MAX)) {
        break;
      }
      dec_mul_10(&rem);
      bit_div(rem, divisor, &tmp, &rem);
      dec_mul_10(result);
      s21_add(*result, tmp, result);
    }
    int scale = diff_scale + num_scale;
    for (; scale > EXP_MAX; scale--) {
      dec_div_10(result);
    }
    for (; scale < 0; scale++) {
      dec_mul_10(result);
    }
    set_scale(result, scale);
    set_sign(result, sign);
  }
  return ret;
}

int s21_mod(s21_decimal value_1, s21_decimal value_2, s21_decimal *result) {
  *result = init_bits();
  int ret = OK;
  int scale_1 = get_scale(value_1);
  int scale_2 = get_scale(value_2);
  int scale = (scale_1 > scale_2) ? scale_1 : scale_2;
  if (dec_is_zero(&value_2)) {
    ret = N_NAN;
  } else {
    int sign = get_sign(value_1);
    equal_scale(&value_1, &value_2);
    value_1.bits[SCALE] = value_2.bits[SCALE] = 0;
    bit_div(value_1, value_2, &value_1, result);
    set_scale(result, scale);
    set_sign(result, sign);
  }
  return ret;
}

int s21_from_int_to_decimal(int src, s21_decimal *dst) {
  int ret = SUCCESS;
  if (dst) {
    int neg = (src < 0);
    src = (neg) ? -src : src;
    *dst = init_bits();
    if (src) {
      dst->bits[LOW] = src;
    }
    if (neg) {
      set_sign(dst, neg);
    }
  } else {
    ret = CONVERT_ERR;
  }
  return ret;
}

int s21_from_decimal_to_int(s21_decimal src, int *dst) {
  *dst = 0;
  int ret = s21_truncate(src, &src);
  if (ret != OK || src.bits[MID] || src.bits[HIGH] ||
      get_scale(src) > EXP_MAX) {
    ret = CONVERT_ERR;
  } else {
    *dst = src.bits[LOW];
    for (int i = get_scale(src); i > 0; i--) {
      *dst /= 10;
    }
    if (get_sign(src)) {
      *dst = -*dst;
    }
  }
  return ret;
}

int s21_from_float_to_decimal(float src, s21_decimal *dst) {
  *dst = init_bits();
  int ret = SUCCESS;
  if (isnan(src) || isinf(src) || f_dec_border(src)) {
    ret = CONVERT_ERR;
  } else {
    int neg = (src < 0), scale = 0;
    double tmp = fabsf(src);
    int digits = count_digits(tmp);
    int digits_left = digits - FLOAT_PRECISION;
    if (digits_left > 0) {
      for (; digits > FLOAT_PRECISION; tmp /= 10.0, digits--)
        ;
    } else {
      for (double fp_int; digits < FLOAT_PRECISION;
           tmp *= 10.0, digits++, scale++) {
        if (modf(tmp, &fp_int) < FLOAT_FRAC_PRECISION) {
          break;
        }
      }
    }
    tmp = round(tmp);
    float f = (float)tmp;
    int exp = get_float_exp(f);
    long unsigned int f_bits = *((long unsigned int *)&f);
    set_bit(dst, exp--);
    for (long unsigned int mask = FLOAT_23TH_BIT; mask; mask >>= 1, exp--) {
      if (f_bits & mask) {
        set_bit(dst, exp);
      }
    }
    for (; digits_left > 0; dec_mul_10(dst), digits_left--)
      ;
    if (neg) {
      set_sign(dst, neg);
    }
    set_scale(dst, scale);
  }
  return ret;
}

int s21_from_decimal_to_float(s21_decimal src, float *dst) {
  double temp = 0;
  int ret = SUCCESS;
  if (get_scale(src) > EXP_MAX) {
    ret = CONVERT_ERR;
  } else {
    for (int i = 0; i <= LAST_BIT_MANTISSA; i++) {
      if (get_bit(src, i)) {
        temp += pow(2, i);
      }
    }
    for (int i = get_scale(src); i > 0; i--) {
      temp /= 10.0;
    }
    *dst = (float)temp;
    if (get_sign(src)) {
      *dst = -*dst;
    }
  }
  return ret;
}

int s21_negate(s21_decimal value, s21_decimal *result) {
  *result = value;
  if (get_sign(*result)) {
    set_sign(result, POSITIVE);
  } else {
    set_sign(result, NEGATIVE);
  }
  return OK;
}

int s21_truncate(s21_decimal value, s21_decimal *result) {
  int sign = get_sign(value);
  for (int scale = get_scale(value); scale; scale--) {
    dec_div_10(&value);
  }
  value.bits[SCALE] = 0;
  *result = value;
  set_sign(result, sign);
  return OK;
}

int s21_round(s21_decimal value, s21_decimal *result) {
  int error = OK, sign = get_sign(value);
  *result = init_bits();
  set_sign(&value, POSITIVE);
  s21_decimal frac_part = init_bits(), int_part = init_bits();
  s21_truncate(value, &int_part);
  s21_sub(value, int_part, &frac_part);
  const s21_decimal half = {{[LOW] = 5, [SCALE] = SCALE_1}},
      one = {{[LOW] = 1}};
  if (s21_is_greater_or_equal(frac_part, half)) {
    error = s21_add(int_part, one, result);
  } else {
    *result = int_part;
  }
  set_sign(result, sign);
  return error;
}

int s21_floor(s21_decimal value, s21_decimal *result) {
  int error = OK;
  *result = init_bits();
  s21_truncate(value, result);
  if (get_sign(value)) {
    const s21_decimal one = {{[LOW] = 1}};
    error = s21_sub(*result, one, result);
  }
  return error;
}
