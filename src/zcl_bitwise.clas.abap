class ZCL_BITWISE definition
  public
  final
  create public .

public section.

  class-methods CLASS_CONSTRUCTOR .
  class-methods LEFT_SHIFT_X
    importing
      !VALUE type RAW4
      !POSITIONS type I
    returning
      value(RETURNING) type RAW4 .
  class-methods LEFT_SHIFT_I
    importing
      !VALUE type I
      !POSITIONS type I
    returning
      value(RETURNING) type I .
  class-methods RIGHT_SHIFT_X
    importing
      !VALUE type RAW4
      !POSITIONS type I
    returning
      value(RETURNING) type RAW4 .
  class-methods RIGHT_SHIFT_I
    importing
      !VALUE type I
      !POSITIONS type I
    returning
      value(RETURNING) type I .
  class-methods UNSIGNED_RIGHT_SHIFT_X
    importing
      !VALUE type RAW4
      !POSITIONS type I
    returning
      value(RETURNING) type RAW4 .
  class-methods UNSIGNED_RIGHT_SHIFT_I
    importing
      !VALUE type I
      !POSITIONS type I
    returning
      value(RETURNING) type I .
  class-methods ADD_X
    importing
      !A type RAW4
      !B type RAW4
    returning
      value(RETURNING) type RAW4 .
  class-methods ADD_I
    importing
      !A type I
      !B type I
    returning
      value(RETURNING) type I .
  class-methods SUBTRACT_X
    importing
      !A type RAW4
      !B type RAW4
    returning
      value(RETURNING) type RAW4 .
  class-methods SUBTRACT_I
    importing
      !A type I
      !B type I
    returning
      value(RETURNING) type I .
  class-methods MULTIPLY_X
    importing
      !A type RAW4
      !B type RAW4
    returning
      value(RETURNING) type RAW4 .
  class-methods MULTIPLY_I
    importing
      !A type I
      !B type I
    returning
      value(RETURNING) type I .
protected section.
private section.

  constants:
    H_00000001 type X length 4 value '00000001'. "#EC NOTEXT
  constants:
    H_3FFFFFFF type X length 4 value '3FFFFFFF'. "#EC NOTEXT
  constants:
    H_40000000 type X length 4 value '40000000'. "#EC NOTEXT
  constants:
    H_7FFFFFFF type X length 4 value '7FFFFFFF'. "#EC NOTEXT
  constants:
    H_80000000 type X length 4 value '80000000'. "#EC NOTEXT
  class-data:
    POWER_OF_2 type standard table of I .
ENDCLASS.



CLASS ZCL_BITWISE IMPLEMENTATION.


method ADD_I.
  data BYTE_A type X length 4.
  data BYTE_B type X length 4.
  BYTE_A = A.
  BYTE_B = B.
  RETURNING = ADD_X( A = BYTE_A B = BYTE_B ).
endmethod.


method ADD_X.
  data RESULT type X length 4.
  try.
      " First try regular addition
      RESULT = A + B.
    catch CX_SY_ARITHMETIC_OVERFLOW.
      " Overflow occured, perform bitwise addition
      data CARRY type X length 4.
      CARRY = A bit-and B.
      RESULT = A bit-xor B.
      while CARRY <> 0.
        data SHIFTEDCARRY type X length 4.
        SHIFTEDCARRY = LEFT_SHIFT_X( VALUE = CARRY POSITIONS = 1 ).
        CARRY = RESULT bit-and SHIFTEDCARRY.
        RESULT = RESULT bit-xor SHIFTEDCARRY.
      endwhile.
  endtry.
  RETURNING = RESULT.
endmethod.


method CLASS_CONSTRUCTOR.
  append 2 to power_of_2.
  append 4 to power_of_2.
  append 8 to power_of_2.
  append 16 to power_of_2.
  append 32 to power_of_2.
  append 64 to power_of_2.
  append 128 to power_of_2.
  append 256 to power_of_2.
  append 512 to power_of_2.
  append 1024 to power_of_2.
  append 2048 to power_of_2.
  append 4096 to power_of_2.
  append 8192 to power_of_2.
  append 16384 to power_of_2.
  append 32768 to power_of_2.
  append 65536 to power_of_2.
  append 131072 to power_of_2.
  append 262144 to power_of_2.
  append 524288 to power_of_2.
  append 1048576 to power_of_2.
  append 2097152 to power_of_2.
  append 4194304 to power_of_2.
  append 8388608 to power_of_2.
  append 16777216 to power_of_2.
  append 33554432 to power_of_2.
  append 67108864 to power_of_2.
  append 134217728 to power_of_2.
  append 268435456 to power_of_2.
  append 536870912 to power_of_2.
  append 1073741824 to power_of_2.
endmethod.


method LEFT_SHIFT_I.
  if POSITIONS = 0.
    RETURNING = VALUE.
    return.
  endif.
  data BYTE_VALUE type X length 4.
  BYTE_VALUE = VALUE.
  RETURNING = LEFT_SHIFT_X( VALUE = BYTE_VALUE POSITIONS = POSITIONS ).
endmethod.


method LEFT_SHIFT_X.
if positions = 0.
    returning = value.
    return.
  endif.
  data positions_to_shift type i.
  positions_to_shift = positions mod 32.
  if positions_to_shift > 0.
    data result type x length 4.
    try.
        " First try regular multiplication
        data a type i.
        read table power_of_2 into a index positions_to_shift.
        result = value * a.
      catch cx_sy_arithmetic_overflow.
        " Overflow occured, perform bitwise multiplication
        data calc_value type x length 4.
        calc_value = value.
        data b type x length 4.
        do positions_to_shift times.
          b = calc_value bit-and h_40000000.
          calc_value = calc_value bit-and h_3fffffff.
          calc_value = calc_value * 2.
          if b <> 0.
            calc_value = calc_value bit-or h_80000000.
          endif.
        enddo.
        result = calc_value.
    endtry.
    returning = result.
    return.
  else.
    returning = value.
    return.
  endif.

endmethod.


method MULTIPLY_I.
  data BYTE_A type X length 4.
  data BYTE_B type X length 4.
  BYTE_A = A.
  BYTE_B = B.
  RETURNING = MULTIPLY_X( A = BYTE_A B = BYTE_B ).
endmethod.


method MULTIPLY_X.
  data RESULT type X length 4.
  try.
      " First try regular multiplication
      RESULT = A * B.
    catch CX_SY_ARITHMETIC_OVERFLOW.
      " Overflow occured, perform bitwise multiplication
      data CALC_A type X length 4.
      data CALC_B type X length 4.
      CALC_A = A.
      CALC_B = B.
      RESULT = 0.
      while CALC_B <> 0.
        data CALC_C type X length 4.
        CALC_C = CALC_B bit-and H_00000001.
        if CALC_C <> 0.
          RESULT = ADD_X( A = RESULT B = CALC_A ).
        endif.
        CALC_A = LEFT_SHIFT_X( VALUE = CALC_A POSITIONS = 1 ).
        CALC_B = UNSIGNED_RIGHT_SHIFT_X( VALUE = CALC_B POSITIONS = 1 ).
      endwhile.
  endtry.
  RETURNING = RESULT.
endmethod.


method RIGHT_SHIFT_I.
  if POSITIONS = 0.
    RETURNING = VALUE.
    return.
  endif.
  data BYTE_VALUE type X length 4.
  BYTE_VALUE = VALUE.
  RETURNING = RIGHT_SHIFT_X( VALUE = BYTE_VALUE POSITIONS = POSITIONS ).
endmethod.


method RIGHT_SHIFT_X.
  if POSITIONS = 0.
    RETURNING = VALUE.
    return.
  endif.
  data POSITIONS_TO_SHIFT type I.
  POSITIONS_TO_SHIFT = POSITIONS mod 32.
  if POSITIONS_TO_SHIFT = 31.
    if VALUE < 0.
      RETURNING = -1.
      return.
    else.
      RETURNING = 0.
      return.
    endif.
  elseif POSITIONS_TO_SHIFT > 0.
    data A type I.
    read table POWER_OF_2 into A index POSITIONS_TO_SHIFT.
    RETURNING = VALUE div A.
    return.
  else.
    RETURNING = VALUE.
    return.
  endif.
endmethod.


method SUBTRACT_I.
  data BYTE_A type X length 4.
  data BYTE_B type X length 4.
  BYTE_A = A.
  BYTE_B = B.
  RETURNING = SUBTRACT_X( A = BYTE_A B = BYTE_B ).
endmethod.


method SUBTRACT_X.
  data RESULT type X length 4.
  try.
      " First try regular subtraction
      RESULT = A - B.
    catch CX_SY_ARITHMETIC_OVERFLOW.
      " Overflow occured, perform bitwise subtraction
      " a - b is the same as a + (-1 * b)
      data B_NEGATED type X length 4.
      if B = -2147483648.
        B_NEGATED = B.
      else.
        B_NEGATED = B * -1.
      endif.
      RESULT = ADD_X( A = A B = B_NEGATED ).
  endtry.
  RETURNING = RESULT.
endmethod.


method UNSIGNED_RIGHT_SHIFT_I.
  if POSITIONS = 0.
    RETURNING = VALUE.
    return.
  endif.
  data BYTE_VALUE type X length 4.
  BYTE_VALUE = VALUE.
  RETURNING = UNSIGNED_RIGHT_SHIFT_X( VALUE = BYTE_VALUE POSITIONS = POSITIONS ).
endmethod.


method UNSIGNED_RIGHT_SHIFT_X.
  if POSITIONS = 0.
    RETURNING = VALUE.
    return.
  endif.
  data POSITIONS_TO_SHIFT type I.
  POSITIONS_TO_SHIFT = POSITIONS mod 32.
  if POSITIONS_TO_SHIFT > 0.
    data CALC_VALUE type X length 4.
    data A type X length 4.
    data B type X length 4.
    CALC_VALUE = VALUE.
    A = CALC_VALUE bit-and H_7FFFFFFF.
    A = RIGHT_SHIFT_X( VALUE = A POSITIONS = POSITIONS_TO_SHIFT ).
    B = CALC_VALUE bit-and H_80000000.
    B = RIGHT_SHIFT_X( VALUE = B POSITIONS = POSITIONS_TO_SHIFT ).
    RETURNING = A - B.
    return.
  else.
    RETURNING = VALUE.
    return.
  endif.
endmethod.
ENDCLASS.
