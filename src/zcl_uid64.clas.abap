class ZCL_UID64 definition
  public
  final
  create public .

public section.

  class-methods CLASS_CONSTRUCTOR .
  class-methods CREATE
    importing
      !I_CLUSTERWORKERID type I default 1
    returning
      value(R_UID64) type STRING .
protected section.
private section.

  class-data V_ALFABET_RND type STRING .
  constants C_ALFABET type STRING value '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_-'. "#EC NOTEXT
  constants C_REDUCE_TIME type TIMESTAMPL value 20180101000000. "#EC NOTEXT
  constants C_VERSION type I value 6. "#EC NOTEXT
  class-data V_PREVIOUSSECONDS type TIMESTAMPL .

  class-methods ENCODE
    importing
      !I_POS type I
    returning
      value(R_STR) type STRING .
  class-methods RANDOMBYTE
    exporting
      value(R_VALUE) type X .
ENDCLASS.



CLASS ZCL_UID64 IMPLEMENTATION.


method CLASS_CONSTRUCTOR.

  data: LV_ALFABET      type STRING,
        LV_TEMP         type STRING,
        LV_RANDOM       type I,
        LV_COUNT        type I.

  LV_COUNT = strlen( C_ALFABET ) - 1.
  LV_ALFABET = C_ALFABET.
  while LV_COUNT gt 1.
    call function 'QF05_RANDOM_INTEGER'
      exporting
        RAN_INT_MAX = LV_COUNT
        RAN_INT_MIN = 0
      importing
        RAN_INT     = LV_RANDOM.

    concatenate V_ALFABET_RND LV_ALFABET+LV_RANDOM(1)
           into V_ALFABET_RND.
    LV_TEMP = LV_ALFABET(LV_RANDOM).
    add 1 to LV_RANDOM.
    concatenate LV_TEMP LV_ALFABET+LV_RANDOM
           into LV_ALFABET.
    subtract 1 from LV_COUNT.
  endwhile.
  concatenate V_ALFABET_RND LV_ALFABET
         into V_ALFABET_RND.
endmethod.


method CREATE.

  data: COUNTER         type I,
        TIME_STAMP      type TIMESTAMPL,
        SECONDS         type TIMESTAMPL,
        LV_VALUE        type C length 8,
        LV_ALFABET      type STRING,
        LV_ALFABET_RND  type STRING,
        LV_TEMP         type STRING,
        LV_ALFABET_LEN  type I,
        LV_RANDOM       type I,
        LV_COUNT        type I,
        LV_I            type I.

  get time stamp field TIME_STAMP.
  SECONDS = ( TIME_STAMP - C_REDUCE_TIME ) / 1000.

  if SECONDS eq V_PREVIOUSSECONDS.
    add 1 to COUNTER.
  else.
    COUNTER = 0.
    V_PREVIOUSSECONDS = SECONDS.
  endif.

  LV_TEMP = ENCODE( C_VERSION ).
  concatenate R_UID64 LV_TEMP
         into R_UID64.

  LV_TEMP = ENCODE( I_CLUSTERWORKERID ).
  concatenate R_UID64 LV_TEMP
         into R_UID64.

  if COUNTER gt 0.
    LV_TEMP = ENCODE( COUNTER ).
    concatenate R_UID64 LV_TEMP
           into R_UID64.
  endif.

  move SECONDS to LV_I.
      LV_TEMP = ENCODE( LV_I ).
  concatenate R_UID64 LV_TEMP
         into R_UID64.

endmethod.


method ENCODE.

  constants: LC_0F type X value '0F',
             LC_30 type X value '30'.

  data: LOOPCOUNTER type I value 0,
        LV_TEMP     type I,
        LV_I        type I,
        DONE        type XFELD,
        LV_X        type X,
        LV_RANDOM   type X.

  R_STR = ''.

  while DONE is initial.
    LV_TEMP = 4 * LOOPCOUNTER.
    call method ZCL_BITWISE=>RIGHT_SHIFT_I
      exporting
        VALUE     = I_POS
        POSITIONS = LV_TEMP
      receiving
        RETURNING = LV_I.
    move LV_I to LV_X.
    LV_X = LV_X bit-and LC_0F.

    RANDOMBYTE(
      IMPORTING
        R_VALUE = LV_RANDOM
    ).

    LV_RANDOM = LV_RANDOM bit-and LC_30.

    LV_X = LV_X bit-or LV_RANDOM.
    move LV_X to LV_TEMP.
    concatenate R_STR V_ALFABET_RND+LV_TEMP(1)
           into R_STR.
    LV_TEMP = LOOPCOUNTER + 1.
    LV_TEMP = 16 ** LV_TEMP.
    if I_POS lt LV_TEMP.
      DONE = 'X'.
    endif.
    add 1 to LOOPCOUNTER.
  endwhile.

endmethod.


method RANDOMBYTE.

  constants: LC_30 type X value '30'.

  data: LV_TEMP     type I.

  call function 'QF05_RANDOM_INTEGER'
    exporting
      RAN_INT_MAX = 254
      RAN_INT_MIN = 0
    importing
      RAN_INT     = LV_TEMP.
  move LV_TEMP to R_VALUE.
  R_VALUE = R_VALUE bit-and LC_30.

endmethod.
ENDCLASS.
