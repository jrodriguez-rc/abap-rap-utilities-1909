CLASS zcx_rapu_error DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES:
      if_t100_dyn_msg,
      if_t100_message.

    CONSTANTS:
      " Data type &1 is not supported.
      BEGIN OF data_type_not_supported,
        msgid TYPE symsgid VALUE `ZRAPU`,
        msgno TYPE symsgno VALUE `001`,
        attr1 TYPE scx_attrname VALUE `TEXT1`,
        attr2 TYPE scx_attrname VALUE ``,
        attr3 TYPE scx_attrname VALUE ``,
        attr4 TYPE scx_attrname VALUE ``,
      END OF data_type_not_supported.

    CONSTANTS:
      " Content with key &1 is already collected.
      BEGIN OF content_already_collected,
        msgid TYPE symsgid VALUE `ZRAPU`,
        msgno TYPE symsgno VALUE `002`,
        attr1 TYPE scx_attrname VALUE `TEXT1`,
        attr2 TYPE scx_attrname VALUE ``,
        attr3 TYPE scx_attrname VALUE ``,
        attr4 TYPE scx_attrname VALUE ``,
      END OF content_already_collected.

    CONSTANTS:
      " Content with key &1 is collected to delete.
      BEGIN OF content_collected_delete,
        msgid TYPE symsgid VALUE `ZRAPU`,
        msgno TYPE symsgno VALUE `003`,
        attr1 TYPE scx_attrname VALUE `TEXT1`,
        attr2 TYPE scx_attrname VALUE ``,
        attr3 TYPE scx_attrname VALUE ``,
        attr4 TYPE scx_attrname VALUE ``,
      END OF content_collected_delete.

    DATA:
      text1 TYPE string,
      text2 TYPE string,
      text3 TYPE string,
      text4 TYPE string.

    METHODS constructor
      IMPORTING
        textid   LIKE if_t100_message=>t100key OPTIONAL
        text1    LIKE text1 OPTIONAL
        text2    LIKE text2 OPTIONAL
        text3    LIKE text3 OPTIONAL
        text4    LIKE text4 OPTIONAL
        previous LIKE previous OPTIONAL.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS zcx_rapu_error IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.

    super->constructor( previous = previous ).

    CLEAR me->textid.

    if_t100_message~t100key =
        COND #(
            WHEN textid IS INITIAL
                THEN if_t100_message=>default_textid
                ELSE textid ).

    me->text1 = text1.
    me->text2 = text2.
    me->text3 = text3.
    me->text4 = text4.

  ENDMETHOD.


ENDCLASS.
