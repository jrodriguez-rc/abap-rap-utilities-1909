CLASS zcl_rapu_utl DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS get
      RETURNING
        VALUE(result) TYPE REF TO zcl_rapu_utl.

    METHODS constructor.

    METHODS change
      IMPORTING
        current TYPE any
        control TYPE any
      CHANGING
        changed TYPE any.

    METHODS fill_control_changes
      IMPORTING
        current TYPE any
        changed TYPE any
      CHANGING
        control TYPE any.

    METHODS fill_x_structure
      CHANGING
        structure TYPE any.

  PROTECTED SECTION.

  PRIVATE SECTION.
    CLASS-DATA:
      current_instance TYPE REF TO zcl_rapu_utl.

ENDCLASS.



CLASS zcl_rapu_utl IMPLEMENTATION.


  METHOD get.

    result = COND #( WHEN current_instance IS BOUND THEN current_instance ELSE NEW zcl_rapu_utl( ) ).

  ENDMETHOD.


  METHOD constructor.
    current_instance = me.
  ENDMETHOD.


  METHOD change.

    TRY.
        DATA(structdescr) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data( control ) ).
      CATCH cx_sy_move_cast_error.
        RETURN.
    ENDTRY.

    LOOP AT structdescr->get_components( ) INTO DATA(component).

      ASSIGN COMPONENT component-name OF STRUCTURE current TO FIELD-SYMBOL(<current>).
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      ASSIGN COMPONENT component-name OF STRUCTURE changed TO FIELD-SYMBOL(<changed>).
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      ASSIGN COMPONENT component-name OF STRUCTURE control TO FIELD-SYMBOL(<control>).
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      IF <control> = if_abap_behv=>mk-off.
        <changed> = <current>.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD fill_control_changes.

    TRY.
        DATA(lo_structure) =
            CAST cl_abap_structdescr(
                cl_abap_typedescr=>describe_by_data( control ) ).
      CATCH cx_sy_move_cast_error.
        RETURN.
    ENDTRY.

    LOOP AT lo_structure->get_components( ) INTO DATA(ls_component).

      ASSIGN COMPONENT ls_component-name OF STRUCTURE control TO FIELD-SYMBOL(<control_field>).
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      ASSIGN COMPONENT ls_component-name OF STRUCTURE current TO FIELD-SYMBOL(<current_field>).
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      ASSIGN COMPONENT ls_component-name OF STRUCTURE changed TO FIELD-SYMBOL(<changed_field>).
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      <control_field> =
        COND #(
            WHEN <current_field> = <changed_field>
                THEN if_abap_behv=>mk-off
                ELSE if_abap_behv=>mk-on ).

    ENDLOOP.

  ENDMETHOD.


  METHOD fill_x_structure.

    FIELD-SYMBOLS:
      <mk> TYPE if_abap_behv=>t_xflag.

    DO.

      ASSIGN COMPONENT sy-index OF STRUCTURE structure TO <mk>.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      <mk> = if_abap_behv=>mk-on.

    ENDDO.

  ENDMETHOD.


ENDCLASS.
