CLASS zcl_rapu_unmanaged_content DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_rapu_unmanaged_content.

    CLASS-METHODS get
      IMPORTING
        cid           TYPE abp_behv_cid
        entity        TYPE string
      RETURNING
        VALUE(result) TYPE REF TO zif_rapu_unmanaged_content.

    METHODS constructor
      IMPORTING
        cid    TYPE abp_behv_cid
        entity TYPE string.

  PROTECTED SECTION.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_cid_instance,
        cid      TYPE abp_behv_cid,
        entity   TYPE string,
        instance TYPE REF TO zif_rapu_unmanaged_content,
      END OF ty_cid_instance,
      ty_cid_instances TYPE HASHED TABLE OF ty_cid_instance
        WITH UNIQUE KEY cid entity
        WITH UNIQUE HASHED KEY instance COMPONENTS instance.

    TYPES:
      BEGIN OF ty_content,
        key  TYPE string,
        data TYPE REF TO data,
        crud TYPE zif_rapu_crud=>ty_crud,
      END OF ty_content,
      ty_content_table TYPE HASHED TABLE OF ty_content
        WITH UNIQUE KEY key
        WITH NON-UNIQUE SORTED KEY crud COMPONENTS crud.

    CLASS-DATA:
      cid_instances TYPE ty_cid_instances.

    DATA:
      cid               TYPE abp_behv_cid,
      entity            TYPE string,
      content_collected TYPE ty_content_table.

    METHODS data_to_content
      IMPORTING
        data          TYPE data
      RETURNING
        VALUE(result) TYPE ty_content
      RAISING
        zcx_rapu_error.

    METHODS get_data_by_crud
      IMPORTING
        crud TYPE zif_rapu_crud=>ty_crud
      EXPORTING
        data TYPE table.

ENDCLASS.



CLASS zcl_rapu_unmanaged_content IMPLEMENTATION.


  METHOD get.

    TRY.
        result = cid_instances[ cid = cid entity = entity ]-instance.
      CATCH cx_sy_itab_line_not_found.
        result = NEW zcl_rapu_unmanaged_content( cid = cid entity = entity ).
    ENDTRY.

  ENDMETHOD.


  METHOD constructor.

    me->cid    = cid.
    me->entity = entity.

    cid_instances =
        VALUE #(
            BASE cid_instances
            ( cid      = cid
              entity   = entity
              instance = me ) ).

  ENDMETHOD.


  METHOD zif_rapu_unmanaged_content~create.

    DATA(content) = data_to_content( data ).

    IF line_exists( content_collected[ key = content-key ] ).
      RAISE EXCEPTION TYPE zcx_rapu_error
        EXPORTING
          textid = zcx_rapu_error=>content_already_collected
          text1  = content-key.
    ENDIF.

    content_collected =
        VALUE #(
            BASE content_collected
            ( key  = content-key
              data = content-data
              crud = zif_rapu_crud=>create ) ).

  ENDMETHOD.


  METHOD zif_rapu_unmanaged_content~update.

    DATA(content) = data_to_content( data ).

    TRY.

        DATA(content_ref) = REF #( content_collected[ key = content-key ] ).

        IF content_ref->crud = zif_rapu_crud=>delete.
          RAISE EXCEPTION TYPE zcx_rapu_error
            EXPORTING
              textid = zcx_rapu_error=>content_collected_delete
              text1  = content-key.
        ENDIF.

      CATCH cx_sy_itab_line_not_found.

        INSERT
            VALUE #( key  = content-key
                     crud = zif_rapu_crud=>update )
            INTO TABLE content_collected REFERENCE INTO content_ref.

    ENDTRY.

    content_ref->data = content-data.

  ENDMETHOD.


  METHOD zif_rapu_unmanaged_content~delete.

    DATA(content) = data_to_content( data ).

    TRY.

        DATA(content_ref) = REF #( content_collected[ key = content-key ] ).

        CASE content_ref->crud.
          WHEN zif_rapu_crud=>create.
            DELETE content_collected WHERE key = content-key.
            RETURN.

          WHEN zif_rapu_crud=>update.
            content_ref->crud = zif_rapu_crud=>delete.

        ENDCASE.

      CATCH cx_sy_itab_line_not_found.

        INSERT
            VALUE #( key  = content-key
                     crud = zif_rapu_crud=>delete )
            INTO TABLE content_collected REFERENCE INTO content_ref.

    ENDTRY.

    content_ref->data = content-data.

  ENDMETHOD.


  METHOD zif_rapu_unmanaged_content~get_data_to_create.

    get_data_by_crud(
      EXPORTING
        crud = zif_rapu_crud=>create
      IMPORTING
        data = data ).

  ENDMETHOD.


  METHOD zif_rapu_unmanaged_content~get_data_to_update.

    get_data_by_crud(
      EXPORTING
        crud = zif_rapu_crud=>update
      IMPORTING
        data = data ).

  ENDMETHOD.


  METHOD zif_rapu_unmanaged_content~get_data_to_delete.

    get_data_by_crud(
      EXPORTING
        crud = zif_rapu_crud=>delete
      IMPORTING
        data = data ).

  ENDMETHOD.


  METHOD data_to_content.

    DATA(typedescr) = cl_abap_typedescr=>describe_by_data( data ).

    DATA(type_name) = typedescr->get_relative_name( ).

    IF typedescr->type_kind <> typedescr->typekind_struct2 OR typedescr->is_ddic_type( ) <> abap_true.
      RAISE EXCEPTION TYPE zcx_rapu_error
        EXPORTING
          textid = zcx_rapu_error=>data_type_not_supported
          text1  = type_name.
    ENDIF.

    TRY.
        DATA(structdescr) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data( data ) ).
      CATCH cx_sy_move_cast_error.
        RAISE EXCEPTION TYPE zcx_rapu_error
          EXPORTING
            textid = zcx_rapu_error=>data_type_not_supported
            text1  = type_name.
    ENDTRY.

    LOOP AT structdescr->get_ddic_field_list( p_including_substructres = abap_true ) INTO DATA(field)
        WHERE keyflag = abap_true.

      ASSIGN COMPONENT field-fieldname OF STRUCTURE data TO FIELD-SYMBOL(<key_value>).
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      CONCATENATE result-key <key_value>
        INTO result-key RESPECTING BLANKS.

    ENDLOOP.

    CREATE DATA result-data TYPE (type_name).

    ASSIGN result-data->* TO FIELD-SYMBOL(<reference_type>).

    <reference_type> = data.

  ENDMETHOD.


  METHOD get_data_by_crud.

    LOOP AT FILTER #( content_collected USING KEY crud WHERE crud = crud ) INTO DATA(content).

      ASSIGN content-data->* TO FIELD-SYMBOL(<content_data>).
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      data =
        VALUE #(
            BASE data
            ( <content_data> ) ).

    ENDLOOP.

  ENDMETHOD.


ENDCLASS.
