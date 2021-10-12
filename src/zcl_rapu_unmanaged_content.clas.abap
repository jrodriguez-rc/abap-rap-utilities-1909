CLASS zcl_rapu_unmanaged_content DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_rapu_unmanaged_content.

    CLASS-METHODS get
      IMPORTING
        iv_bdef_name  TYPE zif_rapu_bdef_dp=>ty_bdef_name
        iv_entity     TYPE string
        iv_table      TYPE tabname
      RETURNING
        VALUE(result) TYPE REF TO zif_rapu_unmanaged_content
      RAISING
        zcx_rapu_error.

    METHODS constructor
      IMPORTING
        iv_bdef_name TYPE zif_rapu_bdef_dp=>ty_bdef_name
        iv_entity    TYPE string
        iv_table     TYPE tabname
      RAISING
        zcx_rapu_error.

  PROTECTED SECTION.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_bdef_instance,
        bdef_name TYPE zif_rapu_bdef_dp=>ty_bdef_name,
        entity    TYPE string,
        instance  TYPE REF TO zif_rapu_unmanaged_content,
      END OF ty_bdef_instance,
      ty_bdef_instances TYPE HASHED TABLE OF ty_bdef_instance
        WITH UNIQUE KEY entity
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
      gt_cid_instances TYPE ty_bdef_instances,
      gv_construction  TYPE abap_bool.

    DATA:
      mv_cid               TYPE abp_behv_cid,
      mv_bdef_name         TYPE zif_rapu_bdef_dp=>ty_bdef_name,
      mv_entity            TYPE string,
      mv_table             TYPE tabname,
      mo_structdescr       TYPE REF TO cl_abap_structdescr,
      mt_content_collected TYPE ty_content_table,
      mi_bdef_dp           TYPE REF TO zif_rapu_bdef_dp.

    METHODS build_where_from_key
      IMPORTING
        is_key        TYPE simple
      RETURNING
        VALUE(result) TYPE string.

    METHODS get_fields_from_control
      IMPORTING
        is_control    TYPE any
      RETURNING
        VALUE(result) TYPE ttfieldname.

    METHODS concatenate_key
      IMPORTING
        is_key        TYPE simple
      RETURNING
        VALUE(result) TYPE string.

    METHODS data_to_content
      IMPORTING
        ig_data       TYPE data
      RETURNING
        VALUE(result) TYPE ty_content
      RAISING
        zcx_rapu_error.

    METHODS get_data_by_key
      IMPORTING
        iv_key        TYPE string
      RETURNING
        VALUE(result) TYPE REF TO data
      RAISING
        lcx_no_exists
        lcx_deleted.

    METHODS get_data_by_crud
      IMPORTING
        iv_crud TYPE zif_rapu_crud=>ty_crud
      EXPORTING
        eg_data TYPE table.

    METHODS get_structdescr_from_name
      IMPORTING
        iv_name       TYPE tabname
      RETURNING
        VALUE(result) TYPE REF TO cl_abap_structdescr
      RAISING
        zcx_rapu_error.

    METHODS get_structdescr_from_data
      IMPORTING
        ig_data       TYPE data
      RETURNING
        VALUE(result) TYPE REF TO cl_abap_structdescr
      RAISING
        zcx_rapu_error.

    METHODS get_structdescr_from_typedescr
      IMPORTING
        io_typedescr  TYPE REF TO cl_abap_typedescr
      RETURNING
        VALUE(result) TYPE REF TO cl_abap_structdescr
      RAISING
        zcx_rapu_error.

ENDCLASS.



CLASS zcl_rapu_unmanaged_content IMPLEMENTATION.


  METHOD get.

    TRY.
        result = gt_cid_instances[ bdef_name = iv_bdef_name entity = iv_entity ]-instance.
      CATCH cx_sy_itab_line_not_found.
        result = NEW zcl_rapu_unmanaged_content( iv_bdef_name = iv_bdef_name iv_entity = iv_entity iv_table = iv_table ).
    ENDTRY.

  ENDMETHOD.


  METHOD constructor.

    mv_bdef_name = iv_bdef_name.
    mv_entity    = iv_entity.
    mv_table     = iv_table.

    mo_structdescr = get_structdescr_from_name( mv_table ).

    mi_bdef_dp = zcl_rapu_bdef_dp=>create( mv_bdef_name ).

    gt_cid_instances =
        VALUE #(
            BASE gt_cid_instances
            ( bdef_name = iv_bdef_name
              entity    = iv_entity
              instance  = me ) ).

  ENDMETHOD.


  METHOD zif_rapu_unmanaged_content~create.

    DATA(content) = data_to_content( is_data ).

    IF line_exists( mt_content_collected[ key = content-key ] ).
      RAISE EXCEPTION TYPE zcx_rapu_error
        EXPORTING
          textid = zcx_rapu_error=>content_already_collected
          text1  = content-key.
    ENDIF.

    mt_content_collected =
        VALUE #(
            BASE mt_content_collected
            ( key  = content-key
              data = content-data
              crud = zif_rapu_crud=>create ) ).

  ENDMETHOD.


  METHOD zif_rapu_unmanaged_content~read.

    DATA:
      lr_data TYPE REF TO data.
    FIELD-SYMBOLS:
      <ls_data> TYPE any.

    TRY.
        result = get_data_by_key( concatenate_key( is_key ) ).
      CATCH lcx_no_exists.
        CLEAR: result.
      CATCH lcx_deleted.
        RETURN.
    ENDTRY.

    IF result IS BOUND.
      RETURN.
    ENDIF.

    DATA(lv_select_fields) =
        REDUCE string(
            INIT lv_fields TYPE string
            FOR lv_field IN get_fields_from_control( is_control )
            NEXT lv_fields = COND #( WHEN lv_fields IS INITIAL THEN |{ lv_field }|
                                                               ELSE |{ lv_fields },{ lv_field }| ) ).

    DATA(lv_where_fields) = build_where_from_key( is_key ).

    CREATE DATA lr_data TYPE (mv_table).
    IF sy-subrc <> 0 OR lr_data IS NOT BOUND.
      RAISE EXCEPTION TYPE zcx_rapu_error.
    ENDIF.

    ASSIGN lr_data->* TO <ls_data>.
    IF sy-subrc <> 0 OR <ls_data> IS NOT ASSIGNED.
      RAISE EXCEPTION TYPE zcx_rapu_error.
    ENDIF.

    SELECT SINGLE (lv_select_fields)
      FROM (mv_table)
      WHERE (lv_where_fields)
      INTO CORRESPONDING FIELDS OF @<ls_data>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    result = lr_data.

  ENDMETHOD.


  METHOD zif_rapu_unmanaged_content~update.

    DATA(content) = data_to_content( is_data ).

    TRY.

        DATA(content_ref) = REF #( mt_content_collected[ key = content-key ] ).

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
            INTO TABLE mt_content_collected REFERENCE INTO content_ref.

    ENDTRY.

    content_ref->data = content-data.

  ENDMETHOD.


  METHOD zif_rapu_unmanaged_content~delete.

    DATA(content) = data_to_content( is_data ).

    TRY.

        DATA(content_ref) = REF #( mt_content_collected[ key = content-key ] ).

        CASE content_ref->crud.
          WHEN zif_rapu_crud=>create.
            DELETE mt_content_collected WHERE key = content-key.
            RETURN.

          WHEN zif_rapu_crud=>update.
            content_ref->crud = zif_rapu_crud=>delete.

        ENDCASE.

      CATCH cx_sy_itab_line_not_found.

        INSERT
            VALUE #( key  = content-key
                     crud = zif_rapu_crud=>delete )
            INTO TABLE mt_content_collected REFERENCE INTO content_ref.

    ENDTRY.

    content_ref->data = content-data.

  ENDMETHOD.


  METHOD zif_rapu_unmanaged_content~get_data_to_create.

    get_data_by_crud(
      EXPORTING
        iv_crud = zif_rapu_crud=>create
      IMPORTING
        eg_data = et_data ).

  ENDMETHOD.


  METHOD zif_rapu_unmanaged_content~get_data_to_update.

    get_data_by_crud(
      EXPORTING
        iv_crud = zif_rapu_crud=>update
      IMPORTING
        eg_data = et_data ).

  ENDMETHOD.


  METHOD zif_rapu_unmanaged_content~get_data_to_delete.

    get_data_by_crud(
      EXPORTING
        iv_crud = zif_rapu_crud=>delete
      IMPORTING
        eg_data = et_data ).

  ENDMETHOD.


  METHOD build_where_from_key.

    LOOP AT mi_bdef_dp->get_field_mapping_for_table( iv_entity = mv_entity iv_table_name = mv_table ) INTO DATA(ls_mapping).

      ASSIGN COMPONENT ls_mapping-internal OF STRUCTURE is_key TO FIELD-SYMBOL(<lg_value>).
      IF sy-subrc <> 0 OR <lg_value> IS NOT ASSIGNED.
        CONTINUE.
      ENDIF.

      result =
        COND #(
            LET lv_where_field = |{ ls_mapping-table } = `{ <lg_value> }`| IN
            WHEN result IS INITIAL THEN lv_where_field
                                   ELSE |{ result } AND { lv_where_field }| ).

    ENDLOOP.

  ENDMETHOD.


  METHOD get_fields_from_control.

    DATA(lt_mappings) = mi_bdef_dp->get_field_mapping_for_table( iv_entity = mv_entity iv_table_name = mv_table ).

    LOOP AT mo_structdescr->get_ddic_field_list( p_including_substructres = abap_true ) INTO DATA(ls_field).

      IF ls_field-datatype = `CLNT`.
        CONTINUE.
      ENDIF.

      DATA(ls_mapping) = lt_mappings[ KEY by_table table = ls_field-fieldname ].

      ASSIGN COMPONENT ls_mapping-internal OF STRUCTURE is_control TO FIELD-SYMBOL(<lg_control>).
      IF sy-subrc <> 0 OR ( <lg_control> <> if_abap_behv=>mk-on AND is_control IS NOT INITIAL ).
        CONTINUE.
      ENDIF.

      result = VALUE #( BASE result ( ls_mapping-table ) ).

    ENDLOOP.

  ENDMETHOD.


  METHOD concatenate_key.

    LOOP AT mo_structdescr->get_ddic_field_list( p_including_substructres = abap_true ) INTO DATA(ls_field)
        WHERE keyflag = abap_true.

      IF ls_field-datatype = `CLNT`.
        CONTINUE.
      ENDIF.

      ASSIGN COMPONENT ls_field-fieldname OF STRUCTURE is_key TO FIELD-SYMBOL(<lg_key_value>).
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      CONCATENATE result <lg_key_value>
        INTO result RESPECTING BLANKS.

    ENDLOOP.

  ENDMETHOD.


  METHOD data_to_content.

    result-key = concatenate_key( ig_data ).

    CREATE DATA result-data TYPE (mv_table).

    ASSIGN result-data->* TO FIELD-SYMBOL(<lg_reference_type>).

    <lg_reference_type> = ig_data.

  ENDMETHOD.


  METHOD get_structdescr_from_name.

    DATA(lo_typedescr) = cl_abap_typedescr=>describe_by_name( iv_name ).

    result = get_structdescr_from_typedescr( lo_typedescr ).

  ENDMETHOD.


  METHOD get_structdescr_from_data.

    DATA(lo_typedescr) = cl_abap_typedescr=>describe_by_data( ig_data ).

    result = get_structdescr_from_typedescr( lo_typedescr ).

  ENDMETHOD.


  METHOD get_structdescr_from_typedescr.

    IF ( io_typedescr->type_kind <> io_typedescr->typekind_struct1 AND
         io_typedescr->type_kind <> io_typedescr->typekind_struct2 )
       OR io_typedescr->is_ddic_type( ) = abap_false.
      RAISE EXCEPTION TYPE zcx_rapu_error
        EXPORTING
          textid = zcx_rapu_error=>data_type_not_supported
          text1  = io_typedescr->get_relative_name( ).
    ENDIF.

    TRY.
        result  = CAST cl_abap_structdescr( io_typedescr ).
      CATCH cx_sy_move_cast_error.
        RAISE EXCEPTION TYPE zcx_rapu_error
          EXPORTING
            textid = zcx_rapu_error=>data_type_not_supported
            text1  = io_typedescr->get_relative_name( ).
    ENDTRY.

  ENDMETHOD.


  METHOD get_data_by_key.

    TRY.
        DATA(ls_data) = mt_content_collected[ key = iv_key ].
      CATCH cx_sy_itab_line_not_found.
        RAISE EXCEPTION TYPE lcx_no_exists.
    ENDTRY.

    result =
        COND #(
            WHEN ls_data-crud <> zif_rapu_crud=>delete THEN ls_data-data
                                                       ELSE THROW lcx_deleted( ) ).

  ENDMETHOD.


  METHOD get_data_by_crud.

    LOOP AT FILTER #( mt_content_collected USING KEY crud WHERE crud = iv_crud ) INTO DATA(content).

      ASSIGN content-data->* TO FIELD-SYMBOL(<content_data>).
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      eg_data =
        VALUE #(
            BASE eg_data
            ( <content_data> ) ).

    ENDLOOP.

  ENDMETHOD.


ENDCLASS.
