CLASS zcl_rapu_bdef_dp DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_rapu_bdef_dp.

    CLASS-METHODS create
      IMPORTING
        iv_bdef_name  TYPE zif_rapu_bdef_dp=>ty_bdef_name
      RETURNING
        VALUE(result) TYPE REF TO zif_rapu_bdef_dp
      RAISING
        zcx_rapu_error.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA:
      mv_bdef_name   TYPE zif_rapu_bdef_dp=>ty_bdef_name,
      mo_persist     TYPE REF TO if_wb_object_persist,
      mi_object_data TYPE REF TO if_wb_object_data_model,
      mo_parser      TYPE REF TO lcl_bdef_parser.

    METHODS initialize
      IMPORTING
        iv_bdef_name TYPE zif_rapu_bdef_dp=>ty_bdef_name
      RAISING
        zcx_rapu_error.

ENDCLASS.



CLASS zcl_rapu_bdef_dp IMPLEMENTATION.


  METHOD create.

    DATA(lo_dp) = NEW zcl_rapu_bdef_dp( ).

    lo_dp->initialize( iv_bdef_name ).

    result = lo_dp.

  ENDMETHOD.


  METHOD zif_rapu_bdef_dp~get_field_mapping_for_table.

    result = mo_parser->get_field_mapping_for_table( iv_entity = iv_entity iv_table_name = iv_table_name ).

  ENDMETHOD.


  METHOD initialize.

    DATA:
      lv_content TYPE string.

    mv_bdef_name = to_upper( iv_bdef_name ).

    DATA(ls_global_type) =
        VALUE wbobjtype(
            objtype_tr = if_bdef_adt_ressources=>co_transport_type
            subtype_wb = if_bdef_adt_ressources=>co_wb_type ).

    mo_persist ?= cl_wb_object_type=>create_access_object(
      object_type   = ls_global_type
      scope_id      = ie_objtype_scope=>c_wb_persist ).

    IF mo_persist IS NOT BOUND.
      RAISE EXCEPTION TYPE zcx_rapu_error
        EXPORTING
          textid = zcx_rapu_error=>creation_persistence_object
          text1  = |{ ls_global_type-objtype_tr } / { ls_global_type-subtype_wb }|.
    ENDIF.

    mo_persist->initialize( ls_global_type ).

    TRY.
        mo_persist->get(
          EXPORTING
            p_object_key     = CONV #( mv_bdef_name )
            p_version        = `A`
            p_data_selection = if_wb_object_persist=>c_all_data
          CHANGING
            p_object_data    = mi_object_data ).
      CATCH cx_swb_exception INTO DATA(lx_swb_exception).
        RAISE EXCEPTION TYPE zcx_rapu_error
          EXPORTING
            previous = lx_swb_exception.
    ENDTRY.

    mi_object_data->get_content( IMPORTING p_data = lv_content ).

    mo_parser = lcl_bdef_parser=>create( lv_content ).

    mo_parser->get_all_nodes( ).

  ENDMETHOD.


ENDCLASS.
