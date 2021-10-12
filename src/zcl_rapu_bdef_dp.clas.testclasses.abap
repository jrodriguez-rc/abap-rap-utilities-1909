CLASS ltc_dp DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
      cut TYPE REF TO cl_bdef_parser.

    METHODS instantiation FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltc_dp IMPLEMENTATION.


  METHOD instantiation.

    DATA(li_dp) = zcl_rapu_bdef_dp=>create( `SADL_RAP_RS_FIELD_CONTROL` ).

    cl_abap_unit_assert=>assert_bound( li_dp ).

  ENDMETHOD.


ENDCLASS.


CLASS ltc_parser DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
      cut TYPE REF TO cl_bdef_parser.

    METHODS nodes FOR TESTING RAISING cx_static_check.

    METHODS field_mapping_lines FOR TESTING RAISING cx_static_check.

    METHODS get_bdef_patient
      RETURNING
        VALUE(result) TYPE string.

ENDCLASS.


CLASS ltc_parser IMPLEMENTATION.


  METHOD nodes.

    DATA(lo_parser) = lcl_bdef_parser=>create( get_bdef_patient( ) ).

    DATA(lt_nodes) = lo_parser->get_all_nodes( ).

    cl_abap_unit_assert=>assert_not_initial( lt_nodes ).

  ENDMETHOD.


  METHOD field_mapping_lines.

    DATA(lo_parser) = lcl_bdef_parser=>create( get_bdef_patient( ) ).

    DATA(lt_mappings) = lo_parser->get_field_mapping_for_table( iv_entity = `ZI_LAB_Request` iv_table_name = `zlab_req` ).

    cl_abap_unit_assert=>assert_equals(
        act = lines( lt_mappings )
        exp = 5 ).

    cl_abap_unit_assert=>assert_table_contains(
        line  = VALUE zif_rapu_bdef_dp=>ty_field_mapping( internal = `ID`
                                                          table    = `ID` )
        table = lt_mappings ).

    cl_abap_unit_assert=>assert_table_contains(
        line  = VALUE zif_rapu_bdef_dp=>ty_field_mapping( internal = `PATIENTID`
                                                          table    = `PATIENT_ID` )
        table = lt_mappings ).

    cl_abap_unit_assert=>assert_table_contains(
        line  = VALUE zif_rapu_bdef_dp=>ty_field_mapping( internal = `REQUESTEDAT`
                                                          table    = `REQUESTED_AT` )
        table = lt_mappings ).

    cl_abap_unit_assert=>assert_table_contains(
        line  = VALUE zif_rapu_bdef_dp=>ty_field_mapping( internal = `CHANGEDAT`
                                                          table    = `CHANGED_AT` )
        table = lt_mappings ).

    cl_abap_unit_assert=>assert_table_contains(
        line  = VALUE zif_rapu_bdef_dp=>ty_field_mapping( internal = `CHANGEDBY`
                                                          table    = `CHANGED_BY` )
        table = lt_mappings ).

  ENDMETHOD.


  METHOD get_bdef_patient.

    result =
        |unmanaged implementation in class zbp_i_lab_request unique;| &
        |{ cl_abap_char_utilities=>newline }| &
        || &
        |{ cl_abap_char_utilities=>newline }| &
        |define behavior for ZI_LAB_Request alias Request| &
        |{ cl_abap_char_utilities=>newline }| &
        |lock master| &
        |{ cl_abap_char_utilities=>newline }| &
        |etag ChangedAt| &
        |{ cl_abap_char_utilities=>newline }| &
        |\{| &
        |{ cl_abap_char_utilities=>newline }| &
        || &
        |{ cl_abap_char_utilities=>newline }| &
        |  create;| &
        |{ cl_abap_char_utilities=>newline }| &
        |  update;| &
        |{ cl_abap_char_utilities=>newline }| &
        |  delete;| &
        |{ cl_abap_char_utilities=>newline }| &
        || &
        |{ cl_abap_char_utilities=>newline }| &
        |  field ( features: instance ) Id;| &
        |{ cl_abap_char_utilities=>newline }| &
        |  field ( mandatory ) PatientId, RequestedAt;| &
        |{ cl_abap_char_utilities=>newline }| &
        |  field ( readonly ) ChangedAt, ChangedBy;| &
        |{ cl_abap_char_utilities=>newline }| &
        || &
        |{ cl_abap_char_utilities=>newline }| &
        |  action ( features : instance ) SendResults;| &
        |{ cl_abap_char_utilities=>newline }| &
        || &
        |{ cl_abap_char_utilities=>newline }| &
        |  association _Tests \{ create; \}| &
        |{ cl_abap_char_utilities=>newline }| &
        || &
        |{ cl_abap_char_utilities=>newline }| &
        |  mapping for zlab_req| &
        |{ cl_abap_char_utilities=>newline }| &
        |  \{| &
        |{ cl_abap_char_utilities=>newline }| &
        |    Id = id;| &
        |{ cl_abap_char_utilities=>newline }| &
        |    PatientId = patient_id;| &
        |{ cl_abap_char_utilities=>newline }| &
        |    RequestedAt = requested_at;| &
        |{ cl_abap_char_utilities=>newline }| &
        |    ChangedAt = changed_at;| &
        |{ cl_abap_char_utilities=>newline }| &
        |    ChangedBy = changed_by;| &
        |{ cl_abap_char_utilities=>newline }| &
        |  \}| &
        |{ cl_abap_char_utilities=>newline }| &
        || &
        |{ cl_abap_char_utilities=>newline }| &
        |\}| &
        |{ cl_abap_char_utilities=>newline }| &
        || &
        |{ cl_abap_char_utilities=>newline }| &
        |define behavior for ZI_LAB_Request_Test alias Test| &
        |{ cl_abap_char_utilities=>newline }| &
        |lock dependent ( ReqId = Id )| &
        |{ cl_abap_char_utilities=>newline }| &
        |etag ChangedAt| &
        |{ cl_abap_char_utilities=>newline }| &
        |\{| &
        |{ cl_abap_char_utilities=>newline }| &
        || &
        |{ cl_abap_char_utilities=>newline }| &
        |  update;| &
        |{ cl_abap_char_utilities=>newline }| &
        |  delete;| &
        |{ cl_abap_char_utilities=>newline }| &
        || &
        |{ cl_abap_char_utilities=>newline }| &
        |  field ( features: instance ) Id, ReqId;| &
        |{ cl_abap_char_utilities=>newline }| &
        |  field ( mandatory ) Code;| &
        |{ cl_abap_char_utilities=>newline }| &
        |  field ( readonly ) ChangedAt, ChangedBy;| &
        |{ cl_abap_char_utilities=>newline }| &
        || &
        |{ cl_abap_char_utilities=>newline }| &
        |  association _Request \{ \}| &
        |{ cl_abap_char_utilities=>newline }| &
        || &
        |{ cl_abap_char_utilities=>newline }| &
        |  mapping for zlab_req_test| &
        |{ cl_abap_char_utilities=>newline }| &
        |  \{| &
        |{ cl_abap_char_utilities=>newline }| &
        |    Id = id;| &
        |{ cl_abap_char_utilities=>newline }| &
        |    ReqId = req_id;| &
        |{ cl_abap_char_utilities=>newline }| &
        |    Code = code;| &
        |{ cl_abap_char_utilities=>newline }| &
        |    Description = description;| &
        |{ cl_abap_char_utilities=>newline }| &
        |    Value = value;| &
        |{ cl_abap_char_utilities=>newline }| &
        |    Unit = unit;| &
        |{ cl_abap_char_utilities=>newline }| &
        |    ChangedAt = changed_at;| &
        |{ cl_abap_char_utilities=>newline }| &
        |    ChangedBy = changed_by;| &
        |{ cl_abap_char_utilities=>newline }| &
        |  \}| &
        |{ cl_abap_char_utilities=>newline }| &
        || &
        |{ cl_abap_char_utilities=>newline }| &
        |\}| &
        |{ cl_abap_char_utilities=>newline }|.

  ENDMETHOD.


ENDCLASS.
