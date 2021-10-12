INTERFACE zif_rapu_unmanaged_content
  PUBLIC.

  TYPES:
    BEGIN OF ty_field_value,
      field TYPE fieldname,
      value TYPE string,
    END OF ty_field_value,
    ty_field_values TYPE SORTED TABLE OF ty_field_value
        WITH UNIQUE KEY field.

  METHODS create
    IMPORTING
      is_data TYPE simple
    RAISING
      zcx_rapu_error.

  METHODS read
    IMPORTING
      is_key        TYPE simple
      is_control    TYPE any OPTIONAL
    RETURNING
      VALUE(result) TYPE REF TO data
    RAISING
      zcx_rapu_error.

  METHODS update
    IMPORTING
      is_data TYPE simple
    RAISING
      zcx_rapu_error.

  METHODS delete
    IMPORTING
      is_data TYPE simple
    RAISING
      zcx_rapu_error.

  METHODS get_data_to_create
    EXPORTING
      et_data TYPE table.

  METHODS get_data_to_update
    EXPORTING
      et_data TYPE table.

  METHODS get_data_to_delete
    EXPORTING
      et_data TYPE table.

ENDINTERFACE.
