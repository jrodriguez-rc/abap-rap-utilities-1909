INTERFACE zif_rapu_unmanaged_content
  PUBLIC.

  METHODS create
    IMPORTING
      data TYPE data
    RAISING
      zcx_rapu_error.

  METHODS update
    IMPORTING
      data TYPE data
    RAISING
      zcx_rapu_error.

  METHODS delete
    IMPORTING
      data TYPE data
    RAISING
      zcx_rapu_error.

  METHODS get_data_to_create
    EXPORTING
      data TYPE table.

  METHODS get_data_to_update
    EXPORTING
      data TYPE table.

  METHODS get_data_to_delete
    EXPORTING
      data TYPE table.

ENDINTERFACE.
