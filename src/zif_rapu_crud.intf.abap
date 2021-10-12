INTERFACE zif_rapu_crud
  PUBLIC.

  TYPES:
    BEGIN OF ENUM ty_crud,
      create,
      read,
      update,
      delete,
    END OF ENUM ty_crud.

ENDINTERFACE.
