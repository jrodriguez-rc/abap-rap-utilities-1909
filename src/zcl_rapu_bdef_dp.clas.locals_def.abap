CLASS lcl_bdef_parser DEFINITION.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_node,
        index TYPE cl_rnd_grammar=>tv_index,
        name  TYPE string,
        nodes TYPE cl_art_rnd_ast_node=>tab,
      END OF ty_node,
      ty_nodes TYPE SORTED TABLE OF ty_node
        WITH UNIQUE KEY index
        WITH UNIQUE SORTED KEY by_name COMPONENTS name.

    CLASS-METHODS create
      IMPORTING
        iv_bdl_source TYPE string
      RETURNING
        VALUE(result) TYPE REF TO lcl_bdef_parser.

    METHODS get_node_type_for_name
      IMPORTING
        iv_name       TYPE string
      RETURNING
        VALUE(result) TYPE cl_rnd_grammar=>ts_ast_node_type.

    METHODS get_nodes_for_name
      IMPORTING
        iv_name       TYPE string
      RETURNING
        VALUE(result) TYPE ty_node.

    METHODS get_all_nodes
      RETURNING
        VALUE(result) TYPE ty_nodes.

    METHODS get_field_mapping_for_table
      IMPORTING
        iv_entity     TYPE string
        iv_table_name TYPE tabname
      RETURNING
        VALUE(result) TYPE zif_rapu_bdef_dp=>ty_field_mappings.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA:
      ms_ast      TYPE cl_rnd_parser=>tm_ast,
      mo_ast_util TYPE REF TO cl_art_rnd_ast_util.

    METHODS initialize
      IMPORTING
        iv_bdl_source TYPE string.

    METHODS subnode_to_field_mapping
      IMPORTING
        io_subnode    TYPE REF TO cl_art_rnd_ast_node
      RETURNING
        VALUE(result) TYPE zif_rapu_bdef_dp=>ty_field_mapping.

    METHODS get_entity_from_lower_node
      IMPORTING
        io_node       TYPE REF TO cl_art_rnd_ast_node
      RETURNING
        VALUE(result) TYPE REF TO cl_art_rnd_ast_node.

ENDCLASS.
