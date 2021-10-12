CLASS lcl_bdef_parser IMPLEMENTATION.


  METHOD create.

    result = NEW #( ).

    result->initialize( iv_bdl_source = iv_bdl_source ).

  ENDMETHOD.


  METHOD get_node_type_for_name.

    TRY.
        result = ms_ast-node_types->*[ KEY by_name name = iv_name ].
      CATCH cx_sy_itab_line_not_found.
        CLEAR: result.
    ENDTRY.

  ENDMETHOD.


  METHOD get_nodes_for_name.

    DATA(ls_node_type) = get_node_type_for_name( iv_name ).

    IF ls_node_type IS INITIAL.
      RETURN.
    ENDIF.

    result =
        VALUE #(
            index = ls_node_type-index
            name  = ls_node_type-name
            nodes = mo_ast_util->get_nodes_of_type( ls_node_type ) ).

  ENDMETHOD.


  METHOD get_all_nodes.

*    result =
*        VALUE #(
*            FOR ls_node_type IN ms_ast-node_types->*
*            ( index = ls_node_type-index
*              name  = ls_node_type-name
*              nodes = mo_ast_util->get_nodes_of_type( ls_node_type ) ) ).

    result =
        REDUCE #(
          INIT lt_nodes_result TYPE ty_nodes
          FOR ls_node_type IN ms_ast-node_types->*
          LET lt_nodes = mo_ast_util->get_nodes_of_type( ls_node_type ) IN
          NEXT lt_nodes_result =
            COND #(
                WHEN lt_nodes IS INITIAL
                    THEN lt_nodes_result
                    ELSE VALUE #( BASE lt_nodes_result
                                  ( index = ls_node_type-index
                                    name  = ls_node_type-name
                                    nodes = mo_ast_util->get_nodes_of_type( ls_node_type ) ) ) ) ).

  ENDMETHOD.


  METHOD get_field_mapping_for_table.

    DATA(ls_nodes) = get_nodes_for_name( zif_rapu_bdef_dp=>gc_node_type-MapBehavior ).

    LOOP AT ls_nodes-nodes INTO DATA(lo_node).

      IF NOT contains( val = lo_node->content regex = |mapping( *)for( *){ iv_table_name }| case = abap_false ).
        CONTINUE.
      ENDIF.

      DATA(lo_entity) = get_entity_from_lower_node( lo_node ).

      IF lo_entity IS NOT BOUND OR to_upper( lo_entity->content ) <> to_upper( iv_entity ).
        CONTINUE.
      ENDIF.

      DATA(lt_subnode) = lo_node->get_subnodes_by_type( get_node_type_for_name( zif_rapu_bdef_dp=>gc_node_type-FieldMap ) ).

      result =
        VALUE #(
            BASE result
            FOR lo_subnode IN lt_subnode
            ( subnode_to_field_mapping( lo_subnode ) ) ).

    ENDLOOP.

  ENDMETHOD.


  METHOD initialize.

    DATA(lo_rnd_bdl_parser) = cl_bdl_parser_factory=>create_simple_bdl_parser( ).

    lo_rnd_bdl_parser->parse_and_create_ast( EXPORTING iv_input = iv_bdl_source
                                             IMPORTING em_ast   = ms_ast ).

    mo_ast_util = cl_art_rnd_ast_util=>create( REF #( ms_ast ) ).

  ENDMETHOD.


  METHOD subnode_to_field_mapping.

    FIND FIRST OCCURRENCE OF REGEX `(:?\w*) *= *(:?\w*) *;`
      IN io_subnode->content
      SUBMATCHES DATA(lv_internal) DATA(lv_table).

    result-internal = to_upper( lv_internal ).
    result-table    = to_upper( lv_table ).

  ENDMETHOD.


  METHOD get_entity_from_lower_node.

    DATA(lo_entity_behavior) =
        io_node->find_ancestor_by_type(
            get_node_type_for_name( zif_rapu_bdef_dp=>gc_node_type-EntityBehavior ) ).

    IF lo_entity_behavior IS NOT BOUND.
      RETURN.
    ENDIF.

    DATA(lt_entity) =
        lo_entity_behavior->get_children_by_type(
            get_node_type_for_name( zif_rapu_bdef_dp=>gc_node_type-Entity ) ).

    IF lt_entity IS INITIAL.
      RETURN.
    ENDIF.

    result = lt_entity[ 1 ].

  ENDMETHOD.


ENDCLASS.
