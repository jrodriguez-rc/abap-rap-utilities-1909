INTERFACE zif_rapu_bdef_dp
  PUBLIC.

  TYPES:
    ty_bdef_name TYPE c LENGTH 30.

  TYPES:
    BEGIN OF ty_field_mapping,
      internal TYPE fieldname,
      table    TYPE fieldname,
    END OF ty_field_mapping,
    ty_field_mappings TYPE SORTED TABLE OF ty_field_mapping
        WITH UNIQUE KEY internal
        WITH UNIQUE SORTED KEY by_table COMPONENTS table.

  CONSTANTS:
    BEGIN OF gc_node_type,
      BehaviorDefinition       TYPE string VALUE `BehaviorDefinition`,
      BehaviorProjection       TYPE string VALUE `BehaviorProjection`,
      BehaviorExtension        TYPE string VALUE `BehaviorExtension`,
      DraftTable               TYPE string VALUE `DraftTable`,
      PersistentTable          TYPE string VALUE `PersistentTable`,
      AdditionalSave           TYPE string VALUE `AdditionalSave`,
      UnmanagedSave            TYPE string VALUE `UnmanagedSave`,
      AuthorizationContext     TYPE string VALUE `AuthorizationContext`,
      EntityBehavior           TYPE string VALUE `EntityBehavior`,
      ExtendEntityBehavior     TYPE string VALUE `ExtendEntityBehavior`,
      BehaviorProjectionExtens TYPE string VALUE `BehaviorProjectionExtens`,
      Entity                   TYPE string VALUE `Entity`,
      EntityAlias              TYPE string VALUE `EntityAlias`,
      ImplClass                TYPE string VALUE `ImplClass`,
      EtagField                TYPE string VALUE `EtagField`,
      LocalField               TYPE string VALUE `LocalField`,
      OtherField               TYPE string VALUE `OtherField`,
      Field                    TYPE string VALUE `Field`,
      EntityField              TYPE string VALUE `EntityField`,
      PartnerField             TYPE string VALUE `PartnerField`,
      FieldAttribution         TYPE string VALUE `FieldAttribution`,
      LateNumbering            TYPE string VALUE `LateNumbering`,
      Group                    TYPE string VALUE `Group`,
      GroupName                TYPE string VALUE `GroupName`,
      Features                 TYPE string VALUE `Features`,
      ActionProjection         TYPE string VALUE `ActionProjection`,
      Action                   TYPE string VALUE `Action`,
      ActionName               TYPE string VALUE `ActionName`,
      Result                   TYPE string VALUE `Result`,
      Parameter                TYPE string VALUE `Parameter`,
      CDSType                  TYPE string VALUE `CDSType`,
      PartnerType              TYPE string VALUE `PartnerType`,
      ControlType              TYPE string VALUE `ControlType`,
      MapBehavior              TYPE string VALUE `MapBehavior`,
      FieldMap                 TYPE string VALUE `FieldMap`,
      Self                     TYPE string VALUE `Self`,
      Association              TYPE string VALUE `Association`,
      AssociationAlias         TYPE string VALUE `AssociationAlias`,
      AssociationBehavior      TYPE string VALUE `AssociationBehavior`,
      AssociationProjection    TYPE string VALUE `AssociationProjection`,
      AssocCreate              TYPE string VALUE `AssocCreate`,
      Determination            TYPE string VALUE `Determination`,
      Validation               TYPE string VALUE `Validation`,
      Name                     TYPE string VALUE `Name`,
      OnSave                   TYPE string VALUE `OnSave`,
      OnModify                 TYPE string VALUE `OnModify`,
      Managed                  TYPE string VALUE `Managed`,
      Unmanaged                TYPE string VALUE `Unmanaged`,
      Abstract                 TYPE string VALUE `Abstract`,
      StrictMode               TYPE string VALUE `StrictMode`,
      Create                   TYPE string VALUE `Create`,
      Delete                   TYPE string VALUE `Delete`,
      Update                   TYPE string VALUE `Update`,
      Read                     TYPE string VALUE `Read`,
      Lock                     TYPE string VALUE `Lock`,
      LockMaster               TYPE string VALUE `LockMaster`,
      AuthorizationMaster      TYPE string VALUE `AuthorizationMaster`,
      AuthorizationDependent   TYPE string VALUE `AuthorizationDependent`,
      AuthorizationNone        TYPE string VALUE `AuthorizationNone`,
      Disabled                 TYPE string VALUE `Disabled`,
      ReadOnly                 TYPE string VALUE `ReadOnly`,
      Static                   TYPE string VALUE `Static`,
      DCInstanceControlled     TYPE string VALUE `DCInstanceControlled`,
      Selective                TYPE string VALUE `Selective`,
      WithDraft                TYPE string VALUE `WithDraft`,
      bopf                     TYPE string VALUE `BOPF`,
      DraftActName             TYPE string VALUE `DraftActName`,
      Precheck                 TYPE string VALUE `Precheck`,
      Augment                  TYPE string VALUE `Augment`,
      Extensible               TYPE string VALUE `Extensible`,
      EntityExtensible         TYPE string VALUE `EntityExtensible`,
    END OF gc_node_type.

  METHODS get_field_mapping_for_table
    IMPORTING
      iv_entity     TYPE string
      iv_table_name TYPE tabname
    RETURNING
      VALUE(result) TYPE zif_rapu_bdef_dp=>ty_field_mappings.

ENDINTERFACE.
