package org.datacrafts.noschema.catalog

import org.datacrafts.noschema.catalog.NoSchemaExternalizable.TypeInfo

/**
  * Externalized NoSchema will support ETL with all complex scenarios of schema evolution.
  * Schema has detailed version and will be archived externally for quick reference.
  * Data in the data lake will have reference to the schema version.
  * Data in the data lake will be stored in flexible format such as JSON.
  * With block compression, size is not an issue, the self-descriptive nature of JSON is critical.
  * Pipeline data operations is simply a matter of using the right schema locate the right fields.
  * Multiple versions of the same schema can also be compared by tools.
  */
case class NoSchemaExternalizable (
  schemaId: String,
  schemaVersion: Option[String],
  typeInfo: TypeInfo
)

object NoSchemaExternalizable {

  object SchemaDomain extends Enumeration {
    val Scala, External = Value
  }

  case class TypeInfo(typeName: String, typeArgs: Seq[TypeInfo])
}
