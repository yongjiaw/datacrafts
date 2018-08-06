package org.datacrafts.noschema.catalog

/**
  * Externalized NoSchema will support ETL with all complex scenarios of schema evolution.
  * Schema has detailed version and will be archived externally for quick reference.
  * Data in the data lake will have reference to the schema version.
  * Data in the data lake will be stored in flexible format such as JSON.
  * With block compression, size is not an issue, the self-descriptive nature of JSON is critical.
  * Pipeline data operations is simply a matter of using the right schema locate the right fields.
  * Multiple versions of the same schema can also be compared by tools.
  */
sealed trait NoSchemaExternalizable

object NoSchemaExternalizable {

  // similar to avro's schema doc
  sealed trait SchemaInfo
  /**
    * ScalaType is a reliable schema source of truth, since it's under source control
    * The upstream source of truth depends on how the repo is maintained
    * For example, scrooge thrift can be generated from IDLs managed in another repo
    * Case class can be generated from JSON schema, etc.
    * The goal here is only to keep the trace for investigation,
    * which may or may not be completely automated.
    * Since externalized schema are under explicit data modeling control, it can always be out of
    * sync with the source data that are generated from engineering systems.
    * Source schema and externalized schema are under very different process and iteration cycles.
    * When the source data is out of sync with externalized schema, the analytics pipeline
    * depending on the externalized schema(controlled data model) need to be flexible
    * in terms of backward compatibility, and it should have reasonable fall back behavior when
    * the schema is not expected. This may include exposing error messages,
    * but still keep the pipeline running.
    * In addition, there need to be other processes to alert the schema divergence,
    * so actions can be taken proactively based on nature of the depending processes.
    * For example, any changes to schema can be decomposed as atomic operations of
    * removing an existing field, or adding a new field.
    * The consequences of removing a field can be notified to the engineers controlling the
    * source schema, via a staging environment before the release happens.
    * Adding a new field, on the other hand, should not break anything,
    * but just need to be exposed to downstream analysts so it can be considered as a new source.
    * @param className case class or scrooge generated class
    * @param sourceVersion
    */
  case class Scala (
    className: String,
    sourceVersion: SourceVersion
  ) extends SchemaInfo

  // semantic version and all details can be derived from this
  // but it'll require other tools and separate processes
  case class SourceVersion(
    repoUrl: String, // github URL
    commitHash: String
  )

  case class SchemaWithReference(
    _id: Option[String],
    content: scala.Option[NoSchemaExternalizable]
  )

  case class Primitive(name: String) extends NoSchemaExternalizable

  case class Product(
    info: SchemaInfo,
    fields: scala.collection.Map[String, SchemaWithReference]
  ) extends NoSchemaExternalizable

  case class Coproduct(
    info: SchemaInfo,
    fields: scala.collection.Map[String, SchemaWithReference]
  ) extends NoSchemaExternalizable

  case class Map(
    element: SchemaWithReference
  ) extends NoSchemaExternalizable

  case class Array(
    element: SchemaWithReference
  ) extends NoSchemaExternalizable

  case class Optional(
    element: SchemaWithReference
  ) extends NoSchemaExternalizable

}
