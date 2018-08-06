package org.datacrafts.noschema.catalog

import org.datacrafts.logging.Slf4jLogging
import org.datacrafts.noschema.{Context, NoSchema, NoSchemaDsl, Primitive, ShapelessCoproduct, ShapelessProduct}
import org.datacrafts.noschema.NoSchema.TypeUniqueKey

object ScalaSchemaExternalizer {

  /**
    * Wrapper of schema
    *
    * @param schemaReferenceId unique id in the tree to deal with recursion
    * @param schemaDetail      if the schema already serialized somewhere, this should be None
    */
  case class SerializedSchema(
    schemaReferenceId: String,
    schemaDetail: Option[NoSchemaExternalizable]
  )

}

class ScalaSchemaExternalizer(
  val context: Context.LocalContext[_],
  parent: Option[ScalaSchemaExternalizer]
) extends Slf4jLogging.Default {

  lazy val schemaInfo: NoSchemaExternalizable.SchemaInfo = {
    NoSchemaExternalizable.Scala(
      className = context.noSchema.scalaType.fullName,
      sourceVersion = NoSchemaExternalizable.SourceVersion("", "")
    )
  }

  def root: ScalaSchemaExternalizer = parent.map(_.root).getOrElse(this)

  lazy val path: String = context match {
    case Context.Root(_) => "root"
    case Context.CoproductElement(symbol, _) =>
      // coproduct symbol name may conflict since it's just the class' short name
      // post_fix index
      val elements = parent.get.context.noSchema.dependencies.filter {
        case coproductElement: Context.CoproductElement[_] => coproductElement.symbol == symbol
      }
      if (elements.size > 1) {
        (for (
          i <- 0 until elements.size
          if elements(i) == context
        ) yield {
          s"${symbol.name}_$i"
        }).head
      }
      else {
        symbol.name
      }

    case Context.MemberVariable(symbol, _) => symbol.name
    case Context.ContainerElement(_) => "element"
  }

  lazy val paths: Seq[String] = parent.map(_.paths).getOrElse(Seq.empty) :+ path

  lazy val _schemaReferenceMap = collection.mutable.Map.empty[TypeUniqueKey, String]

  def getOrElseUpdateSchemaReference(
    typeUniqueKey: TypeUniqueKey,
    reference: String
  ): String = {
    root._schemaReferenceMap.getOrElseUpdate(
      typeUniqueKey,
      reference
    )
  }

  private lazy val dependencySerializerMap: Map[Context.LocalContext[_], ScalaSchemaExternalizer] =
    context.noSchema.dependencies.map {
      dep => dep -> new ScalaSchemaExternalizer(dep, Some(this))
    }.toMap

  def dependencySerializer(dependency: Context.LocalContext[_]): ScalaSchemaExternalizer = {
    dependencySerializerMap.getOrElse(
      dependency,
      throw new Exception(
        s"calling with unrecognized dependency ${dependency}, " +
          s"not found in ${dependencySerializerMap.keySet}. " +
          s"This should not happen under intended usage"
      )
    )
  }

  def getExternalizedSchema(): NoSchemaExternalizable.SchemaWithReference = {

    context.noSchema.category match {
      case NoSchema.Category.Primitive =>

        NoSchemaExternalizable.SchemaWithReference(
          None,
          Some(
            NoSchemaExternalizable.Primitive(
              context.noSchema.asInstanceOf[Primitive[_]].refinedType.toString)
          )
        )

      case NoSchema.Category.Map =>
        NoSchemaExternalizable.SchemaWithReference(
          None,
          Some(
            NoSchemaExternalizable.Map(
              dependencySerializer(context.noSchema.dependencies(0)).getExternalizedSchema()
            ))
        )

      case NoSchema.Category.Seq =>
        NoSchemaExternalizable.SchemaWithReference(
          None,
          Some(NoSchemaExternalizable.Array(
            dependencySerializer(context.noSchema.dependencies(0)).getExternalizedSchema()
          ))
        )

      case NoSchema.Category.Option =>
        NoSchemaExternalizable.SchemaWithReference(
          None,
          Some(NoSchemaExternalizable.Optional(
            dependencySerializer(context.noSchema.dependencies(0)).getExternalizedSchema()
          ))
        )

      case NoSchema.Category.Product =>
        context.noSchema match {
          case product: ShapelessProduct[_, _] =>
            val schemaTypeKey = context.noSchema.scalaType.uniqueKey
            val schemaReference = paths.mkString(".")
            val finalReference = getOrElseUpdateSchemaReference(
              schemaTypeKey, schemaReference
            )

            NoSchemaExternalizable.SchemaWithReference(
              Some(finalReference),

              if (finalReference != schemaReference) {
                // schema already seen
                None
              }
              else { // first time see the schema
                Some(NoSchemaExternalizable.Product(
                  schemaInfo,
                  product.dependencies.map {
                    dep =>
                      dependencySerializer(dep).path ->
                        dependencySerializer(dep).getExternalizedSchema()

                  }.sortBy(_._1).toMap
                ))
              }
            )

          case _ => throw new Exception(s"unrecognized product type ${context.noSchema}")
        }

      case NoSchema.Category.CoProduct =>
        context.noSchema match {
          case coproduct: ShapelessCoproduct[_, _] =>
            val schemaTypeKey = context.noSchema.scalaType.uniqueKey
            val schemaReference = paths.mkString(".")
            val finalReference = getOrElseUpdateSchemaReference(
              schemaTypeKey, schemaReference
            )
            NoSchemaExternalizable.SchemaWithReference(
              Some(schemaReference),
              if (finalReference != schemaReference) {
                None
              }
              else {
                Some(NoSchemaExternalizable.Coproduct(
                  schemaInfo,
                  coproduct.dependencies.map {
                    dep =>
                      dependencySerializer(dep).path ->
                        dependencySerializer(dep).getExternalizedSchema()
                  }.sortBy(_._1).toMap
                ))
              }
            )
          case _ => throw new Exception(s"unrecognized coproduct type ${context.noSchema}")
        }
    }
  }
}
