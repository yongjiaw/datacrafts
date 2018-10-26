package org.datacrafts.noschema

import scala.reflect.runtime.{universe => ru}

import com.twitter.scrooge.{ThriftStruct, ThriftUnion}
import org.datacrafts.logging.Slf4jLogging
import org.datacrafts.noschema.NoSchemaThriftTest._
import org.datacrafts.noschema.reflection.{NoSchemaReflector, ReflectedCoproduct, ReflectedProduct, TypeReflector}
import org.datacrafts.noschema.reflection.NoSchemaReflector.{logInfo, ReflectionRule}
import org.datacrafts.noschema.rule.DefaultRule
import org.datacrafts.scrooge.shapes._
import org.scalatest.FlatSpec

// scalastyle:off
class NoSchemaThriftTest extends FlatSpec
  with NoSchemaDsl with ScroogeSupport with Slf4jLogging.Default {

  "Marshalling and unmarshalling with Map" should "be successful" in {

    println(schemaOf[Recursive].format())

    // scrooge thrift enum does not work with scala 2.11
    // https://github.com/circe/circe/pull/644/files
    // println(schemaOf[TweetType].format())

    // can just print the schema itself
    println(schemaOf[TestClass].format())

    val op1 = DefaultRule.withSchema[TestClass]
    println(op1.format())
    // equivalent to the above, just DSL syntactic sugar
    val op2 = schemaOf[TestClass].operation()
    println(op2.format())

    val op3 = reflectedSchemaOf[TestClass](
      new ReflectionRule {

        import ru.typeOf

        override def reflect(tpe: ru.Type): NoSchema[Any] = {
          val reflector = TypeReflector(tpe)
          if (tpe < typeOf[ThriftUnion]) { // union type extends both ThriftUnion and ThriftStruct
            if (reflector.subclasses.nonEmpty) { // coproduct
              logInfo(
                s"${tpe.typeSymbol.fullName} is coproduct of (${reflector.subclasses})")
              new ReflectedCoproduct(
                tpe,
                members = (
                  for (
                    symbol <- reflector.subclasses
                    if (symbol.name.toString != "UnknownUnionField")
                  ) yield {
                    val symbolName = symbol.name.toString
                    val subClassReflector = new TypeReflector(symbol.typeSignature)
                    if (subClassReflector.caseAccessors.size == 0){ // enum is case object
                      ReflectedCoproduct.Member(
                        symbol,
                        None,
                        Context.CoproductElement(
                          Symbol(symbolName),
                          reflect(symbol.typeSignature.dealias)
                        )
                      )
                    }
                    else if (subClassReflector.caseAccessors.size == 1) {
                      // everything else is wrapped in case class
                      val wrappedMember = subClassReflector.caseAccessors(0)
                      ReflectedCoproduct.Member(
                        wrappedMember,
                        Some(symbol),
                        Context.CoproductElement(
                          Symbol(symbolName),
                          reflect(wrappedMember.typeSignature.dealias)
                        )
                      )
                    }
                    else {
                      throw new Exception(
                        s"ThriftUnion's subclass should not have more than 1 case accessors:" +
                          s"${symbolName}(${subClassReflector.caseAccessors})"
                      )
                    }

                  }).toSeq
              )
            }
            else {
              throw new Exception(s"${tpe.typeSymbol.fullName} not recognized")
            }
          }
          else if (tpe < typeOf[ThriftStruct]) {
            logInfo(s"${tpe.typeSymbol.fullName} is thrift struct")
            new ReflectedProduct(
              tpe,
              fields = reflector.applyArgs.map {
                s =>
                  val symbolName = s.name.toString
                  val symbolType =
                    reflector.caseMemberTypeMap.get(symbolName).getOrElse(
                      s.typeSignature
                    )
                  s -> Context.MemberVariable(
                    Symbol(symbolName),

                    getOrCreateLazySchema(
                      symbolType,
                      reflect(symbolType)
                    )
                  )
              }.toMap
            )
          } else {
            logInfo(s"${tpe.typeSymbol.fullName} is not thrift, use default rule")
            super.reflect(tpe)
          }

        }
      }
    ).operation()
    println(op3.format())

    for (op <- Seq(op1, op3)) {

      assert(
        op.operator.marshal(
          Map(
            "v1" -> 10,
            "v5" -> Map("_2" -> 12),
            "v3" -> Iterable(Seq("v21" -> 3)),
            "v6" -> TestClass3(v31 = 5),
            "v7" -> ("org.datacrafts.noschema.Fruit1.Apple", Map("name" -> "bigApple")),
            "v8" -> Map("v" -> Seq(Map("v" -> Seq.empty, "v2" -> 2)), "v2" -> 1),
            "thriftMap" -> Map("id" -> "1"),
            "thriftNested" -> Map("str" -> Map("foo" -> "bar")),
            "thriftUnion" -> ("scala.Int", 1),
            "thriftUnion2" -> UnionExample.C(c = "test"),
            "thriftEnum" -> ("org.datacrafts.scrooge.shapes.TweetType.Tweet", Map())
          )) == TestClass(
          v1 = 10,
          v5 = (null, 12),
          v3 = Some(Seq(Some(
            TestClass2(
              v21 = 3,
              v22 = null
            )))),
          v6 = Some(TestClass3(v31 = 5)),
          v2 = None,
          v4 = null,
          v7 = Fruit1.Apple("bigApple"),
          v8 = Recursive(Seq(Recursive(v2 = 2)), v2 = 1),
          thriftNested = NestedStructExample(str = StructExample(foo = "bar")),
          thriftMap = MapExample(id = "1"),
          thriftUnion = UnionExample.B(b = 1),
          thriftUnion2 = UnionExample.C(c = "test")
        )
      )

      assert(
        op.operator.unmarshal(
          TestClass(
            v1 = 1,
            v2 = null
          )
        ) == Map(
          "v1" -> 1,
          "v2" -> null,
          // the rest are default values
          "v6" -> null,
          "v7" -> ("org.datacrafts.noschema.Apple", Map("size" -> 1)),
          "v8" -> Map("v2" -> 1, "v" -> Seq.empty),
          "v9" -> null,
          "v5" -> Map("_2" -> 2, "_1" -> "a"),
          "v4" -> null,
          "v3" -> Seq(
            Map(
              "v21" -> 3,
              "v22" -> Map("v" -> Map(), "v32" -> Seq(12.0), "v31" -> 0)
            )
          ),
          "thriftMap" -> Map("id" -> "1", "metadata" -> null),
          "thriftNested" -> Map("str" -> Map("foo" -> "bar", "bar" -> null), "qux" -> null),
          "thriftUnion" -> ("scala.Int", 1),
          "thriftUnion2" -> ("org.datacrafts.scrooge.shapes.StructExample", Map("foo" -> "bar", "bar" -> null)),
          "thriftEnum" -> ("org.datacrafts.scrooge.shapes.TweetType.Tweet", Map())
        )
      )
    }

  }
}

object NoSchemaThriftTest {

  case class TestClass(
    v1: Int,
    v2: Option[Seq[Option[Double]]] = None,
    v3: Option[Seq[Option[TestClass2]]] = Some(Seq(Some(TestClass2()))),
    v4: Seq[Int] = null,
    v5: (String, Int) = ("a", 2),
    v6: Option[TestClass3] = None,
    v7: Fruit = Apple(),
    v8: Recursive = Recursive(),
    v9: GenericType[Int] = null,
    thriftNested: NestedStructExample = NestedStructExample(str = StructExample(foo = "bar")),
    thriftMap: MapExample = MapExample(id = "1"),
    thriftUnion: UnionExample = UnionExample.B(b = 1),
    thriftUnion2: UnionExample = UnionExample.A(StructExample(foo = "bar")),
    thriftEnum: TweetType = TweetType.Tweet
  )

  case class TestClass2(v21: Int = 3,
    v22: TestClass3 = TestClass3(0)
  )

  case class TestClass3(v31: Int,
    v32: Iterable[Double] = Seq(12),
    v: Map[String, Int] = Map.empty
  )

  // if there are members with the recursive type and non-empty default value
  // it'll cause runtime instantiation failure due to infinite recursion
  // type wise, it should be supported regardless
  case class Recursive(
    v: Seq[Recursive] = Seq.empty,
    v2: Int = 1
  )

  // this type doesn't make much sense, just to assert cyclic reference with generic type is handled
  case class GenericType[T](v1: T,
    v2: GenericType[GenericType[Double]] = null
  )

}

sealed trait Fruit

case class Apple(size: Int = 1) extends Fruit

case class Pear(size: Double = 1.5) extends Fruit

object Fruit1 {

  case class Apple(name: String = "red") extends Fruit

}
