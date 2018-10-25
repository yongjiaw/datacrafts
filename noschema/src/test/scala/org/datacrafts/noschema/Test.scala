package org.datacrafts.noschema

import org.datacrafts.logging.Slf4jLogging
import org.datacrafts.noschema.reflection.TypeReflector
import org.datacrafts.noschema.NoSchemaTest._

case class TestCase(a: Int, b: Int = 2, c: String = "")
case class TestCase2[T1, T2](a: T2, b: T1)

object Test123 extends NoSchemaDsl with Slf4jLogging.Default {

  def main(args: Array[String]): Unit = {
    val noSchema = reflectedSchemaOf[Seq[TestCase]]()
    println(noSchema)
    println(noSchema.format())
    val resultMap = noSchema.operation().unmarshal(Seq(TestCase(1)))
    println(resultMap)
    println(noSchema.operation().marshal(resultMap))

    import scala.reflect.runtime.{universe => ru}

    println(reflectedSchemaOf[TestClassSimple]().format())
    val op = reflectedSchemaOf[TestClassSimple]().operation()
    val map = op.unmarshal(TestClassSimple(1))
    println(map)
    println(op.marshal(map))
  }
}


case class TestClassSimple(
  v1: Int,
  v2: Option[Seq[Option[Double]]] = None,
  v3: Option[Seq[Option[TestClass2]]] = Some(Seq(Some(TestClass2()))),
  v4: Seq[Int] = null,
  v5: (String, Int) = ("a", 2),
  v6: Option[TestClass3] = None,
  v7: Fruit = Apple(),
  v10: Any = "abc",
  v11: Set[Any] = Set(1, 2, 3)
)