# datacrafts
Scala library for data<br/>
Releases available on [Maven Repository](https://mvnrepository.com/artifact/org.datacrafts)

## NoSchema
NoSchema is for NoSQL, to support customized schema composition and evolution.
Scala types such as case class serves as the structured NoSQL schema, which can be composed hierarchically.
Schema evolution is just a special case of custom marshalling and unmarshalilng.<br/>
The design is to let shapeless and implicit resolution parse type structure at compile time,
while use regular scala runtime code to control serialziation and deserialization.<br/>
The following example converts between case class and nested Map[String, Any],
using the default rule. The root case class consists of scrooge thrift classes.
```
class NoSchemaTest extends FlatSpec with NoSchemaDsl {

  "Marshalling and unmarshalling with Map" should "be successful" in {

    // these 2 types are under the UnkownUnionField of scrooge Union type
    // ideally the shapeless representation should not include them
    // just provide the implicit types here so the entire shapeless structure can be parsed
    implicit val twitterBufPrimitiveType = new Primitive[com.twitter.io.Buf]
    implicit val tFieldPrimitiveType = new Primitive[org.apache.thrift.protocol.TField]

    val op = DefaultRule.withSchema[TestClass]
    println(op.format())
    // equivalent to the above, just DSL syntactic sugar
    val op2 = schemaOf[TestClass].operation()
    println(op2.format())
    // can just print the schema itself
    println(schemaOf[TestClass].format())

    assert(
      op.operator.marshal(
        Map(
          "v1" -> 10,
          "v5" -> Map("_2" -> 12),
          "v3" -> Iterable(Seq("v21" -> 3)),
          "v6" -> TestClass3(v31 = 5),
          "v7" -> ("org.datacrafts.noschema.NoSchemaTest.Fruit.Apple", Map("name" -> "bigApple")),
          "v8" -> Map("v" -> Seq(Map("v" -> Seq.empty, "v2" -> 2)), "v2" -> 1),
          "thriftMap" -> Map("id" -> "1"),
          "thriftNested" -> Map("str" -> Map("foo" -> "bar")),
          "thriftUnion" -> ("scala.Int", 1),
          "thriftUnion2" -> UnionExample.C(c = "test")
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
        v7 = Fruit.Apple("bigApple"),
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
        "v7" -> ("org.datacrafts.noschema.NoSchemaTest.Apple", Map("size" -> 1)),
        "v8" -> Map("v2" -> 1, "v" -> Seq.empty),
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
        "thriftUnion2" -> ("org.datacrafts.scrooge.shapes.StructExample", Map("foo" -> "bar", "bar" -> null))
      )
    )

  }
}

object NoSchemaTest {

  case class TestClass(
    v1: Int,
    v2: Option[Seq[Option[Double]]] = None,
    v3: Option[Seq[Option[TestClass2]]] = Some(Seq(Some(TestClass2()))),
    v4: Seq[Int] = null,
    v5: (String, Int) = ("a", 2),
    v6: Option[TestClass3] = None,
    v7: Fruit = Apple(),
    v8: Recursive = Recursive(),
    thriftNested: NestedStructExample = NestedStructExample(str = StructExample(foo = "bar")),
    thriftMap: MapExample = MapExample(id = "1"),
    thriftUnion: UnionExample = UnionExample.B(b = 1),
    thriftUnion2: UnionExample = UnionExample.A(StructExample(foo = "bar"))
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

  sealed trait Fruit

  case class Apple(size: Int = 1) extends Fruit
  case class Pear(size: Double = 1.5) extends Fruit

  object Fruit {
    case class Apple(name: String = "red") extends Fruit
  }

}

```
This is the details of the operation with the case class TestClass
```
org.datacrafts.noschema.NoSchemaTest.TestClass(Product, nullable=true)
 ├──thriftMap: org.datacrafts.scrooge.shapes.MapExample(Product, nullable=true)
 │   ├──id: java.lang.String(Primitive, nullable=true)
 │   └──metadata: scala.Option(Option, nullable=false)
 │      └──element: scala.collection.Map(Map, nullable=true)
 │         └──element: java.lang.String(Primitive, nullable=true)
 ├──thriftNested: org.datacrafts.scrooge.shapes.NestedStructExample(Product, nullable=true)
 │   ├──qux: scala.Option(Option, nullable=false)
 │   │   └──element: scala.Long(Primitive, nullable=false)
 │   └──str: org.datacrafts.scrooge.shapes.StructExample(Product, nullable=true)
 │      ├──bar: scala.Option(Option, nullable=false)
 │      │   └──element: java.lang.String(Primitive, nullable=true)
 │      └──foo: java.lang.String(Primitive, nullable=true)
 ├──thriftUnion: org.datacrafts.scrooge.shapes.UnionExample(CoProduct, nullable=true)
 │   ├──A: org.datacrafts.scrooge.shapes.StructExample(Product, nullable=true)
 │   │   ├──bar: scala.Option(Option, nullable=false)
 │   │   │   └──element: java.lang.String(Primitive, nullable=true)
 │   │   └──foo: java.lang.String(Primitive, nullable=true)
 │   ├──B: scala.Int(Primitive, nullable=false)
 │   ├──C: java.lang.String(Primitive, nullable=true)
 │   └──UnknownUnionField: com.twitter.scrooge.TFieldBlob(Product, nullable=true)
 │      ├──content: com.twitter.io.Buf(Primitive, nullable=false)
 │      └──field: org.apache.thrift.protocol.TField(Primitive, nullable=false)
 ├──thriftUnion2: org.datacrafts.scrooge.shapes.UnionExample(CoProduct, nullable=true)
 │   ├──A: org.datacrafts.scrooge.shapes.StructExample(Product, nullable=true)
 │   │   ├──bar: scala.Option(Option, nullable=false)
 │   │   │   └──element: java.lang.String(Primitive, nullable=true)
 │   │   └──foo: java.lang.String(Primitive, nullable=true)
 │   ├──B: scala.Int(Primitive, nullable=false)
 │   ├──C: java.lang.String(Primitive, nullable=true)
 │   └──UnknownUnionField: com.twitter.scrooge.TFieldBlob(Product, nullable=true)
 │      ├──content: com.twitter.io.Buf(Primitive, nullable=false)
 │      └──field: org.apache.thrift.protocol.TField(Primitive, nullable=false)
 ├──v1: scala.Int(Primitive, nullable=false)
 ├──v2: scala.Option(Option, nullable=false)
 │   └──element: scala.collection.Seq(Seq, nullable=true)
 │      └──element: scala.Option(Option, nullable=false)
 │         └──element: scala.Double(Primitive, nullable=false)
 ├──v3: scala.Option(Option, nullable=false)
 │   └──element: scala.collection.Seq(Seq, nullable=true)
 │      └──element: scala.Option(Option, nullable=false)
 │         └──element: org.datacrafts.noschema.NoSchemaTest.TestClass2(Product, nullable=true)
 │            ├──v21: scala.Int(Primitive, nullable=false)
 │            └──v22: org.datacrafts.noschema.NoSchemaTest.TestClass3(Product, nullable=true)
 │               ├──v: scala.collection.immutable.Map(Map, nullable=true)
 │               │   └──element: scala.Int(Primitive, nullable=false)
 │               ├──v31: scala.Int(Primitive, nullable=false)
 │               └──v32: scala.collection.Iterable(Seq, nullable=true)
 │                  └──element: scala.Double(Primitive, nullable=false)
 ├──v4: scala.collection.Seq(Seq, nullable=true)
 │   └──element: scala.Int(Primitive, nullable=false)
 ├──v5: scala.Tuple2(Product, nullable=true)
 │   ├──_1: java.lang.String(Primitive, nullable=true)
 │   └──_2: scala.Int(Primitive, nullable=false)
 ├──v6: scala.Option(Option, nullable=false)
 │   └──element: org.datacrafts.noschema.NoSchemaTest.TestClass3(Product, nullable=true)
 │      ├──v: scala.collection.immutable.Map(Map, nullable=true)
 │      │   └──element: scala.Int(Primitive, nullable=false)
 │      ├──v31: scala.Int(Primitive, nullable=false)
 │      └──v32: scala.collection.Iterable(Seq, nullable=true)
 │         └──element: scala.Double(Primitive, nullable=false)
 ├──v7: org.datacrafts.noschema.NoSchemaTest.Fruit(CoProduct, nullable=true)
 │   ├──Apple: org.datacrafts.noschema.NoSchemaTest.Fruit.Apple(Product, nullable=true)
 │   │   └──name: java.lang.String(Primitive, nullable=true)
 │   ├──Apple: org.datacrafts.noschema.NoSchemaTest.Apple(Product, nullable=true)
 │   │   └──size: scala.Int(Primitive, nullable=false)
 │   └──Pear: org.datacrafts.noschema.NoSchemaTest.Pear(Product, nullable=true)
 │      └──size: scala.Double(Primitive, nullable=false)
 └──v8: org.datacrafts.noschema.NoSchemaTest.Recursive(Product, nullable=true)
    ├──v: scala.collection.Seq(Seq, nullable=true)
    │   └──element: org.datacrafts.noschema.NoSchemaTest.Recursive(...cycle detected, the actual depth depends on runtime instantiation)
    └──v2: scala.Int(Primitive, nullable=false)
```
This is the default rule which is only based on schema type.
Highly customized rules can control behaviors even by context/path of the schema.
```
trait DefaultRule extends Operation.Rule {

  override def getOperator[V](operation: Operation[V]): Operation.Operator[V] = {

    operation.context.noSchema match {

      case _: Primitive[V] => new PrimitiveOperator(operation)

      case shapeless: ShapelessProduct[V, _] =>
        new ShapelessProductMapper(operation, shapeless)

      case shapeless: ShapelessCoproduct[V, _] =>
        new ShapelessCoproductTupler[V](operation, shapeless)

      case option: OptionContainer[_] =>
        new OptionOperator(
          option.element, operation.asInstanceOf[Operation[Option[option.Elem]]])
          .asInstanceOf[Operation.Operator[V]]

      case map: MapContainer[_] =>
        new MapOperator(
          map.element, operation.asInstanceOf[Operation[Map[String, map.Elem]]])
          .asInstanceOf[Operation.Operator[V]]

      case seq: SeqContainer[_] =>
        new SeqOperator(
          seq.element, operation.asInstanceOf[Operation[Seq[seq.Elem]]])
          .asInstanceOf[Operation.Operator[V]]

      case iterable: IterableContainer[_] =>
        new IterableOperator(
          iterable.element, operation.asInstanceOf[Operation[Iterable[iterable.Elem]]])
          .asInstanceOf[Operation.Operator[V]]
    }
  }

}
```
