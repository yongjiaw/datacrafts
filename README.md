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
          "thriftMap" -> Map("id" -> "1"),
          "thriftNested" -> Map("str" -> Map("foo" -> "bar")),
          "thriftUnion" -> ("Int", 1),
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
        "thriftUnion" -> ("Int", 1),
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

  // recursive definition should be supported,
  // unless there are members with the recursive type and non-empty default value
  case class Recursive(v: Seq[Recursive] = Seq.empty,
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
TestClass(Product, nullable=true)
    ├──thriftMap: MapExample(Product, nullable=true)
    │   ├──id: String(Primitive, nullable=true)
    │   └──metadata: Option(Option, nullable=false)
    │      └──element: Map(Map, nullable=true)
    │         └──element: String(Primitive, nullable=true)
    ├──thriftNested: NestedStructExample(Product, nullable=true)
    │   ├──qux: Option(Option, nullable=false)
    │   │   └──element: Long(Primitive, nullable=false)
    │   └──str: StructExample(Product, nullable=true)
    │      ├──bar: Option(Option, nullable=false)
    │      │   └──element: String(Primitive, nullable=true)
    │      └──foo: String(Primitive, nullable=true)
    ├──thriftUnion: UnionExample(CoProduct, nullable=true)
    │   ├──A(org.datacrafts.scrooge.shapes.StructExample): StructExample(Product, nullable=true)
    │   │   ├──bar: Option(Option, nullable=false)
    │   │   │   └──element: String(Primitive, nullable=true)
    │   │   └──foo: String(Primitive, nullable=true)
    │   ├──B(Int): Int(Primitive, nullable=false)
    │   ├──C(String): String(Primitive, nullable=true)
    │   └──UnknownUnionField(com.twitter.scrooge.TFieldBlob): TFieldBlob(Product, nullable=true)
    │      ├──content: Buf(Primitive, nullable=false)
    │      └──field: TField(Primitive, nullable=false)
    ├──thriftUnion2: UnionExample(CoProduct, nullable=true)
    │   ├──A(org.datacrafts.scrooge.shapes.StructExample): StructExample(Product, nullable=true)
    │   │   ├──bar: Option(Option, nullable=false)
    │   │   │   └──element: String(Primitive, nullable=true)
    │   │   └──foo: String(Primitive, nullable=true)
    │   ├──B(Int): Int(Primitive, nullable=false)
    │   ├──C(String): String(Primitive, nullable=true)
    │   └──UnknownUnionField(com.twitter.scrooge.TFieldBlob): TFieldBlob(Product, nullable=true)
    │      ├──content: Buf(Primitive, nullable=false)
    │      └──field: TField(Primitive, nullable=false)
    ├──v1: Int(Primitive, nullable=false)
    ├──v2: Option(Option, nullable=false)
    │   └──element: Seq(Seq, nullable=true)
    │      └──element: Option(Option, nullable=false)
    │         └──element: Double(Primitive, nullable=false)
    ├──v3: Option(Option, nullable=false)
    │   └──element: Seq(Seq, nullable=true)
    │      └──element: Option(Option, nullable=false)
    │         └──element: TestClass2(Product, nullable=true)
    │            ├──v21: Int(Primitive, nullable=false)
    │            └──v22: TestClass3(Product, nullable=true)
    │               ├──v: Map(Map, nullable=true)
    │               │   └──element: Int(Primitive, nullable=false)
    │               ├──v31: Int(Primitive, nullable=false)
    │               └──v32: Iterable(Seq, nullable=true)
    │                  └──element: Double(Primitive, nullable=false)
    ├──v4: Seq(Seq, nullable=true)
    │   └──element: Int(Primitive, nullable=false)
    ├──v5: Tuple2(Product, nullable=true)
    │   ├──_1: String(Primitive, nullable=true)
    │   └──_2: Int(Primitive, nullable=false)
    ├──v6: Option(Option, nullable=false)
    │   └──element: TestClass3(Product, nullable=true)
    │      ├──v: Map(Map, nullable=true)
    │      │   └──element: Int(Primitive, nullable=false)
    │      ├──v31: Int(Primitive, nullable=false)
    │      └──v32: Iterable(Seq, nullable=true)
    │         └──element: Double(Primitive, nullable=false)
    └──v7: Fruit(CoProduct, nullable=true)
       ├──Apple(org.datacrafts.noschema.NoSchemaTest.Apple): Apple(Product, nullable=true)
       │   └──size: Int(Primitive, nullable=false)
       ├──Apple(org.datacrafts.noschema.NoSchemaTest.Fruit.Apple): Apple(Product, nullable=true)
       │   └──name: String(Primitive, nullable=true)
       └──Pear(org.datacrafts.noschema.NoSchemaTest.Pear): Pear(Product, nullable=true)
          └──size: Double(Primitive, nullable=false)
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
