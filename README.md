# datacrafts
Scala library for data<br/>
Available on [Sonatype Nexus](https://oss.sonatype.org/content/repositories/snapshots/org/datacrafts/)

## NoSchema
NoSchema is for NoSQL, to handle schema evolution with customization.
Scala types such as case class serves as the structured NoSQL schema.
Schema evolution is just a special case of custom marshalling and unmarshalilng.
The following example converts between case class and nested Map[String, Any],
using the default rule.

```
class NoSchemaTest extends FlatSpec with ShapelessProduct.Implicits {

  "Marshalling and unmarshalling with Map" should "be successful" in {

    val op = NoSchema.of[TestClass]

    assert(
      op.operator.marshal(
        Map(
          "v1" -> 10,
          "v5" -> Map("_2" -> 12),
          "v3" -> Iterable(Seq("v21" -> 3)),
          "v6" -> TestClass3(v31 = 5)
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
        v4 = null
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
        "v5" -> Map("_2" -> 2, "_1" -> "a"),
        "v4" -> null,
        "v3" -> Seq(
          Map(
            "v21" -> 3,
            "v22" -> Map("v" -> Map(), "v32" -> Seq(12.0), "v31" -> 0)
          )
        )
      )
    )

  }
}

object NoSchemaTest {

  case class TestClass(v1: Int,
    v2: Option[Seq[Option[Double]]] = None,
    v3: Option[Seq[Option[TestClass2]]] = Some(Seq(Some(TestClass2()))),
    v4: Seq[Int] = null,
    v5: (String, Int) = ("a", 2),
    v6: Option[TestClass3] = None
  )

  case class TestClass2(v21: Int = 3,
    v22: TestClass3 = TestClass3(0)
  )

  case class TestClass3(v31: Int,
    v32: Iterable[Double] = Seq(12),
    v: Map[String, Int] = Map.empty
  )

}

```
This is the details of the operation with the case class TestClass
```
println(op.format())
org.datacrafts.noschema.NoSchemaTest.TestClass(nullable = true) => ShapelessProductMapper
	v6: Option[org.datacrafts.noschema.NoSchemaTest.TestClass3](nullable = false) => OptionOperator
		element: org.datacrafts.noschema.NoSchemaTest.TestClass3(nullable = true) => ShapelessProductMapper
			v: Map[String,Int](nullable = true) => MapOperator
				element: Int(nullable = false) => PrimitiveOperator
			v32: Iterable[Double](nullable = true) => IterableOperator
				element: Double(nullable = false) => PrimitiveOperator
			v31: Int(nullable = false) => PrimitiveOperator
	v1: Int(nullable = false) => PrimitiveOperator
	v5: (String, Int)(nullable = true) => ShapelessProductMapper
		_2: Int(nullable = false) => PrimitiveOperator
		_1: String(nullable = true) => PrimitiveOperator
	v3: Option[Seq[Option[org.datacrafts.noschema.NoSchemaTest.TestClass2]]](nullable = false) => OptionOperator
		element: Seq[Option[org.datacrafts.noschema.NoSchemaTest.TestClass2]](nullable = true) => SeqOperator
			element: Option[org.datacrafts.noschema.NoSchemaTest.TestClass2](nullable = false) => OptionOperator
				element: org.datacrafts.noschema.NoSchemaTest.TestClass2(nullable = true) => ShapelessProductMapper
					v22: org.datacrafts.noschema.NoSchemaTest.TestClass3(nullable = true) => ShapelessProductMapper
						v: Map[String,Int](nullable = true) => MapOperator
							element: Int(nullable = false) => PrimitiveOperator
						v32: Iterable[Double](nullable = true) => IterableOperator
							element: Double(nullable = false) => PrimitiveOperator
						v31: Int(nullable = false) => PrimitiveOperator
					v21: Int(nullable = false) => PrimitiveOperator
	v2: Option[Seq[Option[Double]]](nullable = false) => OptionOperator
		element: Seq[Option[Double]](nullable = true) => SeqOperator
			element: Option[Double](nullable = false) => OptionOperator
				element: Double(nullable = false) => PrimitiveOperator
	v4: Seq[Int](nullable = true) => SeqOperator
		element: Int(nullable = false) => PrimitiveOperator
```
This is the default rule that can be customized
```
trait DefaultRule extends Operation.Rule {

  override def getOperator[V](operation: Operation[V]): Operation.Operator[V] = {

    operation.context.noSchema match {

      case _: Primitive[V] => new PrimitiveOperator[V](operation)

      case shapeless: ShapelessProduct[V, _] =>
        new ShapelessProductMapper[V](operation, shapeless)

      case option: OptionContainer[_] =>
        new OptionOperator[option.Elem](
          option.element, operation.asInstanceOf[Operation[Option[option.Elem]]])
          .asInstanceOf[Operation.Operator[V]]

      case map: MapContainer[_] =>
        new MapOperator[map.Elem](
          map.element, operation.asInstanceOf[Operation[Map[String, map.Elem]]])
          .asInstanceOf[Operation.Operator[V]]

      case seq: SeqContainer[_] =>
        new SeqOperator[seq.Elem](
          seq.element, operation.asInstanceOf[Operation[Seq[seq.Elem]]])
          .asInstanceOf[Operation.Operator[V]]

      case iterable: IterableContainer[_] =>
        new IterableOperator[iterable.Elem](
          iterable.element, operation.asInstanceOf[Operation[Iterable[iterable.Elem]]])
          .asInstanceOf[Operation.Operator[V]]
    }
  }

}
```
