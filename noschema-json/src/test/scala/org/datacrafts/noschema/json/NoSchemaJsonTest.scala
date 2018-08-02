package org.datacrafts.noschema.json

import scala.util.Try

import org.datacrafts.noschema.json.JsonOperation.JsonConfig
import org.datacrafts.noschema.json.NoSchemaJsonTest.Test3
import org.scalatest.FlatSpec

class NoSchemaJsonTest extends FlatSpec with JsonOperationDsl {
  "JSON marshal/unmarshal" should "be successful" in {

    val jsonWithNull = """{"v1":{"v1":{"v1":"a","v2":null},"v2":{}},"v2":[]}"""
    val jsonWithoutNull = """{"v1":{"v1":{"v1":"a"},"v2":{}},"v2":[]}"""
    val jsonWithUnknownField = """{"v1":{"v1":{"v1":"a","v2":null,"x":null},"v2":{}},"v2":[]}"""

    val jsonOp = jsonOperationOf[Test3](
      JsonConfig(
        allowUnknownField = false,
        allowAbsence = true,
        includeNull = true)
    )

    println(jsonOp.toJson(Test3(), true)) // scalastyle:ignore

    assert(jsonOp.toJson(Test3()) == jsonWithNull)

    assert(Test3() == jsonOp.fromJson(jsonWithNull))
    assert(Test3() == jsonOp.fromJson(jsonWithoutNull))
    assert(Try(jsonOp.fromJson(jsonWithUnknownField)).isFailure)

    val jsonOp2 = jsonOperationOf[Test3](JsonConfig(includeNull = false))
    assert(jsonOp2.toJson(Test3()) == jsonWithoutNull)

    val jsonOp3 = jsonOperationOf[Test3](JsonConfig(allowAbsence = false))
    assert(Try(jsonOp3.fromJson(jsonWithoutNull)).isFailure)

    val jsonOp4 = jsonOperationOf[Test3](JsonConfig(allowUnknownField = true))
    assert(Test3() == jsonOp4.fromJson(jsonWithUnknownField))
  }
}

object NoSchemaJsonTest {
  case class Test1(v1: String = "a", v2: Option[Seq[Map[String, Double]]] = None)
  case class Test2(v1: Option[Test1] = Some(Test1()), v2: Map[String, Test1] = Map.empty)
  case class Test3(v1: Test2 = Test2(), v2: Seq[Test3] = Seq.empty)
}
