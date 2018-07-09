package org.datacrafts

package object noschema extends Primitive.Instances with Container.Instances
  with ShapelessProduct.Instances with ShapelessCoproduct.Instances
  with com.stripe.scrooge.shapes.GenericInstances {

  // these 2 types are under the UnkownUnionField of scrooge Union type,
  // and is an auto generated private class.
  // ideally the shapeless representation should not include them
  // they will not participate in marshaling/unmarshaling, just for type description
  implicit val twitterBufPrimitiveType = new Primitive[com.twitter.io.Buf]
  implicit val tFieldPrimitiveType = new Primitive[org.apache.thrift.protocol.TField]
}
