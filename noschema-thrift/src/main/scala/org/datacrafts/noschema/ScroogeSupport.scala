package org.datacrafts.noschema

trait ScroogeSupport extends com.stripe.scrooge.shapes.GenericInstances {

  // these 2 types are under the UnkownUnionField of scrooge Union type,
  // and is an auto generated private class.
  // ideally the shapeless representation should not include them
  // they will not participate in marshaling/unmarshaling, just for type description
  implicit val twitterBufPrimitiveType = new Primitive[com.twitter.io.Buf]
  implicit val tFieldPrimitiveType = new Primitive[org.apache.thrift.protocol.TField]

  // eliminate the node from the final schema tree
  // they are required to build the complete schema,
  // but do not participate in actual marshal/unmarshal operations
  schemaClassFilter += "com.twitter.scrooge.TFieldBlob"
}

object ScroogeSupport extends ScroogeSupport
