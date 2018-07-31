namespace java org.datacrafts.scrooge.shapes

struct StructExample {
  1: required string foo
  2: optional string bar
}

struct NestedStructExample {
  1: required StructExample str
  2: optional i64 qux
}

union UnionExample {
  1: StructExample a
  2: i32 b
  3: string c
}

struct MapExample {
  1: required string id
  3: optional map<string,string> metadata
}

enum TweetType {
    Tweet = 1,
    Retweet = 2,
    Dm = 3,
    Reply = 4
}

union NestedUnion {
  1: UnionExample a
  2: i16 b
  3: string c
}
