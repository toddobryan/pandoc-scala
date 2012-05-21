package pandoc.util

import org.scalatest.FunSuite

import Reader.{read, parsedPlusRest}

class ReadTest extends FunSuite {
  test("parse various things") {
    assert(parsedPlusRest(read[Int]("123")) === Some(123, ""))
    assert(parsedPlusRest(read[Double]("4.0 + a")) === Some(4.0, " + a"))
    assert(parsedPlusRest(read[Boolean]("true")) === Some(true, ""))
    assert(parsedPlusRest(read[Boolean]("true or false")) === Some(true, " or false"))
    assert(parsedPlusRest(read[Boolean]("falsefalsefalse")) === Some(false, "falsefalse"))
    assert(parsedPlusRest(read[String]("\"a string\" is nice")) === Some("\"a string\"", " is nice"))
    assert(parsedPlusRest(read[List[Int]]("[1, 2, 3]")) === Some(List(1, 2, 3), ""))
    assert(parsedPlusRest(read[List[Double]]("[1   ,     2, 3]")) === Some(List(1.0, 2.0, 3.0), ""))
    assert(parsedPlusRest(read[List[Boolean]]("[true]")) === Some(List(true), ""))
    assert(parsedPlusRest(read[List[String]]("[\"a\"],\"b\"")) === Some(List("\"a\""), ",\"b\""))
  }
}