package text.pandoc

import org.scalatest.FunSuite

import MyReader.{read, parsedPlusRest}

class ReadTest extends FunSuite with Parsing {  
  test("parse various things") {
    assert(parsedPlusRest(read[Int]("123")) === Some(123, ""))
    assert(parsedPlusRest(read[Double]("4.0 + a")) === Some(4.0, " + a"))
    assert(parsedPlusRest(read[Boolean]("true")) === Some(true, ""))
    assert(parsedPlusRest(read[Boolean]("true or false")) === Some(true, " or false"))
    assert(parsedPlusRest(read[Boolean]("falsefalsefalse")) === Some(false, "falsefalse"))
    assert(parsedPlusRest(read[String]("\"a string\" is nice")) === Some("\"a string\"", " is nice"))
    assert(parsedPlusRest(read[Stream[Int]]("[1, 2, 3]")) === Some(List(1, 2, 3), ""))
    assert(parsedPlusRest(read[Stream[Double]]("[1   ,     2, 3]")) === Some(List(1.0, 2.0, 3.0), ""))
    assert(parsedPlusRest(read[Stream[Boolean]]("[true]")) === Some(List(true), ""))
    assert(parsedPlusRest(read[Stream[String]]("[\"a\"],\"b\"")) === Some(List("\"a\""), ",\"b\""))
  }
}