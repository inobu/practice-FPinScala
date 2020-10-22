package errorhandling

import org.scalatest.FunSuite


class EitherTest extends FunSuite {
  val e = Left[Exception](new Exception())
  val valuea = Right[String]("a")
  val valueb = Right[String]("b")
  val a = List(e)


  test("sequenceTest") {
    assertResult(Left(new Exception())) (Either.sequence(a))
//    assertResult(1 )(1)

  }
}
