package errorhandling

import errorhandling.Either.Try
import org.scalatest.funsuite.AnyFunSuite


class EitherTest extends AnyFunSuite {
  val exception = new Exception("test exception")


  test("sequenceTest") {
    assertResult(Either.sequence(List(Try(throw exception)))) (Left(exception))
  }
}
