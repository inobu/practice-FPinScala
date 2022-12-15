package errorhandling

import org.scalatest.funsuite.AnyFunSuite

class OptionTest extends AnyFunSuite {
  test("fallingFn") {
    assertThrows[Exception] {
      Option.fallingFn(1)
    }
  }

  test("fallingFn2") {
    assert(Option.fallingFn2(1) === 43)
  }
}
