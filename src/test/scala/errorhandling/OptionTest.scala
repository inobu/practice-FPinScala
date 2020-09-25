package errorhandling

import org.scalatest.FunSuite

class OptionTest extends FunSuite {
  test("fallingFn") {
    assertThrows[Exception] {
      Option.fallingFn(1)
    }
  }

  test("fallingFn2") {
    assert(Option.fallingFn2(1) === 43)
  }
}
