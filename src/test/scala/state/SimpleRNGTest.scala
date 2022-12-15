package state

import org.scalatest.funsuite.AnyFunSuite

class SimpleRNGTest extends AnyFunSuite {
  test("nextIntTest") {
    val rng = SimpleRNG(42)
    val (n1, rng2) = rng.nextInt
    val (n2, rng3) = rng2.nextInt
    val (n3, _) = rng3.nextInt
    assert(n1 === 16159453)
    assert(n2 === -1281479697)
    assert(n3 === -340305902)
  }
}
