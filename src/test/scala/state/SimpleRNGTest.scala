package state

import org.scalatest.funsuite.AnyFunSuite
import state.SimpleRNG.{double, intDouble, nonNegativeInt}

class SimpleRNGTest extends AnyFunSuite {
  private val denominator = Int.MaxValue.toDouble + 1

  test("nextIntTest") {
    val rng = SimpleRNG(42)
    val (n1, rng2) = rng.nextInt
    val (n2, rng3) = rng2.nextInt
    val (n3, _) = rng3.nextInt
    assert(n1 === 16159453)
    assert(n2 === -1281479697)
    assert(n3 === -340305902)
  }

  test("nonNegativeInt") {
    val rng = SimpleRNG(42)
    val (n1, rng2) = nonNegativeInt(rng)
    val (n2, rng3) = nonNegativeInt(rng2)
    val (n3, _) = nonNegativeInt(rng3)
    assert(n1 === 16159453)
    assert(n2 === 1281479696)
    assert(n3 === 340305901)
  }

  test("double") {
    val rng = SimpleRNG(42)
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, _) = double(rng3)

    assert(d1 === 16159453 / denominator)
    assert(d2 === 1281479696 / denominator)
    assert(d3 === 340305901 / denominator)
  }

  test("intDouble") {
    val rng = SimpleRNG(42)
    val ((n1, d1), r2) = intDouble(rng)
    val ((n2, d2), r3) = intDouble(r2)
    val ((n3, d3), _) = intDouble(r3)

    assert(n1 === 16159453)
    assert(n2 === -340305902)
    assert(n3 === 1770001318)

    assert(d1 === 1281479696 / denominator)
    assert(d2 === 2015756019 / denominator)
    assert(d3 === 1934589058 / denominator)
  }
}
