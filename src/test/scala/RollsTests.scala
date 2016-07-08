package scikoro

import utest._
import scikoro.Roll._

object RollsTests extends TestSuite {

  val F = 4.d6

  def tests = this {
    "A 6 sided die would never score 7" - {
      for (i <- 0 to 100) {
        val result = F.roll
        assert(result.values.forall(_ < F.face + 1))
        assert(result.total < F.face * F.number)
      }
    }
  }
}
