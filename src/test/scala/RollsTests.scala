package scikoro

import utest._
import scikoro._, Roll._

object RollsTests extends TestSuite {

  val F = 4.d6

  def tests = this {
    "A 6 sided die would never score 7" - {
      for (i <- 0 to 100) {
        val results = F.rollSeq
        assert(results.forall(_ < F.face + 1))
        assert(results.sum < F.face * F.number)
      }
    }
  }
}
