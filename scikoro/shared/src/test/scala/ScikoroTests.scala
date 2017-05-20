package scikoro

import utest._
import scikoro.dice._

object ScikoroTests extends TestSuite {

  val pool = 4.d6

  def tests = this {
    "A six sided die would never score 7" - {
      for (i <- 0 to 1000) {
        val result = pool.roll
        assert(result.values.forall(_ < pool.face + 1))
        assert(result.total <= pool.face * pool.size)
      }
    }

    "Number keyed table" - {
      val table = Table.withKeyNumbers(1.d6)(
        1 -> "One",
        2 -> "Two",
        3 -> "Three",
        4 -> "Four",
        5 -> "Five",
        6 -> "Six"
      )
      for (_ <- 0 to 1000) {
        val entry = table.nextEntry()
        assert(entry.nonEmpty)
      }
    }
  }
}
