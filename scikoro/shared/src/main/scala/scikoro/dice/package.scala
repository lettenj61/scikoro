package scikoro

package object dice {

  implicit class RollableInt(val num: Int) extends AnyVal {
    def d4: Roll = Roll(num, 4)
    def d6: Roll = Roll(num, 6)
    def d8: Roll = Roll(num, 8)
    def d10: Roll = Roll(num, 10)
    def d12: Roll = Roll(num, 12)
    def d20: Roll = Roll(num, 20)
    def d100: Roll = Roll(num, 100)
  }
}
