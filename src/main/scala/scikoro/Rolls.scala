package scikoro

import scala.util.Random

/**
  */
trait Roll {

  protected[this] val random = new Random

  def nextValue: Int = random.nextInt(face) + 1

  /** Number of dice in this dice pool.
    */
  def number: Int

  /** Type of the dice represented by faces it has.
    */
  def face: Int

  def modifier: Int => Int

  def expr: String

  /** A symbol used in a expression.
    */
  val symbol: String = "d"

  def +(i: Int): Roll
  def -(i: Int): Roll

  /** Attempt to roll all of the dice in this pool
    * and get results as `Seq`.
    */
  def roll: Result = {
    val vs = for (i <- 1 to number) yield nextValue
    new Result(this, vs)
  }

  def partially(xs: Seq[Int]): Result = {
    require(xs.size <= number, s"Size of slize $xs is too large.")
    new Result(this, xs.map(i => nextValue))
  }

  override def toString = s"$number$symbol$face$expr"
}

object Roll {

  case class Impl(number: Int,
                  face: Int,
                  modifier: Int => Int = identity,
                  expr: String = "") extends Roll {

    def +(i: Int) = copy(
      modifier = this.modifier.andThen((_ + i)),
      expr = this.expr + s"+$i"
    )
    def -(i: Int) = copy(
      modifier = this.modifier.andThen((_ - i)),
      expr = this.expr + s"-$i"
    )
  }

  def four = 1.d4
  def six = 1.d6
  def eight = 1.d8
  def ten = 1.d10
  def twelve = 1.d12
  def twenty = 1.d20
  def percent = 1.d100
  def hundred = percent

  implicit class RollableInt(val num: Int) extends AnyVal {
    def d4: Roll = Impl(num, 4)
    def d6: Roll = Impl(num, 6)
    def d8: Roll = Impl(num, 8)
    def d10: Roll = Impl(num, 10)
    def d12: Roll = Impl(num, 12)
    def d20: Roll = Impl(num, 20)
    def d100: Roll = Impl(num, 100)
  }
}

/** Result of the dice roll.
  *
  * @param source the dice roll created this result.
  * @param values result of each dice as [[Seq]].
  */
case class Result(source: Roll, values: Seq[Int]) {

  def total: Int = source.modifier(values.sum)

  /**
    */
  def rerollMin: Result = {
    val newValues = (values.toBuffer - values.min) :+ source.nextValue
    copy(this.source, newValues)
  }

  def rerollMax: Result = {
    val newValues = (values.toBuffer - values.max) :+ source.nextValue
    copy(this.source, newValues)
  }

  def reroll: Result = source.roll

  def rerollLower(threshold: Int): Result = {
    val replaced = values.filter(_ <= threshold).size
    val newValues = (1 to replaced).map(x => source.nextValue)
    copy(values = newValues)
  }

  override def toString = values.mkString("(", ",", ")") + source.expr.toString
}
