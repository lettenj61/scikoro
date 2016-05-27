package scikoro

import scala.util.{ Random => R }

/** Rollable dice pools.
  */
trait Roll[A] {

  /** Dice quantity.
    */
  def number: Int

  /** Faces the die has.
    */
  def face: Int

  /** Modifier number which will be applied to this roll when resolved.
    */
  def modifier: Int

  /** A symbol represents this die.
    */
  val symbol: String = "d"

  /** Give a throw for current roll and return results as a `Seq`.
    */
  def rollSeq: Seq[A]

  /** Roll the die and get actual result.
    */
  def roll: A

  /** Create a new roll with increasing modifier of current roll
    * by amount of given `mod`.
    */
  def +(mod: Int): Roll[A]

  /** Create a new roll with decreasing current modifier by amount of given `mod`.
    */
  def -(mod: Int): Roll[A]

  override def toString = {
    def pred = modifier match {
      case i if i > 0 => s" + $i"
      case d if d < 0 => s" - ${d.abs}"
      case _          => ""
    }
    s"$number$symbol$face$pred"
  }
}

case class SimpleRoll(
    number: Int = 1,
    face: Int = 6,
    modifier: Int = 0) extends Roll[Int] {

  def rollSeq: Seq[Int] = (1 to number).map(n => R.nextInt(face) + 1)
  def roll: Int = rollSeq.foldLeft(modifier)(_ + _)
  def +(mod: Int) = copy(modifier = modifier + mod)
  def -(mod: Int) = copy(modifier = modifier - mod)
}

/*
case class ZippedRoll(
    left: Roll, right: Roll) extends Roll[(Int, Int)] {

  import util.Random._
  type Result2 = (Int, Int)

  def modifier = left.modifier + right.modifier
  def display = (left.display._1 zip right.display._1, modifier)
}
*/

object Roll {

  def four = 1.d4
  def six = 1.d6
  def eight = 1.d8
  def ten = 1.d10
  def twelve = 1.d12
  def twenty = 1.d20
  def percent = 1.d100

  implicit class RollableInt(val num: Int) extends AnyVal {
    def d4: Roll[Int] = SimpleRoll(num, 4)
    def d6: Roll[Int] = SimpleRoll(num)
    def d8: Roll[Int] = SimpleRoll(num, 8)
    def d10: Roll[Int] = SimpleRoll(num, 10)
    def d12: Roll[Int] = SimpleRoll(num, 12)
    def d20: Roll[Int] = SimpleRoll(num, 20)
    def d100: Roll[Int] = SimpleRoll(num, 100)
  }
}
