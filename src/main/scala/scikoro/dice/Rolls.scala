package scikoro.dice

import scala.util.Random

/** Rollable dice pool.
  */
trait Roll {

  protected[this] val random = new Random

  def nextValue: Int = random.nextInt(face) + 1

  /** Number of dice in this dice pool.
    */
  def size: Int

  /** Type of the dice represented by faces it has.
    */
  def face: Int

  /** A function applied to sum of the result of this dice roll when it's resolved.
    */
  def modifier: Int => Int

  /** Expression for modifier used to get a `String` representation of this dice roll.
    */
  def expr: String

  /** A symbol used in a expression.
    */
  val symbol: String = "d"

  /** Generate new `Roll` with adding given integer to its result.
    */
  def +(i: Int): Roll

  /** Generate new `Roll` with subtracting given integer from its result.
    */
  def -(i: Int): Roll

  /** Generate new Roll with its pool size increased by given number.
   */
  def ++(increasing: Int): Roll

  /** Generate new Roll with its pool size decreased by given number.
   */
  def --(decreasing: Int): Roll

  /** Attempt to roll all of the dice in this pool
    * and get results as `Seq`.
    */
  def roll: Rolled = size match {
    case 1  => Rolled.Result1(this, nextValue)
    case 2  => Rolled.Result2(this, (nextValue, nextValue))
    case 3  => Rolled.Result3(this, (nextValue, nextValue, nextValue))
    case z if z < 1 => Rolled.Result1(this, 0)
    case _  =>
      val vs = for (i <- 1 to size) yield nextValue
      Rolled.ResultSeq(this, vs)
  }

  override def toString = s"$size$symbol$face$expr"
}

object Roll {

  case class Impl private (size: Int,
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

    def ++(increasing: Int) = copy(size = this.size + increasing)

    def --(decreasing: Int) = {
      val newPoolSize = if (size - decreasing < 1) 0 else size - decreasing
      copy(size = newPoolSize)
    }
  }

  def apply(size: Int, face: Int): Roll = {
    require(size > -1, s"The size of dice pool must be at least 1: $size")
    Impl(size, face)
  }
  def apply(size: Int, face: Int, modifier: Int => Int, expr: String): Roll = {
    require(size > -1, s"The size of dice pool must be at least 1: $size")
    Impl(size, face, modifier, expr)
  }

  def four = apply(1, 4)
  def six = apply(1, 6)
  def eight = apply(1, 8)
  def ten = apply(1, 10)
  def twelve = apply(1, 12)
  def twenty = apply(1, 20)
  def percent = apply(1, 100)
  def hundred = percent
}


trait Rolled {

  def source: Roll

  def values: Seq[Int]

  def top(count: Int): Seq[Int] = values.sorted.takeRight(count)

  def least(count: Int): Seq[Int] = values.sorted.take(count)

  def total: Int = source.modifier(values.sum)

  def reroll: Rolled = source.roll

  def rerollBy(p: Int => Boolean): Rolled
}

object Rolled {

  case class Result1(source: Roll, repr: Int) extends Rolled {

    lazy val values: Seq[Int] = Seq(repr)

    def rerollBy(p: Int => Boolean) =
      if (p(repr)) copy(repr = source.nextValue) else this

    override def toString = s"($repr)${source.expr}"
  }

  case class Result2(source: Roll, repr: (Int, Int)) extends Rolled {

    lazy val values: Seq[Int] = Seq(repr._1, repr._2)

    def rerollBy(p: Int => Boolean) = {
      import source.nextValue
      val nv = values.map(v => if(p(v)) nextValue else v)
      copy(source, (nv(0), nv(1)))
    }

    override def toString = repr.toString + source.expr.toString
  }

  case class Result3(source: Roll, repr: (Int, Int, Int)) extends Rolled {

    lazy val values: Seq[Int] = Seq(repr._1, repr._2, repr._3)

    def rerollBy(p: Int => Boolean) = {
      import source.nextValue
      val nv = values.map(v => if(p(v)) nextValue else v)
      copy(source, (nv(0), nv(1), nv(2)))
    }

    override def toString = repr.toString + source.expr.toString
  }

  case class ResultSeq(source: Roll, values: Seq[Int]) extends Rolled {

    def rerollBy(p: Int => Boolean) =
      copy(values = this.values.map(v => if(p(v)) source.nextValue else v))

    override def toString = values.mkString("(", ",", ")") + source.expr.toString
  }
}
