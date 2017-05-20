package scikoro.dice

final class Table[K, T](
  val dice: Roll,
  val resultMap: Map[K, T],
  val solver: Roll => K
) {

  def lookup(key: K): Option[T] = resultMap get key
  def nextEntry(): Option[T] = lookup(solver(dice))
}

object Table {

  def withKeyNumbers[T](roll: Roll)(body: (Int, T)*): Table[Int, T] =
    new Table(roll, body.toMap, ((r: Roll) => r.roll.first))
}
