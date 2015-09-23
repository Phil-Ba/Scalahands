package at.bayava.domain

import scala.collection.mutable

/**
 * Created by pbayer.
 */
object Values {

  private val valueList = mutable.MutableList[Value]()

  sealed abstract class Value(val value: Char, val ordinal: Short) extends Ordered[Value] {
    valueList += this

    override def compare(that: Value): Int = this.ordinal.compare(that.ordinal)
  }

  case object TWO extends Value('2', 2)

  case object THREE extends Value('3', 3)

  case object FOUR extends Value('4', 4)

  case object FIVE extends Value('5', 5)

  case object SIX extends Value('6', 6)

  case object SEVEN extends Value('7', 7)

  case object EIGHT extends Value('8', 8)

  case object NINE extends Value('9', 9)

  case object TEN extends Value('t', 10)

  case object JACK extends Value('j', 11)

  case object QUEEN extends Value('q', 12)

  case object KING extends Value('k', 13)

  case object ACE extends Value('a', 14)

  def fromChar(value: Char): Value = {
    def valLower = value.toLower
    valueList.find((v: Value) => v.value == valLower).getOrElse(throw new IllegalArgumentException(s"Unknown card value '$value'!"))
  }
}
