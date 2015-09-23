package at.bayava.domain

import scala.collection.mutable

/**
 * Created by pbayer.
 */
object Colors {

  private val colorList = mutable.MutableList[Color]()

  sealed abstract class Color(val value: Char) {
    colorList += this
  }

  case object CLUBS extends Color('c')

  case object HEARTHS extends Color('h')

  case object DIAMONDS extends Color('d')

  case object SPADES extends Color('s')

  def fromChar(value: Char): Color = {
    def valLower = value.toLower
    colorList.find((c: Color) => c.value == valLower).getOrElse(throw new IllegalArgumentException(s"Unknown color '$value'!"))
  }
}
