package at.bayava.domain

/**
 * Created by pbayer.
 */
object Colors {


  sealed abstract class Color(val value: Char) {
  }

  case object CLUBS extends Color('c')

  case object HEARTHS extends Color('h')

  case object DIAMONDS extends Color('d')

  case object SPADES extends Color('s')

  private val colorList = CLUBS :: HEARTHS :: DIAMONDS :: SPADES :: Nil

  def fromChar(value: Char): Color = {
    def valLower = value.toLower
    colorList.find((c: Color) => c.value == valLower).getOrElse(throw new IllegalArgumentException(s"Unknown color '$value'!"))
  }
}
