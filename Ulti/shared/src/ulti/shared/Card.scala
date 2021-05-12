package ulti.shared

import upickle.default.{macroRW, ReadWriter => RW}

abstract sealed class CardSuit {
  val name: String
}

object CardSuit {
  implicit def rw: RW[CardSuit] = macroRW

  val cardSuits: List[CardSuit] = List(Acorns, Bells, Hearts, Leaves)
}

case object Acorns extends CardSuit {
  override val name: String = "acorns"
}
case object Bells extends CardSuit {
  override val name: String = "bells"
}
case object Hearts extends CardSuit {
  override val name: String = "hearts"
}
case object Leaves extends CardSuit {
  override val name: String = "leaves"
}
case object Plain extends CardSuit {
  override val name: String = "plain"
}

abstract sealed class CardValue {
  def value(trumpSuit: CardSuit): Int
  val name: String
  def <(that: CardValue)(implicit adu: CardSuit): Boolean = this.value(adu) < that.value(adu)
  def <=(that: CardValue)(implicit adu: CardSuit): Boolean = this.value(adu) <= that.value(adu)
  def >(that: CardValue)(implicit adu: CardSuit): Boolean = this.value(adu) > that.value(adu)
  def >=(that: CardValue)(implicit adu: CardSuit): Boolean = this.value(adu) >= that.value(adu)
}

object CardValue {
  implicit def rw: RW[CardValue] = macroRW

  val cardValues: List[CardValue] = List(V7, V8, V9, V10, VUnder, VOver, VKing, VAce)
}

case object V7 extends CardValue {
  override def value(trumpSuit: CardSuit): Int = 7
  override val name: String = "7"
}

case object V8 extends CardValue {
  override def value(trumpSuit: CardSuit): Int = 8
  override val name: String = "8"
}

case object V9 extends CardValue {
  override def value(trumpSuit: CardSuit): Int = 9
  override val name: String = "9"
}

case object V10 extends CardValue {
  override def value(trumpSuit: CardSuit): Int = if (trumpSuit != Plain) 14 else 10
  override val name: String = "10"
}

case object VUnder extends CardValue {
  override def value(trumpSuit: CardSuit): Int = 11
  override val name: String = "under"
}

case object VOver extends CardValue {
  override def value(trumpSuit: CardSuit): Int = 12
  override val name: String = "over"
}

case object VKing extends CardValue {
  override def value(trumpSuit: CardSuit): Int = 13
  override val name: String = "king"
}

case object VAce extends CardValue {
  override def value(trumpSuit: CardSuit): Int = 15
  override val name: String = "ace"
}

case class Card(suit: CardSuit, value: CardValue) {
  val name: String = suit.name + "_" + value.name

  //WATCH OUT: partial ordering!
  def <(that: Card)(implicit adu: CardSuit): Boolean = {
    if (this.suit == that.suit)
      this.value < that.value
    else if (that.suit == adu)
      true
    else
      false
  }
  def >(that: Card)(implicit adu: CardSuit): Boolean = {
    if (this.suit == that.suit)
      this.value > that.value
    else if (this.suit == adu)
      true
    else
      false
  }
}

object Card {
  implicit def rw: RW[Card] = macroRW

  val deck: List[Card] = {
    (for (suit <- CardSuit.cardSuits) yield CardValue.cardValues.map(value => Card(suit, value))).flatten
  }

}
