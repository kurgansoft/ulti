package ulti.shared

import upickle.default.{macroRW, ReadWriter => RW}

abstract sealed class Bid {
  val value: List[Int]
  val canBeRed: Boolean = true
  val trumpGame: Boolean = true
  val canMarriagesBeDeclared: Boolean = true
  val canBeUpgradedToPlain: Boolean = false
  val open: Boolean = false

  lazy val stringWithValue: String = {
    val sum = value.sum
    val signSeparated = value.mkString("+")
    value.size match {
      case 1 => toString + " (" + sum + ")"
      case _ => toString + " (" + signSeparated + "=" + sum + ")"
    }
  }

  def <(that: Bid): Boolean = {
    val sumThis = this.value.sum
    val sumThat = that.value.sum
    if (sumThis != sumThat)
      sumThis < sumThat
    else
      that.value.size < this.value.size
  }

  def <=(that: Bid): Boolean = {
    val sumThis = this.value.sum
    val sumThat = that.value.sum
    if (sumThis != sumThat)
      sumThis <= sumThat
    else
      that.value.size <= this.value.size
  }

  def >(that: Bid): Boolean = {
    val sumThis = this.value.sum
    val sumThat = that.value.sum
    if (sumThis != sumThat)
      sumThis > sumThat
    else
      that.value.size > this.value.size
  }

  def >=(that: Bid): Boolean = {
    val sumThis = this.value.sum
    val sumThat = that.value.sum
    if (sumThis != sumThat)
      sumThis >= sumThat
    else
      that.value.size >= this.value.size
  }
}

object Bid {
  implicit def rw: RW[Bid] = macroRW

  val topBid: Bid = Red(CombinedBid(Set(Bid20_100, BidUlti, BidDurchmars(open = true))))
}

sealed trait CombinableBid extends Bid

object CombinableBid {
  implicit def rw: RW[CombinableBid] = macroRW
}

case class CombinedBid(components: Set[CombinableBid]) extends Bid {
  assert(
    !(components.contains(Bid20_100) && components.contains(Bid40_100))
  )
  assert(components.size >= 2)
  override val value: List[Int] = components.toList.map(_.value.head).sorted.reverse

  override def toString: String = components.toList.sortBy(_.value.head).mkString(" ")

  override val canMarriagesBeDeclared: Boolean = components.forall(_.canMarriagesBeDeclared)

  override val open: Boolean = components.exists(_.open)
}

object CombinedBid {
  implicit def rw: RW[CombinedBid] = macroRW
}

case object BidSimple extends Bid {
  override val value: List[Int] = List(1)

  override def toString: String = "Simple"
}

case object Bid40_100 extends CombinableBid {
  override val value: List[Int] = List(4)
  override val canMarriagesBeDeclared: Boolean = false

  override def toString: String = "40-100"
}

case object BidUlti extends CombinableBid {
  override val value: List[Int] = List(4,1)

  override def toString: String = "Ulti"
}

case class BidBetli(override val open: Boolean = false) extends Bid {
  override val value: List[Int] = if (open) List(10) else List(5)
  override val trumpGame: Boolean = false
  override val canMarriagesBeDeclared: Boolean = false

  override def toString: String = if (open) "Open Betli" else "Betli"
}

object BidBetli {
  implicit def rw: RW[BidBetli] = macroRW
}

case object Bid20_100 extends CombinableBid {
  override val value: List[Int] = List(6)
  override val canMarriagesBeDeclared: Boolean = false

  override def toString: String = "20-100"
}

case class BidDurchmars(override val open: Boolean = false) extends CombinableBid {
  override val value: List[Int] = if (open) List(14) else List(7)
  override val canMarriagesBeDeclared: Boolean = false

  override def toString: String = if (open) "Open Durchmars" else "Durchmars"

  override val canBeUpgradedToPlain: Boolean = true
}

object BidDurchmars {
  implicit def rw: RW[BidDurchmars] = macroRW
}

case class BidPlainDurchmars(override val open: Boolean = false) extends Bid {
  override val value: List[Int] = if (open) List(28) else List(14)
  override val canBeRed: Boolean = false
  override val trumpGame: Boolean = false
  override val canMarriagesBeDeclared: Boolean = false

  override def toString: String = if (open) "Open Plain Durchmars" else "Plain Durchmars"
}

object BidPlainDurchmars {
  implicit def rw: RW[BidPlainDurchmars] = macroRW
}

case class Red(content: Bid) extends Bid {
  override val canBeRed: Boolean = false
  override val value: List[Int] = content.value.map(_*2)
  override val trumpGame: Boolean = content.trumpGame
  assert(content.canBeRed)

  val unRed: Bid = content

  override def toString: String = "Red " + content.toString

  override val open: Boolean = content.open
  override val canMarriagesBeDeclared: Boolean = content.canMarriagesBeDeclared
}

object Red {
  implicit def rw: RW[Red] = macroRW
}