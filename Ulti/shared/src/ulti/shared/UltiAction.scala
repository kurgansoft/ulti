package ulti.shared

import gbge.shared.actions.GameAction
import ulti.shared.abstract0.UltiPlayer
import ulti.shared.contracts.Contract
import upickle.default.{macroRW, ReadWriter => RW}

abstract sealed class UltiAction extends GameAction {
  override def serialize(): String = upickle.default.write[UltiAction](this)
}

object UltiAction {
  implicit def rw: RW[UltiAction] = macroRW
}

case object Init extends UltiAction {
  override val adminOnly: Boolean = true
}

case class DealCards(startPlayer: UltiPlayer, seed: Long) extends UltiAction {
  override val systemOnly: Boolean = true
}

object DealCards {
  implicit def rw: RW[DealCards] = macroRW
}

case class SubmitBid(bid: Bid, talon: Set[Card]) extends UltiAction {
  assert(talon.size == 2)
}

object SubmitBid {
  implicit def rw: RW[SubmitBid] = macroRW
}

case object PickUpTalon extends UltiAction

case object SkipBid extends UltiAction

case class DeclareTrumpSuit(suit: CardSuit) extends UltiAction

object DeclareTrumpSuit {
  implicit def rw: RW[DeclareTrumpSuit] = macroRW
}

case class DeclareMarriages(suits: Set[CardSuit]) extends UltiAction {
  def value()(implicit trumpColor: CardSuit): Int = {
    val temp = suits.size*20
    if (suits.contains(trumpColor)) {
      temp + 20
    } else {
      temp
    }
  }
}

object DeclareMarriages {
  implicit def rw: RW[DeclareMarriages] = macroRW
}

case class PlayCard(card: Card) extends UltiAction

object PlayCard {
  implicit def rw: RW[PlayCard] = macroRW
}

case class DoubleContract(contract: Contract) extends UltiAction

object DoubleContract {
  implicit def rw: RW[DoubleContract] = macroRW
}

case object DoneWithDoubling extends UltiAction

case object ReadyForNextRound extends UltiAction