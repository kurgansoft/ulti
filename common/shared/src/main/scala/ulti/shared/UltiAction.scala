package ulti.shared

import gbge.shared.actions.GameAction
import ulti.shared.abstract0.UltiPlayer
import ulti.shared.contracts.Contract
import zio.json.{EncoderOps, JsonCodec}
import zio.schema.{DeriveSchema, Schema}

sealed trait UltiAction extends GameAction {
  override def convertToJson(): String =
    this.toJson
}

object UltiAction {
  implicit val schema: Schema[UltiAction] = DeriveSchema.gen
  implicit val codec: JsonCodec[UltiAction] = 
    zio.schema.codec.JsonCodec.jsonCodec(schema)
}

case object Init extends UltiAction {
  override val adminOnly: Boolean = true
}

case class DealCards(startPlayer: UltiPlayer, seed: Long) extends UltiAction {
  override val systemOnly: Boolean = true
}

case class SubmitBid(bid: Bid, talon: Set[Card]) extends UltiAction {
  assert(talon.size == 2)
}

case object PickUpTalon extends UltiAction

case object SkipBid extends UltiAction

case class DeclareTrumpSuit(suit: CardSuit) extends UltiAction

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

case class PlayCard(card: Card) extends UltiAction

case class DoubleContract(contract: Contract) extends UltiAction

case object DoneWithDoubling extends UltiAction

case object ReadyForNextRound extends UltiAction