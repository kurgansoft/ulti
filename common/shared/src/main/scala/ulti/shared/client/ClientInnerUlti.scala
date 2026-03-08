package ulti.shared.client

import ulti.shared.*
import ulti.shared.abstract0.*
import ulti.shared.contracts.ContractWithStatus
import zio.json.JsonCodec
import zio.schema.{DeriveSchema, Schema}

case class TakenTrick(
                       taker: UltiPlayer,
                       cards: Option[List[Card]]
                     ) {
  if (cards.isDefined) {
    assert(cards.get.size == 3)
  }

  def toClient(role: Option[UltiPlayer]): TakenTrick = {
    if (role.contains(taker))
      this
    else
      TakenTrick(taker, None)
  }
}

object TakenTrick {
  implicit val schema: Schema[TakenTrick] = DeriveSchema.gen[TakenTrick]
  implicit val codec: JsonCodec[TakenTrick] =
    zio.schema.codec.JsonCodec.jsonCodec(schema)
}

case class ClientInnerUlti(
                            override val round: Int,
                            override val phase: Phase,
                            override val startPlayer: UltiPlayer = UltiPlayer1,
                            override val currentPlayer: UltiPlayer = UltiPlayer1,
                            override val currentBid: Option[Bid] = None,
                            override val currentBidWinner: Option[UltiPlayer] = None,
                            override val trumpSuit: Option[CardSuit] = None,
                            override val contracts: Map[UltiPlayer, Set[ContractWithStatus]] = Map.empty,
                            override val declaredMarriages: Map[UltiPlayer, Set[CardSuit]] = Map.empty,
                            override val cardsOnTheTable: List[Card] = List.empty,
                            override val talonOnTheTable: Boolean = false,
                            playerHands: Map[UltiPlayer, Either[Int, Set[Card]]],
                            override val tricks: List[TakenTrick] = List.empty,
                            lastTrick: Option[TakenTrick] = None,
                            pointsScored: Map[UltiPlayer, Int] = Map.empty,
                            pickedUpTalon: Set[Card] = Set.empty,
                            override val blockIsLifted: Boolean = false,
                            override val valueInTheTalon: Int = 0
                          ) extends AbstractInnerUlti(round, phase, startPlayer, currentPlayer, currentBid, currentBidWinner, trumpSuit, contracts, declaredMarriages, cardsOnTheTable, talonOnTheTable) {
  val cardsOnTheTableToDisplay: List[Card] = {
    if (cardsOnTheTable.nonEmpty)
      cardsOnTheTable
    else if (phase == PlayingPhase && lastTrick.isDefined) {
      lastTrick.get.cards.get
    } else
      List.empty
  }

  override val currentPlayersHand: Set[Card] = playerHands(currentPlayer).getOrElse(Set.empty)

  def getSortedPlayerHand(player: UltiPlayer): Either[Int, List[Card]] = {
    val suitOrder = List(Hearts, Leaves, Acorns, Bells)
    playerHands(player).map(set => set.toList.sortWith((c1, c2) => {
      implicit val aceColor: CardSuit = Plain
      if (c1.suit == c2.suit)
        c1.value < c2.value
      else
        suitOrder.indexOf(c1.suit) < suitOrder.indexOf(c2.suit)
    }))
  }

  def availableMarriagesForPlayer(player: UltiPlayer): Set[CardSuit] = {
    val cards = playerHands(player).getOrElse(Set.empty)

    CardSuit.cardSuits.flatMap(suit => {
      if (cards.contains(Card(suit, VOver)) && cards.contains(Card(suit, VKing)))
        Some(suit)
      else
        None
    }).toSet
  }
}

object ClientInnerUlti {
  implicit val schema: Schema[ClientInnerUlti] = DeriveSchema.gen[ClientInnerUlti]
  implicit val codec: JsonCodec[ClientInnerUlti] =
    zio.schema.codec.JsonCodec.jsonCodec(schema)
}
