package ulti.shared.contracts

import ulti.shared._
import ulti.shared.abstract0.{AbstractInnerUlti, UltiPlayer}
import zio.json.JsonCodec
import zio.schema.{DeriveSchema, Schema}

sealed trait ContractResult

case object Fulfilled extends ContractResult
case object Failed extends ContractResult
case object Open extends ContractResult

object ContractResult {
  implicit val schema: Schema[ContractResult] = DeriveSchema.gen[ContractResult]
  implicit val codec: JsonCodec[ContractResult] = zio.schema.codec.JsonCodec.jsonCodec(schema)
}

sealed trait Contract {
  val trumpColor: CardSuit
  val open: Boolean = false
  val silent: Boolean = false
  val losingCondition: (AbstractInnerUlti, UltiPlayer) => Boolean = (_,_) => false
  val winningCondition: (AbstractInnerUlti, UltiPlayer) => Boolean = (_,_) => false
  val openMultiplier: Int = if (open) 2 else 1
  val value: Int

  private val toStringTrumpPrefix: String = trumpColor match {
    case Hearts => "Red "
    case Plain => "Plain "
    case _ => ""
  }

  private val toStringOpenPrefix: String = if (open) "Open " else ""

  val toStringMainPart: String

  override def toString: String = toStringTrumpPrefix + toStringOpenPrefix + toStringMainPart
}

object Contract {
  implicit val schema: Schema[Contract] = DeriveSchema.gen[Contract]
  implicit val codec: JsonCodec[Contract] = zio.schema.codec.JsonCodec.jsonCodec(schema)
}

case class ContractSimple(override val trumpColor: CardSuit) extends Contract {
  assert(trumpColor != Plain)
  val value: Int = if (trumpColor == Hearts) 2 else 1

  override val losingCondition: (AbstractInnerUlti, UltiPlayer) => Boolean = (innerUlti, ultiPlayer) => {
    val playersPoints = innerUlti.pointsScored(ultiPlayer) + innerUlti.bonusPointsFromDeclaredMarriagesForPlayer(ultiPlayer)
    val pointsOfOtherPlayer1 = innerUlti.pointsScored(ultiPlayer.nextPlayer) + innerUlti.bonusPointsFromDeclaredMarriagesForPlayer(ultiPlayer.nextPlayer)
    val pointsOfOtherPlayer2 = innerUlti.pointsScored(ultiPlayer.nextPlayer.nextPlayer)  + innerUlti.bonusPointsFromDeclaredMarriagesForPlayer(ultiPlayer.nextPlayer.nextPlayer)

    innerUlti.round == 10 &&
    (playersPoints < pointsOfOtherPlayer1 + pointsOfOtherPlayer2 + innerUlti.valueInTheTalon)
  }
  override val winningCondition: (AbstractInnerUlti, UltiPlayer) => Boolean = (innerUlti, ultiPlayer) => {
    val playersPoints = innerUlti.pointsScored(ultiPlayer) + innerUlti.bonusPointsFromDeclaredMarriagesForPlayer(ultiPlayer)
    val pointsOfOtherPlayer1 = innerUlti.pointsScored(ultiPlayer.nextPlayer) + innerUlti.bonusPointsFromDeclaredMarriagesForPlayer(ultiPlayer.nextPlayer)
    val pointsOfOtherPlayer2 = innerUlti.pointsScored(ultiPlayer.nextPlayer.nextPlayer)  + innerUlti.bonusPointsFromDeclaredMarriagesForPlayer(ultiPlayer.nextPlayer.nextPlayer)

    innerUlti.round == 10 &&
    (playersPoints > pointsOfOtherPlayer1 + pointsOfOtherPlayer2 + innerUlti.valueInTheTalon)
  }

  override val toStringMainPart: String = "Simple"

}

case class Contract40_100(override val trumpColor: CardSuit) extends Contract {
  assert(trumpColor != Plain)

  val value: Int = if (trumpColor == Hearts) 8 else 4

  override val winningCondition: (AbstractInnerUlti, UltiPlayer) => Boolean = (innerUlti, ultiPlayer) => {
    innerUlti.round == 10 && innerUlti.pointsScored(ultiPlayer) + 4 >= 10
  }

  override val losingCondition: (AbstractInnerUlti, UltiPlayer) => Boolean = (innerUlti, ultiPlayer) => {
    innerUlti.round == 10 && innerUlti.pointsScored(ultiPlayer) + 4 < 10
  }

  override val toStringMainPart: String = "40-100"
}

case class Contract20_100(override val trumpColor: CardSuit) extends Contract {
  assert(trumpColor != Plain)

  val value: Int = if (trumpColor == Hearts) 12 else 6

  override val winningCondition: (AbstractInnerUlti, UltiPlayer) => Boolean = (innerUlti, ultiPlayer) => {
    innerUlti.round == 10 && innerUlti.pointsScored(ultiPlayer) + 2 >=10
  }

  override val losingCondition: (AbstractInnerUlti, UltiPlayer) => Boolean = (innerUlti, ultiPlayer) => {
    innerUlti.round == 10 && innerUlti.pointsScored(ultiPlayer) + 2 < 10
  }

  override val toStringMainPart: String = "20-100"
}

case class ContractUlti(override val trumpColor: CardSuit) extends Contract {
  assert(trumpColor != Plain)

  val value: Int = if (trumpColor == Hearts) 8 else 4

  override val losingCondition: (AbstractInnerUlti, UltiPlayer) => Boolean = (innerUlti, player) => {
    innerUlti.round == 10 &&
      (innerUlti.winnerOfTheLastRound != player || !innerUlti.tricks(9).cards.get.contains(Card(innerUlti.trumpSuit.get, V7)))
  }
  override val winningCondition: (AbstractInnerUlti, UltiPlayer) => Boolean = ContractUlti.winningCondition

  override val toStringMainPart: String = "Ulti"
}

object ContractUlti {
  val winningCondition: (AbstractInnerUlti, UltiPlayer) => Boolean = (innerUlti, player) => {
    innerUlti.round == 10 &&
      (innerUlti.winnerOfTheLastRound == player && innerUlti.tricks(9).cards.get.contains(Card(innerUlti.trumpSuit.get, V7)))
  }
}

case class ContractBetli(red: Boolean = false, override val open: Boolean = false) extends Contract {
  override val trumpColor: CardSuit = Plain
  override val losingCondition: (AbstractInnerUlti, UltiPlayer) => Boolean = (innerUlti, player) => {
    innerUlti.tricks.exists(_.taker == player)
  }
  override val winningCondition: (AbstractInnerUlti, UltiPlayer) => Boolean = (innerUlti, player) => {
    innerUlti.round == 10 && !innerUlti.tricks.exists(_.taker == player)
  }

  val baseValue = 5
  val redMultiplier: Int = if (red) 2 else 1
  val value: Int = baseValue * redMultiplier * openMultiplier

  override val toStringMainPart: String = "Betli"
}

case class ContractDurchmars(override val trumpColor: CardSuit, override val open: Boolean = false) extends Contract {
  val baseValue = 7
  val colorMultiplier: Int = if (trumpColor == Plain || trumpColor == Hearts) 2 else 1
  val value: Int = baseValue * openMultiplier * colorMultiplier

  override val winningCondition: (AbstractInnerUlti, UltiPlayer) => Boolean = ContractDurchmars.winningCondition

  override val losingCondition: (AbstractInnerUlti, UltiPlayer) => Boolean = (innerUlti, ultiPlayer) => {
    innerUlti.tricks.exists(_.taker != ultiPlayer)
  }

  override val toStringMainPart: String = "Durchmars"
}

object ContractDurchmars {
  val winningCondition: (AbstractInnerUlti, UltiPlayer) => Boolean = (innerUlti, ultiPlayer) => {
    innerUlti.round == 10 && innerUlti.tricks.forall(_.taker == ultiPlayer)
  }
}

case class ContractSilentUlti(override val trumpColor: CardSuit) extends Contract {
  assert(trumpColor != Plain)
  override val silent: Boolean = true

  override val winningCondition: (AbstractInnerUlti, UltiPlayer) => Boolean = ContractUlti.winningCondition

  override val losingCondition: (AbstractInnerUlti, UltiPlayer) => Boolean = (innerUlti, player) => {

    val theCard = Card(innerUlti.trumpSuit.get, V7)
    val cardIndexes: Map[UltiPlayer, Int] = Map(
      innerUlti.currentPlayer -> 2,
      innerUlti.currentPlayer.nextPlayer -> 0,
      innerUlti.currentPlayer.nextPlayer.nextPlayer -> 1
    )

    innerUlti.round == 10 &&
    innerUlti.winnerOfTheLastRound != player &&
    innerUlti.tricks(9).cards.get.contains(theCard) &&
    innerUlti.cardsOnTheTable(cardIndexes(player)) == theCard
  }

  val value: Int = if (trumpColor == Hearts) 4 else 2

  override val toStringMainPart: String = "Silent Ulti"
}

case class ContractSilent100(override val trumpColor: CardSuit) extends Contract {
  assert(trumpColor != Plain)
  override val silent: Boolean = true

  val value: Int = if (trumpColor == Hearts) 4 else 2

  override val winningCondition: (AbstractInnerUlti, UltiPlayer) => Boolean = (innerUlti, player) => {
    innerUlti.pointsScored(player) + innerUlti.bonusPointsFromDeclaredMarriagesForPlayer(player) >= 10
  }

  override val losingCondition: (AbstractInnerUlti, UltiPlayer) => Boolean = (innerUlti, player) => {
    (innerUlti.pointsScored(player.nextPlayer) +
    innerUlti.bonusPointsFromDeclaredMarriagesForPlayer(player.nextPlayer) +
    innerUlti.pointsScored(player.nextPlayer.nextPlayer) +
    innerUlti.bonusPointsFromDeclaredMarriagesForPlayer(player.nextPlayer.nextPlayer) +
    innerUlti.valueInTheTalon) >= 10
  }

  override val toStringMainPart: String = "Silent 100"
}

case class ContractSilentDurchmars(override val trumpColor: CardSuit) extends Contract {
  assert(trumpColor != Plain)
  override val silent: Boolean = true
  override val winningCondition: (AbstractInnerUlti, UltiPlayer) => Boolean = ContractDurchmars.winningCondition

  val value: Int = if (trumpColor == Hearts) 7 else 4

  override val toStringMainPart: String = "Silent Durchmars"
}