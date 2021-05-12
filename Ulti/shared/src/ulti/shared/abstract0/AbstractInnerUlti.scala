package ulti.shared.abstract0

import gbge.shared.FrontendPlayer
import monocle.Lens
import ulti.shared._
import ulti.shared.client.TakenTrick
import ulti.shared.contracts._

abstract class AbstractInnerUlti(
                                  val round: Int,
                                  val phase: Phase,
                                  val startPlayer: UltiPlayer,
                                  val currentPlayer: UltiPlayer,
                                  val currentBid: Option[Bid],
                                  val currentBidWinner: Option[UltiPlayer],
                                  val trumpSuit: Option[CardSuit],
                                  val contracts: Map[UltiPlayer, Set[ContractWithStatus]],
                                  val declaredMarriages: Map[UltiPlayer, Set[CardSuit]],
                                  val cardsOnTheTable: List[Card],
                                  val talonOnTheTable: Boolean,
                                  val tricks: List[TakenTrick] = List.empty,
                                  val blockIsLifted: Boolean = false, // Becomes true when the soloist clicks on "DONE WITH DOUBLING"
                                  val valueInTheTalon: Int = 0
                                ) {

  val currentPlayersHand: Set[Card]

  def bidStatusText()(implicit players: List[FrontendPlayer]) = {
    val cpName = players.find(_.role.contains(currentPlayer.roleId)).map(_.name).getOrElse(FrontendPlayer.MISSING_PLAYERS_NAME)
    lazy val currentBidWinnersName = currentBidWinner.map(x => FrontendPlayer.getNameOfPlayerWithRole(x.roleId)(players)).get
    this.phase match {
      case BiddingPhase => {
        if (currentBid.isEmpty)
          s"Wating for $cpName to submit it's bid."
        else
          s"Current winning bid: ${currentBid.get} ($currentBidWinnersName)"
      }
      case PlayingPhase | GameOverPhase => {
        s"Bid: ${currentBid.get} ($currentBidWinnersName)"
      }
      case _ => ""
    }
  }

  val winningBidTuple: Option[(UltiPlayer, Bid)] = {
    if (currentBidWinner.isDefined && currentBid.isDefined) {
      Some((currentBidWinner.get, currentBid.get))
    } else {
      None
    }
  }

  val pointsScored: Map[UltiPlayer, Int]

  def playerHandLen(player: UltiPlayer): Lens[Map[UltiPlayer, Set[Card]], Set[Card]] =
    Lens[Map[UltiPlayer, Set[Card]], Set[Card]](playerHands => playerHands(player))(playerHand => playerHands => {
      playerHands.updated(player, playerHand)
    })

  def contractsOfAPlayerLen(player: UltiPlayer): Lens[Map[UltiPlayer, Set[ContractWithStatus]], Set[ContractWithStatus]] =
    Lens[Map[UltiPlayer, Set[ContractWithStatus]], Set[ContractWithStatus]](contracts => contracts(player))(contract => contracts => {
      contracts.updated(player, contract)
    })

  lazy val contractsOfTheBidWinnerLen = contractsOfAPlayerLen(currentBidWinner.get)

  lazy val contractsOfTheBidWinner: List[ContractWithStatus] = {
    if (currentBidWinner.isEmpty || !contracts.keys.exists(_ == currentBidWinner.get)) {
      List.empty
    } else {
      contracts(currentBidWinner.get).toList.sortBy(_.contract.value)
    }
  }

  def getUltiPlayerFromRoleId(roleId: Int): Option[UltiPlayer] = roleId match {
    case 1 => Some(UltiPlayer1)
    case 2 => Some(UltiPlayer2)
    case 3 => Some(UltiPlayer3)
    case _ => None
  }

  val roundCaller: Option[UltiPlayer] = {
    if (phase != PlayingPhase)
      None
    else {
      cardsOnTheTable.size match {
        case 0 => Some(currentPlayer)
        case 1 => Some(currentPlayer.nextPlayer.nextPlayer)
        case 2 => Some(currentPlayer.nextPlayer)
        case _ => Some(currentPlayer)
      }
    }
  }

  lazy val winnerOfTheLastRound: UltiPlayer = {
    assert(round == 10)
    tricks.last.taker
  }

  def gainForPlayerFromContract(ultiPlayer: UltiPlayer, contractWithStatus: ContractWithStatus, underTaker: Boolean): Int = {
    if (contractWithStatus.contractResult == Open)
      0
    else {
      val cl = contractWithStatus.contraLevel
      val baseValueOfTheContract = contractWithStatus.contract.value
      val result = contractWithStatus.contractResult
      val baseValue2: Int = if (underTaker) {
        cl match {
          case NoContra => {
            baseValueOfTheContract * 2
          }
          case JointContra(level) => {
            baseValueOfTheContract * 2 * Math.pow(2, level).toInt
          }
          case IndividualContra(map) => {
            map.values.map(level => {
              baseValueOfTheContract * Math.pow(2, level).toInt
            }).sum
          }
        }
      } else {
        baseValueOfTheContract * Math.pow(2, cl.levelForPlayer(ultiPlayer)).toInt
      }
      if ((result == Fulfilled && underTaker) || (result == Failed && !underTaker)) {
        baseValue2
      } else {
        -baseValue2
      }
    }
  }

  def totalGainsForPlayer(ultiPlayer: UltiPlayer): Int = {
    assert(phase == GameOverPhase)
    contracts.keys.map(player => {
      contracts(player).toList.map(gainForPlayerFromContract(ultiPlayer, _, ultiPlayer == player)).sum
    }).sum
  }

  def bonusPointsFromDeclaredMarriagesForPlayer(player: UltiPlayer): Int = {
    if (declaredMarriages.contains(player))
      declaredMarriages(player).map(suit => if (trumpSuit.contains(suit)) 4 else 2).sum
    else
      0
  }

  def contractsThatCanBeDoubledByPlayer(player: UltiPlayer): List[ContractWithStatus] = {
    if (phase != PlayingPhase || round != 1) {
      List.empty
    } else {
      if (currentBidWinner.contains(player)) {
        contractsOfTheBidWinner.filterNot(_.contract.silent).filter(contractWithStatus => {
          contractWithStatus.contraLevel.levelForPlayer(currentPlayer) % 2 == 1 &&
          contractWithStatus.contraLevel.levelForPlayer(currentPlayer) < ContraLevel.maximumContraLevel
        }).toList.sortBy(_.contract.value)
      } else if (player == currentPlayer) {
        contractsOfTheBidWinner.filterNot(_.contract.silent).filter(contractWithStatus => {
          contractWithStatus.contraLevel.levelForPlayer(currentPlayer) % 2 == 0 &&
          contractWithStatus.contraLevel.levelForPlayer(currentPlayer) < ContraLevel.maximumContraLevel
        }).toList.sortBy(_.contract.value)
      } else {
        List.empty
      }
    }
  }

  lazy val couldSoloistRedouble: Boolean = {
    round == 1 &&
    phase == PlayingPhase &&
    contracts(currentBidWinner.get).exists(_.contraLevel.levelForPlayer(currentPlayer) % 2 == 1)
  }

  def validMove(card: Card): Option[String] = {
    if (cardsOnTheTable.isEmpty)
      None
    else {
      implicit val adu: CardSuit = trumpSuit.get
      val color = cardsOnTheTable.head.suit
      if (couldSoloistRedouble && !blockIsLifted) {
        Some("Waiting for soloist to redouble or lift the block.")
      } else if (card.suit != color && currentPlayersHand.exists(_.suit == color)) {
        Some("Suit must be followed.")
      } else if (card.suit != cardsOnTheTable.head.suit && trumpSuit.get != Plain && card.suit != trumpSuit.get && currentPlayersHand.exists(_.suit == trumpSuit.get)) {
        Some("Trump suit have to be called if you lack the proper suit.")
      } else if (cardsOnTheTable.size == 1 && card.suit == color && card.value < cardsOnTheTable.last.value && currentPlayersHand.exists(c => c.suit == card.suit && c.value > cardsOnTheTable.last.value) ) {
        Some("A higher valued card must be played if possible.")
      } else if (cardsOnTheTable.size == 2) {
        if (card.suit == color &&  card < cardsOnTheTable.head && cardsOnTheTable.last.suit != color && !trumpSuit.contains(cardsOnTheTable.last.suit) &&
          currentPlayersHand.exists(c => c.suit == color && (c.value > cardsOnTheTable.head.value))) {
          Some("A higher valued card must be played if possible.")
        } else if (card.suit == color && cardsOnTheTable.last.suit == color && (card < cardsOnTheTable.head || card < cardsOnTheTable.last) &&
          currentPlayersHand.exists(c => c.suit == color && (c.value > cardsOnTheTable.head.value && c.value > cardsOnTheTable.last.value))) {
          Some("A higher valued card must be played if possible.")
        } else if (card.suit == adu && cardsOnTheTable.head.suit == adu && cardsOnTheTable.last.suit != adu && card.value < cardsOnTheTable.head.value &&
          currentPlayersHand.exists(c => c.suit == adu && c.value > cardsOnTheTable.head.value )) {
          Some("Your card must have a higher value than the previous trump card.")
        } else {
          None
        }
      } else {
        None
      }
    }
  }

  lazy val nextCardCanBePlayed: Boolean = {
    blockIsLifted ||
    contractsThatCanBeDoubledByPlayer(currentBidWinner.get).isEmpty
  }
}
