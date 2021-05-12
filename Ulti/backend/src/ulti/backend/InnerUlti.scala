package ulti.backend

import gbge.backend.{GeneralFailure, OK, UniverseResult}
import monocle.Lens
import ulti.shared.abstract0._
import ulti.shared._
import ulti.shared.client.{ClientInnerUlti, TakenTrick}
import ulti.shared.contracts._

import scala.util.Random

case class InnerUlti(
                      override val round: Int = 1,
                      override val phase: Phase = BiddingPhase,
                      override val startPlayer: UltiPlayer = UltiPlayer1,
                      override val currentPlayer: UltiPlayer = UltiPlayer1,
                      override val currentBid: Option[Bid] = None,
                      override val currentBidWinner: Option[UltiPlayer] = None,
                      override val trumpSuit: Option[CardSuit] = None,
                      override val contracts: Map[UltiPlayer, Set[ContractWithStatus]] = Map.empty,
                      override val declaredMarriages: Map[UltiPlayer, Set[CardSuit]] = Map.empty,
                      override val cardsOnTheTable: List[Card] = List.empty,
                      override val talonOnTheTable: Boolean = false,
                      talon: Set[Card] = Set.empty,
                      playerHands: Map[UltiPlayer, Set[Card]],
                      override val tricks: List[TakenTrick] = List.empty,
                      override val blockIsLifted: Boolean = false
                    ) extends AbstractInnerUlti(round, phase, startPlayer, currentPlayer, currentBid, currentBidWinner, trumpSuit, contracts, declaredMarriages, cardsOnTheTable, talonOnTheTable) {
  def invariant(): Unit = {
    if (phase == BiddingPhase) {
      assert(talon.isEmpty || talon.size == 2)
      playerHands.values.foreach(hand => {
        assert(hand.size == 10 || hand.size == 12)
      })
      if (talonOnTheTable) {
        playerHands.values.foreach(hand => {
          assert(hand.size == 10)
        })
      }
    }
  }

  override val currentPlayersHand: Set[Card] = {
    if (playerHands.contains(currentPlayer))
      playerHands(currentPlayer)
    else
      Set.empty
  }
  override val valueInTheTalon: Int = talon.toList.map(_.value).map(value => {
    if (value == V10 || value == VAce) 1 else 0
  }).sum

  private def declaredMarriageLen(player: UltiPlayer): Lens[Map[UltiPlayer, Set[CardSuit]], Set[CardSuit]] =
    Lens[Map[UltiPlayer, Set[CardSuit]], Set[CardSuit]](declareMarriages => declareMarriages(player))(suits => declaredMarriages => {
      declaredMarriages.updated(player, suits)
    })

  def calculateContractForDefender(player: UltiPlayer, trumpSuit: CardSuit = Plain, contractsOfTheBidWinner: Set[Contract]): Set[Contract] = {
    val sd = contractsOfTheBidWinner.exists(_.isInstanceOf[ContractSilentDurchmars])
    val su = playerHands(player).contains(Card(trumpSuit, V7)) &&
      contractsOfTheBidWinner.exists(contract => contract.isInstanceOf[ContractUlti] || contract.isInstanceOf[ContractSilentUlti])
    Set(
      if (sd) Some(ContractSilentDurchmars(trumpSuit)) else None,
      if (su) Some(ContractSilentUlti(trumpSuit)) else None,
    ).flatten
  }

  def calculateContractsForBidWinner(bid: Bid, trumpSuit: CardSuit = Plain, atLeastOneMarriageIsDeclared: Boolean = false): Set[Contract] = {
    lazy val silent100: Set[Contract] = {
      if (atLeastOneMarriageIsDeclared) {
        Set(ContractSilent100(trumpSuit))
      } else {
        Set.empty
      }
    }

    bid match {
      case BidSimple => Set(ContractSimple(trumpSuit), ContractSilentUlti(trumpSuit), ContractSilentDurchmars(trumpSuit)) ++ silent100
      case BidUlti => Set(ContractSimple(trumpSuit), ContractUlti(trumpSuit), ContractSilentDurchmars(trumpSuit)) ++ silent100
      case Bid40_100 => Set(Contract40_100(trumpSuit), ContractSilentUlti(trumpSuit), ContractSilentDurchmars(trumpSuit))
      case Bid20_100 => Set(Contract20_100(trumpSuit), ContractSilentUlti(trumpSuit), ContractSilentDurchmars(trumpSuit))
      case BidDurchmars(open) => Set(ContractDurchmars(trumpSuit, open))
      case BidPlainDurchmars(open) => Set(ContractDurchmars(Plain, open))
      case BidBetli(open) => {
        Set(ContractBetli(open = open))
      }
      case Red(content) => {
        content match {
          case bidBetli: BidBetli => {
            Set(ContractBetli(red = true, open = bidBetli.open))
          }
          case _ => calculateContractsForBidWinner(content, Hearts)
        }
      }
      case CombinedBid(components) => {
        var initialSet = components.map(calculateContractsForBidWinner(_, trumpSuit)).fold(Set.empty)((set1, set2) => set1 ++ set2)
        if (components.contains(BidUlti)) {
          initialSet = initialSet.filterNot(contract => {
            contract.isInstanceOf[ContractSilentUlti]
          })
        }
        if (components.contains(Bid20_100) || components.contains(Bid40_100)) {
          initialSet = initialSet.filterNot(contract => {
            contract.isInstanceOf[ContractSimple] ||
            contract.isInstanceOf[ContractSilent100]
          })
        }
        if (components.exists(_.isInstanceOf[BidDurchmars])) {
          initialSet = initialSet.filterNot(contract => {
            contract.isInstanceOf[ContractSimple] ||
            contract.isInstanceOf[ContractSilentUlti] ||
            contract.isInstanceOf[ContractSilentDurchmars] ||
            contract.isInstanceOf[ContractSilent100]

          })
        }
        initialSet
      }
      case _ => Set.empty
    }
  }

  private def doesPlayerHaveAMarriageInSuit(player: UltiPlayer, suit: CardSuit): Boolean = {
    playerHands(player).contains(Card(suit, VOver)) && playerHands(player).contains(Card(suit, VKing))
  }

  private val newRoundNumber = if (currentPlayer.nextPlayer == startPlayer) round+1 else round

  override val pointsScored: Map[UltiPlayer, Int] = Map(
    UltiPlayer1 -> tricks.filter(_.taker == UltiPlayer1),
    UltiPlayer2 -> tricks.filter(_.taker == UltiPlayer2),
    UltiPlayer3 -> tricks.filter(_.taker == UltiPlayer3)
  ).map {
    case (player, tricks) => {
      val lastTrickBonus = if (round == 10 && winnerOfTheLastRound == player) 1 else 0
      (player, tricks.filter(_.cards.isDefined).map(_.cards.get).fold(List.empty)(_ ::: _)
        .count(card => card.value == V10 || card.value == VAce) + lastTrickBonus)
    }
  }

  def toClientInnerUlti(role: Option[UltiPlayer] = None): ClientInnerUlti = {
    val ph: Map[UltiPlayer, Either[Int, Set[Card]]] =
      playerHands.transform((ultiPlayer, value) => {
        if (role.contains(ultiPlayer) || (phase == PlayingPhase && round >=2 && currentBid.exists(_.open)))
          Right(value)
        else
          Left(value.size)
      })
    val lastTrick = if (cardsOnTheTable.isEmpty && tricks.nonEmpty) Some(tricks.last) else None

    val pickedUpTalon: Set[Card] = if (role.map(playerHands(_)).exists(_.size == 12)) talon else Set.empty

    val valueInTheTalonForClient: Int = {
      if (phase == GameOverPhase || (phase == PlayingPhase && round == 10 && cardsOnTheTable.size == 3))
        valueInTheTalon
      else
        0
    }
    ClientInnerUlti(round, phase, startPlayer, currentPlayer, currentBid, currentBidWinner, trumpSuit,
      contracts, declaredMarriages, cardsOnTheTable, talonOnTheTable,
      ph, tricks.map(_.toClient(role)) ,lastTrick, pointsScored, pickedUpTalon, blockIsLifted, valueInTheTalonForClient)
  }

  lazy val resolveOpenContracts: InnerUlti = {
    val newContracts = contracts.map {
      case (player, contracts) => {
        val updatedContracts: Set[ContractWithStatus] = contracts.map(contractPair => {
          val contract = contractPair.contract
          if (contract.losingCondition(this, player))
            contractPair.copy(contractResult = Failed)
          else if (contract.winningCondition(this, player))
            contractPair.copy(contractResult = Fulfilled)
          else
            contractPair
        })
        (player, updatedContracts)
      }
    }
    this.copy(contracts = newContracts)
  }

  private lazy val transitionToPlayingPhase: InnerUlti = {
    val generatedTrumpSuit = {
      if (currentBid.exists(!_.trumpGame))
        Some(Plain)
      else if (currentBid.exists(_.isInstanceOf[Red]))
        Some(Hearts)
      else
        None
    }
    val temp = this.copy(phase = PlayingPhase, round = 1, trumpSuit = generatedTrumpSuit)
    if (generatedTrumpSuit.isDefined) {
      val bidWinnerContracts = temp.calculateContractsForBidWinner(currentBid.get, generatedTrumpSuit.get)
      val contracts: Map[UltiPlayer, Set[ContractWithStatus]] = Map(
        currentPlayer -> bidWinnerContracts.map(contract => ContractWithStatus(contract)),
        currentPlayer.nextPlayer -> calculateContractForDefender(currentPlayer.nextPlayer, generatedTrumpSuit.get, bidWinnerContracts).map(contract => ContractWithStatus(contract)),
        currentPlayer.nextPlayer.nextPlayer -> calculateContractForDefender(currentPlayer.nextPlayer.nextPlayer, generatedTrumpSuit.get, bidWinnerContracts).map(contract => ContractWithStatus(contract))
      )
      temp.copy(contracts = contracts)
    } else {
      temp
    }
  }

  def reduce(ultiAction: UltiAction, invoker: UltiPlayer): (InnerUlti, UniverseResult) = {
    ultiAction match {
      case SubmitBid(bid, talon) => reduceSubmitBid(bid, talon, invoker)
      case PickUpTalon => reducePickUpTalon(invoker)
      case SkipBid => reduceSkipBid(invoker)
      case DeclareTrumpSuit(suit) => reduceDeclareTrumpSuit(suit, invoker)
      case DeclareMarriages(suits) => reduceDeclareMarriages(suits, invoker)
      case PlayCard(card) => reducePlayCard(card, invoker)
      case DoubleContract(contract) => reduceDoubleContract(contract, invoker)
      case DoneWithDoubling => reduceDoneWithDoubling(invoker)
      case _ => (this, GeneralFailure(s"Action $ultiAction is not implemented."))
    }
  }

  private def reduceSubmitBid(bid: Bid, talon: Set[Card], invoker: UltiPlayer): (InnerUlti, UniverseResult) = {
    if (phase != BiddingPhase) {
      (this, GeneralFailure("Wrong phase."))
    } else if (invoker != currentPlayer) {
      (this, GeneralFailure("Not your turn."))
    } else if (this.playerHands(currentPlayer).size != 12) {
      (this, GeneralFailure("You can only bid if the talon is in your hand."))
    } else if (!talon.subsetOf(playerHands(currentPlayer))) {
      (this, GeneralFailure("You don't have those cards."))
    } else if (this.currentBid.isEmpty || bid > currentBid.get) {
      val temp = this.copy(
        currentBid = Some(bid),
        talon = talon,
        talonOnTheTable = true,
        playerHands = playerHandLen(currentPlayer).modify(_ -- talon)(playerHands),
        currentBidWinner = Some(currentPlayer)
      )
      if (bid == Bid.topBid) {
        (temp.transitionToPlayingPhase, OK)
      } else {
        (temp.copy(
          round = newRoundNumber,
          currentPlayer = currentPlayer.nextPlayer), OK)
      }
    } else {
      (this, GeneralFailure("Your bid must be higher than the current one."))
    }
  }

  private def reducePickUpTalon(invoker: UltiPlayer): (InnerUlti, UniverseResult) = {
    if (phase != BiddingPhase) {
      (this, GeneralFailure("Wrong phase."))
    } else if (!talonOnTheTable) {
      (this, GeneralFailure("There is no talon to pick up."))
    } else if (currentPlayer!=invoker) {
      (this, GeneralFailure("Not your turn."))
    } else {
      (this.copy(talonOnTheTable = false, playerHands = playerHandLen(currentPlayer).modify(_ ++ talon)(playerHands)), OK)
    }
  }

  private def reduceSkipBid(invoker: UltiPlayer): (InnerUlti, UniverseResult) = {
    if (phase != BiddingPhase) {
      (this, GeneralFailure("Wrong phase."))
    } else if (currentPlayer != invoker) {
      (this, GeneralFailure("Not your turn."))
    } else  if (!talonOnTheTable) {
      (this, GeneralFailure("You cannot skip the bid, since the talon is in your hand."))
    } else {
      if (currentBidWinner.contains(currentPlayer)) {
        (this.transitionToPlayingPhase, OK)
      } else {
        (this.copy(currentPlayer = currentPlayer.nextPlayer, round = newRoundNumber), OK)
      }
    }
  }

  private def reduceDeclareTrumpSuit(suit: CardSuit, invoker: UltiPlayer): (InnerUlti, UniverseResult) = {
    if (phase != PlayingPhase || round != 1) {
      (this, GeneralFailure("Declaring the trump suit is only possible in the first round of the playing phase."))
    } else if (!currentBidWinner.contains(currentPlayer) || invoker != currentPlayer) {
      (this, GeneralFailure("Only the bid winner can declare the trump suite."))
    } else if (trumpSuit.isDefined) {
      (this, GeneralFailure("Trump suite has been already declared."))
    } else if (!currentBid.get.trumpGame) {
      (this, GeneralFailure("This is a plain game, trump suite cannot be declared."))
    } else {
      val temp = this.copy(trumpSuit = Some(suit))
      val bidWinnerContracts = temp.calculateContractsForBidWinner(currentBid.get, suit)
      val contracts: Map[UltiPlayer, Set[ContractWithStatus]] = Map(
        currentPlayer -> bidWinnerContracts.map(contract => ContractWithStatus(contract)),
        currentPlayer.nextPlayer -> calculateContractForDefender(currentPlayer.nextPlayer, suit, bidWinnerContracts).map(contract => ContractWithStatus(contract)),
        currentPlayer.nextPlayer.nextPlayer -> calculateContractForDefender(currentPlayer.nextPlayer.nextPlayer, suit, bidWinnerContracts).map(contract => ContractWithStatus(contract))
      )
      (temp.copy(contracts = contracts), OK)
    }
  }

  private def reduceDeclareMarriages(suits: Set[CardSuit], invoker: UltiPlayer): (InnerUlti, UniverseResult) = {
    if (phase != PlayingPhase || round != 1) {
      (this, GeneralFailure("Declaring marriages is only possible in the first round of the playing phase."))
    } else if (invoker != currentPlayer) {
      (this, GeneralFailure("You can only declare marriages in your turn."))
    } else if (declaredMarriages.keys.toList.contains(currentPlayer)) {
      (this, GeneralFailure("Marriages are declared already."))
    } else if (!currentBid.get.canMarriagesBeDeclared) {
      (this, GeneralFailure("Marriages can't be declared for this game."))
    } else {
      if (suits.forall(doesPlayerHaveAMarriageInSuit(invoker, _))) {
        (this.copy(declaredMarriages = declaredMarriageLen(currentPlayer).set(suits)(declaredMarriages)), OK)
      } else {
        (this, GeneralFailure("One or more marriage that you are trying to declare is missing."))
      }
    }
  }

  private def reducePlayCard(card: Card, invoker: UltiPlayer): (InnerUlti, UniverseResult) = {
    if (phase != PlayingPhase) {
      (this, GeneralFailure("Cards can only be played during the playing phase"))
    } else if (invoker != currentPlayer) {
      (this, GeneralFailure("You can only play a card in your turn."))
    } else if (!playerHands(currentPlayer).contains(card)) {
      (this, GeneralFailure("You don't have that card in your hand."))
    } else if (round == 1 && trumpSuit.isEmpty){
      (this, GeneralFailure("Trump suit has to be declared before playing a card."))
    } else {
      implicit val adu: CardSuit = trumpSuit.get
      val validationResult = validMove(card)
      if (validationResult.nonEmpty) {
        (this, GeneralFailure(validationResult.get))
      } else {
        val temp = this.copy(cardsOnTheTable = cardsOnTheTable.appended(card),
          playerHands = playerHandLen(currentPlayer).modify(hand => hand - card)(playerHands),
          blockIsLifted = false)
        cardsOnTheTable.size match {
          case 2 => {
            val roundWinner: UltiPlayer = {
              var tempWinner = currentPlayer.nextPlayer
              var winningCard = temp.cardsOnTheTable.head
              if (temp.cardsOnTheTable(1) > winningCard) {
                tempWinner = tempWinner.nextPlayer
                winningCard = temp.cardsOnTheTable(1)
              }
              if (temp.cardsOnTheTable(2) > winningCard) {
                tempWinner = currentPlayer
                winningCard = temp.cardsOnTheTable(1)
              }
              tempWinner
            }
            val temp2 = temp.copy(tricks = tricks.appended(TakenTrick(roundWinner, Some(temp.cardsOnTheTable))),
              cardsOnTheTable = List.empty, blockIsLifted = false
            ).resolveOpenContracts.copy(round = temp.round + 1)
            if (temp2.contracts.values.forall(_.forall(contractWithStatus => {
              val contractResult = contractWithStatus.contractResult
              contractResult == Failed || contractResult == Fulfilled
            }))) {
              val nr = if (temp2.round > 10) 10 else temp.round
              (temp2.copy(phase = GameOverPhase, round = nr), OK)
            } else if (temp2.round == 11)
              (temp2.copy(phase = GameOverPhase, round = 10), OK)
            else
              (temp2.copy(currentPlayer = roundWinner), OK)
          }
          case _ => {
            (temp.copy(currentPlayer = temp.currentPlayer.nextPlayer), OK)
          }
        }
      }
    }
  }

  private def reduceDoubleContract(contract: Contract, invoker: UltiPlayer): (InnerUlti, UniverseResult) = {
    if (phase != PlayingPhase || round != 1)
      (this, GeneralFailure(InnerUlti.FailureMessages.doublingOutsideTheFirstRound))
    else {
      val cws = contracts(currentBidWinner.get).find(_.contract == contract)
      if (cws.isEmpty) {
        (this, GeneralFailure(InnerUlti.FailureMessages.doublingNonExistentContract))
      } else {
        if (contract.silent) {
          (this, GeneralFailure(InnerUlti.FailureMessages.doublingSilentContract))
        } else {
          val currentLevel = cws.get.contraLevel.levelForPlayer(currentPlayer)
          if (currentLevel >= ContraLevel.maximumContraLevel) {
            (this, GeneralFailure(InnerUlti.FailureMessages.maximumContraLevelAlreadyReached))
          } else if (currentLevel % 2 == 0) { // defenders turn
            if (invoker == currentBidWinner.get) {
              (this, GeneralFailure(InnerUlti.FailureMessages.defendersTurnToDouble))
            } else if (invoker != currentPlayer) {
              (this, GeneralFailure(InnerUlti.FailureMessages.defendersTurnButNotYourTurn))
            } else {
              (this.copy(contracts = contractsOfTheBidWinnerLen.modify(old => {
                val contractToDouble = old.find(_.contract == contract).get
                val doubledContract = contractToDouble.doubleForPlayer(currentPlayer, currentBidWinner.get)
                old - contractToDouble + doubledContract
              })(this.contracts), blockIsLifted = false), OK)
            }
          } else { // soloist turn
            if (invoker != currentBidWinner.get) {
              (this, GeneralFailure(InnerUlti.FailureMessages.soloistTurnToDouble))
            } else {
              if (blockIsLifted) {
                (this, GeneralFailure(InnerUlti.FailureMessages.soloistHasLiftedTheBlockAlready))
              } else {
                (this.copy(contracts = contractsOfTheBidWinnerLen.modify(old => {
                  val contractToDouble = old.find(_.contract == contract).get
                  val doubledContract = contractToDouble.doubleForPlayer(currentPlayer, currentBidWinner.get)
                  old - contractToDouble + doubledContract
                })(this.contracts)), OK)
              }
            }
          }
        }
      }
    }
  }

  private def reduceDoneWithDoubling(invoker: UltiPlayer): (InnerUlti, UniverseResult) = {
    if (!currentBidWinner.contains(invoker)) {
      (this, GeneralFailure())
    } else {
      if (phase == PlayingPhase && round == 1 && currentPlayer != invoker && contractsThatCanBeDoubledByPlayer(currentBidWinner.get).nonEmpty) {
        (this.copy(blockIsLifted = true), OK)
      } else {
        (this, GeneralFailure("not applicable"))
      }
    }
  }
}

object InnerUlti {
  def generate(startPlayer: UltiPlayer, seed: Long): InnerUlti = {
    val shuffledDeck = new Random(seed).shuffle(Card.deck)
    InnerUlti(startPlayer = startPlayer, currentPlayer = startPlayer, playerHands =
      Map[UltiPlayer, Set[Card]](
        startPlayer -> shuffledDeck.slice(0,12).toSet,
        startPlayer.nextPlayer -> shuffledDeck.slice(12,22).toSet,
        startPlayer.nextPlayer.nextPlayer -> shuffledDeck.slice(22,32).toSet
      )
    )
  }

  object FailureMessages {
    val doublingOutsideTheFirstRound: String = "Doubling is only possible in the first round of the Playing Phase."
    val doublingNonExistentContract: String = "The contract that you are trying to double is non-existent."
    val doublingSilentContract: String = "Silent contracts cannot be doubled."
    val defendersTurnToDouble: String = "It is the defenders turn to double this contract."
    val defendersTurnButNotYourTurn = "It is the defenders turn to double this contract, still you may only do that in your turn."
    val soloistTurnToDouble: String = "It is the soloist turn to double this contract."
    val maximumContraLevelAlreadyReached: String = "The maximum contra level is already reached for this contract."
    val soloistHasLiftedTheBlockAlready: String = "You have lifted your block, so you have waived your right to redouble this round."
  }
}

