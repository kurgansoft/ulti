package ulti.ui

import gbge.client.{DispatchActionWithToken, GeneralEvent}
import gbge.shared.FrontendUniverse
import gbge.ui.eps.player.ScreenEvent
import gbge.ui.state.OfflineState
import ulti.shared.*
import ulti.shared.abstract0.{BiddingPhase, PlayingPhase, UltiPlayer}
import ulti.shared.client.ClientUlti
import ulti.shared.contracts.Contract
import ulti.ui.offline_stuff.{BidProposition, Proposition}
import zio.{UIO, ZIO}

import scala.language.implicitConversions

sealed trait OfflineUltiEvent extends ScreenEvent

case class CardClicked(card: Card) extends OfflineUltiEvent
case class BidPropositionEvent(proposition: Proposition) extends OfflineUltiEvent
case class DoubleContractEvent(contract: Contract) extends OfflineUltiEvent
case object SubmitBidEvent extends OfflineUltiEvent
case object PickUpTalonEvent extends OfflineUltiEvent
case object SkipBidEvent extends OfflineUltiEvent
case class DeclareTrumpSuitEvent(suit: CardSuit) extends OfflineUltiEvent
case class AddOrRemoveMarriageEvent(suit: CardSuit) extends OfflineUltiEvent
case object DeclareMarriagesEvent extends OfflineUltiEvent
case object DoneWithDoublingEvent extends OfflineUltiEvent
case object ReadyEvent extends OfflineUltiEvent
case class SwitchToSubScreen(screen: Option[Subscreen]) extends OfflineUltiEvent
case object PlaySelectedCard extends OfflineUltiEvent
case object ToggleSingleClickCardPlay extends OfflineUltiEvent

case object ClientInit extends OfflineUltiEvent

sealed trait Subscreen

case object LeftPlayerPerspectiveScreen extends Subscreen
case object RightPlayerPerspectiveScreen extends Subscreen
case object BidSelectionScreen extends Subscreen
case object ContractInfoScreen extends Subscreen
case object DoublingScreen extends Subscreen

case class OfflineUltiState(
                             selectedCards: Set[Card] = Set.empty,
                             bidProposition: BidProposition = BidProposition(),
                             marriagesAreDeclared: Boolean = false,
                             proposedMarriages: Set[CardSuit] = Set.empty,
                             currentSubScreen: Option[Subscreen] = None,
                             submitCardsWithSingleClick: Boolean = false,
                             offlineErrorMessage: Option[String] = None,
                             dimensions: (Int, Int) = (0,0),
                           ) extends OfflineState[Any] {
  
  implicit def convert(offlineUltiState: OfflineUltiState): (OfflineUltiState, UIO[List[GeneralEvent]]) = (offlineUltiState, ZIO.succeed(List.empty[GeneralEvent]))

  override def handleScreenEvent(sa: ScreenEvent, fu: Option[FrontendUniverse], playerId: Option[Int]): (OfflineUltiState, UIO[List[GeneralEvent]]) = {
    sa match {
      case ue: OfflineUltiEvent => reduce(ue, fu, playerId)
      case NewDimensions(w, h) => this.copy(dimensions = (w, h))
      case _ => this
    }
  }

  lazy val separateBidScreen: Boolean = dimensions._2 < 1030

  lazy val smallCards: Boolean = dimensions._2 < 920

  private def reduce(offlineUltiEvent: OfflineUltiEvent, fu: Option[FrontendUniverse], playerId: Option[Int]): (OfflineUltiState, UIO[List[GeneralEvent]]) = {
    val clientUlti = fu.get.game.get.asInstanceOf[ClientUlti]
    val yourRole = playerId.flatMap(clientUlti.getRoleById)
    val errorMessageCleared = this.copy(offlineErrorMessage = None)
    offlineUltiEvent match {
      case CardClicked(card) =>
        val innerUlti = clientUlti.innerUlti.get
        if (!yourRole.contains(innerUlti.currentPlayer)) {
          errorMessageCleared
        } else {
          innerUlti.phase match {
            case BiddingPhase =>
              if (!innerUlti.talonOnTheTable) {
                if (selectedCards.contains(card))
                  errorMessageCleared.copy(selectedCards = selectedCards - card)
                else if(selectedCards.size <= 1)
                  errorMessageCleared.copy(selectedCards = selectedCards + card)
                else
                  errorMessageCleared
              } else
                errorMessageCleared
            case PlayingPhase =>
              if (submitCardsWithSingleClick) {
                (errorMessageCleared, ZIO.succeed(List(DispatchActionWithToken(PlayCard(card)))))
              } else {
                val validationError = clientUlti.innerUlti.get.validMove(card)
                if (validationError.isEmpty) {
                  if (selectedCards.contains(card))
                    errorMessageCleared.copy(selectedCards = Set.empty)
                  else
                    errorMessageCleared.copy(selectedCards = Set(card))
                } else {
                  errorMessageCleared.copy(offlineErrorMessage = validationError)
                }
              }
            case _ => errorMessageCleared
          }
        }
      case BidPropositionEvent(proposition) =>
        errorMessageCleared.copy(bidProposition = bidProposition.reduce(proposition))
      case SubmitBidEvent =>
        val submitBidAction = SubmitBid(bidProposition.generatedBid.get, selectedCards)
        (errorMessageCleared, ZIO.succeed(List(DispatchActionWithToken(submitBidAction))))
      case PickUpTalonEvent =>
        (errorMessageCleared, ZIO.succeed(List(DispatchActionWithToken(PickUpTalon))))
      case SkipBidEvent =>
        (errorMessageCleared, ZIO.succeed(List(DispatchActionWithToken(SkipBid))))
      case DeclareTrumpSuitEvent(suit) =>
        (errorMessageCleared, ZIO.succeed(List(DispatchActionWithToken(DeclareTrumpSuit(suit)))))
      case AddOrRemoveMarriageEvent(suit) =>
        if (proposedMarriages.contains(suit))
          errorMessageCleared.copy(proposedMarriages = proposedMarriages - suit)
        else
          errorMessageCleared.copy(proposedMarriages = proposedMarriages + suit)
      case DeclareMarriagesEvent =>
        (errorMessageCleared.copy(marriagesAreDeclared = true), ZIO.succeed(List(DispatchActionWithToken(DeclareMarriages(proposedMarriages)))))
      case ClientInit =>
        (errorMessageCleared, ZIO.succeed(List(DispatchActionWithToken(Init))))
      case DoubleContractEvent(contract) =>
        (errorMessageCleared, ZIO.succeed(List(DispatchActionWithToken(DoubleContract(contract)))))
      case DoneWithDoublingEvent =>
        (errorMessageCleared, ZIO.succeed(List(DispatchActionWithToken(DoneWithDoubling))))
      case ReadyEvent =>
        (errorMessageCleared, ZIO.succeed(List(DispatchActionWithToken(ReadyForNextRound))))
      case SwitchToSubScreen(subscreen) =>
        errorMessageCleared.copy(currentSubScreen = subscreen)
      case PlaySelectedCard =>
        if (clientUlti.innerUlti.isDefined) {
          val innerUlti = clientUlti.innerUlti.get
          if (innerUlti.phase == PlayingPhase && errorMessageCleared.selectedCards.size == 1) {
            (errorMessageCleared.copy(selectedCards = Set.empty), ZIO.succeed(List(DispatchActionWithToken(PlayCard(selectedCards.toList.head)))))
          } else {
            errorMessageCleared
          }
        } else {
          errorMessageCleared
        }
      case ToggleSingleClickCardPlay =>
        val temp = errorMessageCleared.copy(submitCardsWithSingleClick= !submitCardsWithSingleClick)
        if (clientUlti.innerUlti.exists(_.phase == PlayingPhase)) {
          temp.copy(selectedCards = Set.empty)
        } else {
          temp
        }
      case _ => errorMessageCleared
    }
  }
}
