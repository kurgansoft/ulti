package ulti.ui

import gbge.client.{ClientResult, OK, PrepareRestActionWithToken}
import gbge.shared.{FrontendPlayer, FrontendUniverse}
import gbge.ui.eps.player.ScreenEvent
import gbge.ui.state.OfflineState
import ulti.shared.abstract0.{BiddingPhase, PlayingPhase}
import ulti.shared._
import ulti.shared.client.ClientUlti
import ulti.shared.contracts.Contract
import ulti.ui.offline_stuff.{BidProposition, Proposition}

abstract sealed class OfflineUltiEvent extends ScreenEvent

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

abstract sealed class Subscreen

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
                             dimensions: (Int, Int) = (0,0)
                           ) extends OfflineState {
  override def handleScreenEvent(sa: ScreenEvent, fu: Option[FrontendUniverse], you: Option[FrontendPlayer]): (OfflineState, ClientResult) = {
    sa match {
      case ue: OfflineUltiEvent => reduce0(ue, fu, you)
      case NewDimensions(w, h) => this.copy(dimensions = (w,h))
      case _ => this
    }
  }

  lazy val separateBidScreen: Boolean = dimensions._2 < 1030

  lazy val smallCards: Boolean = dimensions._2 < 920

  def reduce0(offlineUltiEvent: OfflineUltiEvent, fu: Option[FrontendUniverse], you: Option[FrontendPlayer]): (OfflineState, ClientResult) = {
    val clientUlti = fu.get.game.get.asInstanceOf[ClientUlti]
    val errorMessageCleared = this.copy(offlineErrorMessage = None)
    offlineUltiEvent match {
      case CardClicked(card) => {
        val innerUlti = clientUlti.innerUlti.get
        if (!you.get.role.flatMap(innerUlti.getUltiPlayerFromRoleId).contains(innerUlti.currentPlayer)) {
          errorMessageCleared
        } else {
          innerUlti.phase match {
            case BiddingPhase => {
              if (!innerUlti.talonOnTheTable) {
                if (selectedCards.contains(card))
                  errorMessageCleared.copy(selectedCards = selectedCards - card)
                else if(selectedCards.size <= 1)
                  errorMessageCleared.copy(selectedCards = selectedCards + card)
                else
                  errorMessageCleared
              } else
                errorMessageCleared
            }
            case PlayingPhase => {
              if (submitCardsWithSingleClick) {
                (errorMessageCleared, PrepareRestActionWithToken(PlayCard(card)))
              } else {
                val validationError = clientUlti.innerUlti.get.validMove(card)
                if (validationError.isEmpty) {
                  if (selectedCards.contains(card))
                    errorMessageCleared.copy(selectedCards = Set.empty)
                  else
                    errorMessageCleared.copy(selectedCards = Set(card))
                } else {
                  println(validationError.get)
                  errorMessageCleared.copy(offlineErrorMessage = validationError)
                }
              }
            }
            case _ => errorMessageCleared
          }
        }
      }
      case BidPropositionEvent(proposition) => {
        errorMessageCleared.copy(bidProposition = bidProposition.reduce(proposition))
      }
      case SubmitBidEvent => {
        (errorMessageCleared, PrepareRestActionWithToken(SubmitBid(bidProposition.generatedBid.get, selectedCards)))
      }
      case PickUpTalonEvent => {
        (errorMessageCleared, PrepareRestActionWithToken(PickUpTalon))
      }
      case SkipBidEvent => {
        (errorMessageCleared, PrepareRestActionWithToken(SkipBid))
      }
      case DeclareTrumpSuitEvent(suit) => {
        (errorMessageCleared, PrepareRestActionWithToken(DeclareTrumpSuit(suit)))
      }
      case AddOrRemoveMarriageEvent(suit) => {
        if (proposedMarriages.contains(suit))
          errorMessageCleared.copy(proposedMarriages = proposedMarriages - suit)
        else
          errorMessageCleared.copy(proposedMarriages = proposedMarriages + suit)
      }
      case DeclareMarriagesEvent => {
        (errorMessageCleared.copy(marriagesAreDeclared = true), PrepareRestActionWithToken(DeclareMarriages(proposedMarriages)))
      }
      case ClientInit => {
        (errorMessageCleared, PrepareRestActionWithToken(Init))
      }
      case DoubleContractEvent(contract) => {
        (errorMessageCleared, PrepareRestActionWithToken(DoubleContract(contract)))
      }
      case DoneWithDoublingEvent => {
        (errorMessageCleared, PrepareRestActionWithToken(DoneWithDoubling))
      }
      case ReadyEvent => {
        (errorMessageCleared, PrepareRestActionWithToken(ReadyForNextRound))
      }
      case SwitchToSubScreen(subscreen) => {
        (errorMessageCleared.copy(currentSubScreen = subscreen), OK)
      }
      case PlaySelectedCard => {
        if (clientUlti.innerUlti.isDefined) {
          val innerUlti = clientUlti.innerUlti.get
          if (innerUlti.phase == PlayingPhase && errorMessageCleared.selectedCards.size == 1) {
            (errorMessageCleared.copy(selectedCards = Set.empty), PrepareRestActionWithToken(PlayCard(selectedCards.toList.head)))
          } else {
            errorMessageCleared
          }
        } else {
          errorMessageCleared
        }
      }
      case ToggleSingleClickCardPlay => {
        val temp = errorMessageCleared.copy(submitCardsWithSingleClick= !submitCardsWithSingleClick)
        if (clientUlti.innerUlti.exists(_.phase == PlayingPhase)) {
          temp.copy(selectedCards = Set.empty)
        } else {
          temp
        }
      }
      case _ => errorMessageCleared
    }
  }
}
