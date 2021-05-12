package ulti.ui

import gbge.client.ClientResult
import gbge.shared.{FrontendPlayer, FrontendUniverse}
import gbge.ui.eps.player.ScreenEvent
import gbge.ui.state.OfflineState

case class NewDimensions(width: Int, height: Int) extends ScreenEvent

case class OfflineUltiSpectatorState(
                                     width: Int,
                                     height: Int
                                     ) extends OfflineState {
  override def handleScreenEvent(sa: ScreenEvent, fu: Option[FrontendUniverse], you: Option[FrontendPlayer]): (OfflineState, ClientResult) = {
    sa match {
      case NewDimensions(width, height) => {
        OfflineUltiSpectatorState(width, height)
      }
      case _ => this
    }
  }
}
