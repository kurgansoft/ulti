package ulti.ui

import gbge.client.events_and_effects.{PlayerEvent, ScreenEvent}
import gbge.shared.FrontendUniverse
import gbge.ui.state.OfflineState
import zio.{UIO, ZIO}

case class NewDimensions(width: Int, height: Int) extends ScreenEvent

case class OfflineUltiSpectatorState(
                                     width: Int,
                                     height: Int
                                     ) extends OfflineState[Any] {
  override def handleScreenEvent(sa: ScreenEvent, fu: Option[FrontendUniverse], playerId: Option[Int]): (OfflineUltiSpectatorState, UIO[List[PlayerEvent]]) = {
    sa match {
      case NewDimensions(width, height) =>
        (OfflineUltiSpectatorState(width, height), ZIO.succeed(List.empty[PlayerEvent]))
      case _ => (this, ZIO.succeed(List.empty[PlayerEvent]))
    }
  }
}
