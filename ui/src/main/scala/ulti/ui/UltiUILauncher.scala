package ulti.ui

import gbge.shared.FrontendGame
import gbge.shared.actions.GameAction
import gbge.ui.EntryPoint
import ulti.shared.client.ClientUlti
import zio.json.JsonCodec

import scala.scalajs.js.annotation.JSExportTopLevel

@JSExportTopLevel("ep")
object UltiUILauncher extends EntryPoint {
  gbge.shared.RG.gameCodecs = List(ClientUlti.codec.asInstanceOf[JsonCodec[FrontendGame[_ <: GameAction]]])
  gbge.ui.RG.registeredGames = List(UltiGameProps)
}
