package base

import gbge.ui.EntryPoint
import ulti.ui.UltiUIExport
import ulti.shared.client.ClientUlti

import scala.scalajs.js.annotation.JSExportTopLevel

@JSExportTopLevel("ep")
object UltiUILauncher extends EntryPoint {
  gbge.shared.RG.registeredGames = List(ClientUlti)
  gbge.ui.RG.registeredGames = List(UltiUIExport)
}