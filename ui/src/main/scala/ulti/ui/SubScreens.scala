package ulti.ui

import gbge.shared.FrontendPlayer
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.vdom.TagOf
import japgolly.scalajs.react.vdom.all._
import org.scalajs.dom.html.Div
import uiglue.{Event, EventLoop}
import ulti.shared.abstract0.UltiPlayer
import ulti.shared.client.ClientInnerUlti

object SubScreens {
  def contractsInfoSubScreen(innerUlti: ClientInnerUlti, you: UltiPlayer, commander: EventLoop.EventHandler[SwitchToSubScreen])(implicit players: List[FrontendPlayer]): TagOf[Div] = {
    div(color:="yellow",
      div(innerUlti.bidStatusText()(players), fontSize:="xx-large", textAlign:="center", marginTop:="25px"), br,
      Directives.contractsDisplayer(innerUlti.contractsOfTheBidWinner)(players),
      button("BACK", `class`:="btn btn-primary", onClick --> Callback {
        commander(SwitchToSubScreen(None))
      }, position:="relative", left:="50%", transform:="translate(-50%)")
    )
  }

  def foreignPlayerPerspective(innerUlti: ClientInnerUlti, player: UltiPlayer, commander: EventLoop.EventHandler[Event], smallCards: Boolean = false)(implicit players: List[FrontendPlayer]): TagOf[Div] = {
    val playersName = FrontendPlayer.getNameOfPlayerWithRole(player.roleId)
    val cards = innerUlti.getSortedPlayerHand(player).getOrElse(List.empty)

    val cardHandCenterPoint: (Int, Int) = if (smallCards) {
      (-(178 / 4), -50)
    } else {
      (-(178 / 2), -50)
    }
    val customRadius = if (smallCards) 150 else 300

    div(color:="yellow",
      h1(s"Perspective of $playersName"),
      button(`class`:="btn btn-primary", "BACK", onClick --> Callback {
        commander(SwitchToSubScreen(None))
      }),
      div(
        Directives.cardHand(cards.map(c => (c, false)), radius = customRadius, centerPoint = cardHandCenterPoint, eventHandler = null),
        position:="absolute", left:="50%", bottom:="0px", transform:="translate(-50%)"
      )
    )
  }
}
