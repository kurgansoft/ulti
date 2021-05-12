package ulti.ui

import gbge.shared.FrontendUniverse
import gbge.ui.eps.player.ClientState
import gbge.ui.UIExport
import org.scalajs.dom.html.Div
import gbge.client._
import gbge.ui.eps.spectator.SpectatorState
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.vdom.TagOf
import japgolly.scalajs.react.vdom.all._
import ulti.shared.client.ClientUlti

object UltiUIExport extends UIExport {

  implicit def to3(clientState: ClientState): (ClientState, ClientResult) = (clientState, OK)

  override val playerDisplayer: (ClientState, ClientEventHandler[ClientEvent]) => TagOf[Div] = (state, commander) => {
    org.scalajs.dom.window.onresize = _ => {
      val w = org.scalajs.dom.window.innerWidth.toInt
      val h = org.scalajs.dom.window.innerHeight.toInt
      commander.addAnEventToTheEventQueue(NewDimensions(w,h))
    }
    Screens.playerScreen(state, commander)
  }

  override val spectatorDisplayer: (SpectatorState, ClientEventHandler[ClientEvent]) => TagOf[Div] = (state, commander) => {
    org.scalajs.dom.window.onresize = _ => {
      val w = org.scalajs.dom.window.innerWidth.toInt
      val h = org.scalajs.dom.window.innerHeight.toInt
      commander.addAnEventToTheEventQueue(NewDimensions(w,h))
    }
    Screens.spectatorScreen(state, commander)
  }

  override val handleNewFUForSpectator: (SpectatorState, FrontendUniverse) => (SpectatorState, ClientResult) = (state, fu) => {
    val tempState = {
      if (!state.offlineState.isInstanceOf[OfflineUltiSpectatorState]) {
        val w = org.scalajs.dom.window.innerWidth.toInt
        val h = org.scalajs.dom.window.innerHeight.toInt
        state.copy(offlineState = OfflineUltiSpectatorState(w, h), frontendUniverse = Some(fu))
      } else
        state.copy(frontendUniverse = Some(fu))
    }
    (tempState, OK)
  }

  override val handleNewFU: (ClientState, FrontendUniverse)  => (ClientState, ClientResult) = (clientState, newFU) => {
    val tempClientState = {
      if (!clientState.offlineState.isInstanceOf[OfflineUltiState]) {
        val w = org.scalajs.dom.window.innerWidth.toInt
        val h = org.scalajs.dom.window.innerHeight.toInt
        clientState.copy(offlineState = OfflineUltiState(dimensions = (w, h)))
      } else
        clientState
    }
    val ous = tempClientState.offlineState.asInstanceOf[OfflineUltiState]
    val newClientUlti: ClientUlti = newFU.game.get.asInstanceOf[ClientUlti]

    if (newClientUlti.innerUlti.isDefined) {
      val newInnerUlti = newClientUlti.innerUlti.get
      if (!clientState.you.flatMap(_.role).contains(newInnerUlti.currentPlayer.roleId)) {
        tempClientState.copy(offlineState = OfflineUltiState(submitCardsWithSingleClick = ous.submitCardsWithSingleClick, dimensions = ous.dimensions), frontendUniverse = Some(newFU))
      } else if (newInnerUlti.pickedUpTalon.nonEmpty && ous.selectedCards.isEmpty) {
        tempClientState.copy(offlineState = ous.copy(selectedCards = newInnerUlti.pickedUpTalon, submitCardsWithSingleClick = ous.submitCardsWithSingleClick), frontendUniverse = Some(newFU))
      } else {
        tempClientState.copy(frontendUniverse = Some(newFU))
      }
    } else {
      tempClientState.copy(frontendUniverse = Some(newFU))
    }
  }
  override val adminDisplayer: (ClientState, ClientEventHandler[ClientEvent]) => TagOf[Div] = (clientState, commander) => {
    div()
  }

  override val metaExtension = (clientState, commander) => {
    val offlineUltiState = clientState.offlineState.asInstanceOf[OfflineUltiState]
    div(
      div(
        div(border:= "solid 1px white",
          input(`type`:= "checkbox", checked:= offlineUltiState.submitCardsWithSingleClick, readOnly:= true),
          span("Submit cards with a single click."),
          onClick --> Callback {
            commander.addAnEventToTheEventQueue(ToggleSingleClickCardPlay)
          }
        )
      ),br,br,
      h1(textAlign:="center", "CREDITS"),
      div("Lightbulb icon made by ",
        a(href:="https://www.flaticon.com/authors/good-ware", title:="Good Ware", "Good Ware"), " from ",
        a(href:="https://www.flaticon.com/", title:="Flaticon", "www.flaticon.com"), "."
      ),br,
      div("Card cover picture is taken from ",
        a(href:="https://opengameart.org/content/colorful-poker-card-back", "OpenGameArt.org"),
        "."
      )
    )
  }

}
