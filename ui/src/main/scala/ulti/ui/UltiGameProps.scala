package ulti.ui

import gbge.shared.FrontendUniverse
import gbge.ui.eps.player.ClientState
import gbge.ui.ClientGameProps
import org.scalajs.dom.html.Div
import gbge.ui.eps.spectator.SpectatorState
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.vdom.TagOf
import japgolly.scalajs.react.vdom.all.*
import uiglue.EventLoop.EventHandler
import uiglue.{Event, EventLoop}
import ulti.shared.{UltiAction, UltiProps}
import ulti.shared.client.ClientUlti
import zio.{UIO, ZIO}

import scala.language.implicitConversions

object UltiGameProps extends UltiProps with ClientGameProps[UltiAction, ClientUlti] {

  override val playerDisplayer: (ClientState, EventLoop.EventHandler[Event]) => TagOf[Div] = (state, commander) => {
    org.scalajs.dom.window.onresize = _ => {
      val w = org.scalajs.dom.window.innerWidth.toInt
      val h = org.scalajs.dom.window.innerHeight.toInt
      commander(NewDimensions(w,h))
    }
    Screens.playerScreen(state, commander)
  }

  override val spectatorDisplayer: (SpectatorState, EventLoop.EventHandler[Event]) => TagOf[Div] = (state, commander) => {
    org.scalajs.dom.window.onresize = _ => {
      val w = org.scalajs.dom.window.innerWidth.toInt
      val h = org.scalajs.dom.window.innerHeight.toInt
      commander(NewDimensions(w,h))
    }
    Screens.spectatorScreen(state, commander)
  }

  override val handleNewFUForSpectator: (SpectatorState, FrontendUniverse) => (SpectatorState, EventLoop.EventHandler[Event] => UIO[List[Nothing]]) =
    (state, fu) => {
      val tempState = {
        if (!state.offlineState.isInstanceOf[OfflineUltiSpectatorState]) {
          val w = org.scalajs.dom.window.innerWidth.toInt
          val h = org.scalajs.dom.window.innerHeight.toInt
          state.copy(offlineState = OfflineUltiSpectatorState(w, h), frontendUniverse = Some(fu))
        } else
          state.copy(frontendUniverse = Some(fu))
      }
      (tempState, _ => ZIO.succeed(List.empty))
    }

  override val handleNewFU: (ClientState, FrontendUniverse)  => (ClientState, EventLoop.EventHandler[Event] => UIO[List[Event]]) = (clientState, newFU) => {
    implicit def convert(clientState: ClientState): (ClientState, EventLoop.EventHandler[Event] => UIO[List[Event]]) =
      (clientState, _ => ZIO.succeed(List.empty))

    val step1 = clientState.copy(frontendUniverse = Some(newFU))

    val step2 = {
      if (!step1.offlineState.isInstanceOf[OfflineUltiState]) {
        val w = org.scalajs.dom.window.innerWidth.toInt
        val h = org.scalajs.dom.window.innerHeight.toInt
        step1.copy(
          offlineState = OfflineUltiState(dimensions = (w, h))
        )
      } else
        step1
    }

    val ous = step2.offlineState.asInstanceOf[OfflineUltiState]
    val newClientUlti: ClientUlti = newFU.game.get.asInstanceOf[ClientUlti]

    val result = if (newClientUlti.innerUlti.isDefined) {
      val newInnerUlti = newClientUlti.innerUlti.get
      if (!step2.player.flatMap(_.role).contains(newInnerUlti.currentPlayer.roleId)) {
        step2.copy(
          offlineState = OfflineUltiState(
            dimensions = ous.dimensions,
            submitCardsWithSingleClick = ous.submitCardsWithSingleClick,
          ),
        )
      } else if (newInnerUlti.pickedUpTalon.nonEmpty && ous.selectedCards.isEmpty) {
        step2.copy(offlineState = ous.copy(selectedCards = newInnerUlti.pickedUpTalon, submitCardsWithSingleClick = ous.submitCardsWithSingleClick))
      } else {
        step2
      }
    } else {
      step2
    }

    result
  }

  override val adminDisplayer: (ClientState, EventLoop.EventHandler[Event]) => TagOf[Div] = (_, _) => {
    div()
  }

  override val metaExtension: (ClientState, EventHandler[Event]) => TagOf[Div] = (clientState, eventHandler) => {
    val offlineUltiState = clientState.offlineState.asInstanceOf[OfflineUltiState]
    div(
      div(
        div(border:= "solid 1px white",
          input(`type`:= "checkbox", checked:= offlineUltiState.submitCardsWithSingleClick, readOnly:= true),
          span("Submit cards with a single click."),
          onClick --> Callback {
            eventHandler(ToggleSingleClickCardPlay)
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
