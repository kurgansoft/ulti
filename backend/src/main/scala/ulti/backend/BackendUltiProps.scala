package ulti.backend

import gbge.backend.{BackendGame, BackendGameProps}
import gbge.shared.actions.Action
import ulti.shared.{UltiAction, UltiProps}
import ulti.shared.client.ClientUlti
import zio.http.codec.ContentCodec
import zio.http.codec.HttpCodec.content

object BackendUltiProps extends UltiProps with BackendGameProps[UltiAction, ClientUlti] {

  override def start(noOfPlayers: Int): (BackendGame[UltiAction, ClientUlti], Option[Action]) = {
    (Ulti(), None)
  }

  override val contentCodec: ContentCodec[UltiAction] = content[UltiAction]
}
