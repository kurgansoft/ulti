package launchers

import gbge.backend.server.CustomServer
import ulti.backend.Ulti
import ulti.shared.client.ClientUlti

object CustomLauncher extends CustomServer {
  gbge.shared.RG.registeredGames = List(ClientUlti)
  gbge.backend.RG.registeredGames = List(Ulti)

  assert(gbge.backend.RG.registeredGames.size == gbge.shared.RG.registeredGames.size)
  gbge.backend.RG.registeredGames.zip(gbge.shared.RG.registeredGames).foreach(a => {
    assert(a._1.frontendGame == a._2)
  })

  override val jsLocation: Option[String] = Some("out/Ulti/ui/fastOpt/dest")
}