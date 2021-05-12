package ulti.shared.client

import gbge.shared.{DecodeCapable, FrontendGame, GameState}
import ulti.shared.{RoundScore, UltiAction}
import ulti.shared.abstract0._
import upickle.default.{macroRW, ReadWriter => RW}

case class ClientUlti(
                       override val state: GameState,
                       override val innerUlti: Option[ClientInnerUlti],
                       override val scores: List[RoundScore] = List.empty,
                       override val readyForNextRound: Map[UltiPlayer, Boolean] = Map(UltiPlayer1 -> false, UltiPlayer2 -> false, UltiPlayer3 -> false)
                     ) extends AbstractUlti(state, innerUlti) with FrontendGame[UltiAction] {
  override def serialize(): String = upickle.default.write(this)

  override def decodeAction(payload: String): UltiAction = {
    upickle.default.read[UltiAction](payload)
  }
}

object ClientUlti extends DecodeCapable {
  implicit def rw: RW[ClientUlti] = macroRW

  override def decode(encodedForm: String): ClientUlti = {
    upickle.default.read[ClientUlti](encodedForm)
  }

  override val name: String = "Ulti"
}