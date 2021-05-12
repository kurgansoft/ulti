package ulti.shared.abstract0

import gbge.shared.GameRole
import upickle.default.{macroRW, ReadWriter => RW}

abstract sealed class UltiPlayer extends GameRole {
  val nextPlayer: UltiPlayer
}

object UltiPlayer {
  implicit def rw: RW[UltiPlayer] = macroRW

  def getUltiPlayerFromRole(role: Int): Option[UltiPlayer] =
    role match {
      case 1 => Some(UltiPlayer1)
      case 2 => Some(UltiPlayer2)
      case 3 => Some(UltiPlayer3)
      case _ => None
    }

  val allThePlayers: List[UltiPlayer] = List(UltiPlayer1, UltiPlayer2, UltiPlayer3)
}

case object UltiPlayer1 extends UltiPlayer {
  override val nextPlayer: UltiPlayer = UltiPlayer2
  override val roleId: Int = 1
}

case object UltiPlayer2 extends UltiPlayer {
  override val nextPlayer: UltiPlayer = UltiPlayer3
  override val roleId: Int = 2
}

case object UltiPlayer3 extends UltiPlayer {
  override val nextPlayer: UltiPlayer = UltiPlayer1
  override val roleId: Int = 3
}