package ulti.shared.abstract0

import gbge.shared.GameRole
import zio.json.JsonCodec
import zio.schema.Schema

sealed trait UltiPlayer extends GameRole {
  val nextPlayer: UltiPlayer
}

object UltiPlayer {
  implicit val schema: Schema[UltiPlayer] = Schema[Int].transformOrFail(
    roleId => getUltiPlayerFromRole(roleId),
    ultiPlayer => Right(ultiPlayer.roleId)
  )
  implicit val codec: JsonCodec[UltiPlayer] =
    zio.schema.codec.JsonCodec.jsonCodec(schema)

  def getUltiPlayerFromRole(role: Int): Either[String, UltiPlayer] =
    role match {
      case 1 => Right(UltiPlayer1)
      case 2 => Right(UltiPlayer2)
      case 3 => Right(UltiPlayer3)
      case other => Left(s"roleId [$other] is invalid")
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