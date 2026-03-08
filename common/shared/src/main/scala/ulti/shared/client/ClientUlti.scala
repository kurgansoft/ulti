package ulti.shared.client

import gbge.shared.{FrontendGame, GameState}
import ulti.shared.{RoundScore, UltiAction}
import ulti.shared.abstract0.*
import zio.json.ast.Json
import zio.json.JsonCodec
import zio.schema.{DeriveSchema, Schema}

case class ClientUlti(
                       override val state: GameState,
                       override val innerUlti: Option[ClientInnerUlti],
                       override val scores: List[RoundScore] = List.empty,
                       override val readyForNextRound: Map[UltiPlayer, Boolean] = Map(UltiPlayer1 -> false, UltiPlayer2 -> false, UltiPlayer3 -> false)
                     ) extends AbstractUlti(state, innerUlti) with FrontendGame[UltiAction] {
  override lazy val encode: Json = ClientUlti.codec.encoder.toJsonAST(this).getOrElse(???)

}

object ClientUlti {
  implicit val schema: Schema[ClientUlti] = DeriveSchema.gen[ClientUlti]
  implicit val codec: JsonCodec[ClientUlti] =
    zio.schema.codec.JsonCodec.jsonCodec(schema)
}