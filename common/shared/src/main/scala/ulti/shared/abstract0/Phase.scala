package ulti.shared.abstract0

import zio.json.JsonCodec
import zio.schema.{DeriveSchema, Schema}

sealed trait Phase

object Phase {
  implicit val schema: Schema[Phase] = DeriveSchema.gen[Phase]
  implicit val codec: JsonCodec[Phase] =
    zio.schema.codec.JsonCodec.jsonCodec(schema)
}

case object BiddingPhase extends Phase
case object PlayingPhase extends Phase
case object GameOverPhase extends Phase