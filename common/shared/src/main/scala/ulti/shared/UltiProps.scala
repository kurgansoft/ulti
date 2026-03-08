package ulti.shared

import gbge.shared.GameProps
import ulti.shared.client.ClientUlti
import zio.json.JsonCodec

trait UltiProps extends GameProps[UltiAction, ClientUlti] {
  override val name: String = "Ulti"
  override val urlFragment: String = "ulti"
  override implicit val actionCodec: JsonCodec[UltiAction] = UltiAction.codec
  override implicit val gameCodec: JsonCodec[ClientUlti] = ClientUlti.codec
}
