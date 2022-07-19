/*
 * Copyright 2019 ACINQ SAS
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package fr.acinq.eclair.wire

import fr.acinq.bitcoin.ByteVector32
import fr.acinq.bitcoin.Crypto.PublicKey
import fr.acinq.eclair.payment.Bolt11Invoice
import fr.acinq.eclair.wire.CommonCodecs._
import fr.acinq.eclair.wire.OnionRoutingCodecs.MissingRequiredTlv
import fr.acinq.eclair.wire.TlvCodecs._
import fr.acinq.eclair.{CltvExpiry, MilliSatoshi, UInt64}
import scodec.bits.{BitVector, ByteVector}

/** Created by t-bast on 05/07/2019.
  */

/*
We support multiple payment flows, each having different requirements for what the onions contain. The following is an
overview of the onion contents we support.

STANDARD PAYMENT (fully source-routed, single-part):

    a -------------> b --------------------------> c --------------------------> d ---------------------------> e
          +-----------------------+     +-----------------------+     +-----------------------+     +-----------------------+
          | amount_fwd: 1025 msat |     | amount_fwd: 1010 msat |     | amount_fwd: 1000 msat |     | amount_fwd: 1000 msat |
          | expiry: 600030        |     | expiry: 600012        |     | expiry: 600000        |     | expiry: 600000        |
          | channel_id: 1105      |     | channel_id: 561       |     | channel_id: 42        |     | secret: xyz (opt)     |
          |-----------------------|     |-----------------------|     |-----------------------|     +-----------------------+
          |     (encrypted)       |     |     (encrypted)       |     |     (encrypted)       |     |          EOF          |
          +-----------------------+     +-----------------------+     +-----------------------+     +-----------------------+

STANDARD MULTI-PART PAYMENT (fully source-routed, multi-part):

    a -------------> b --------------------------> c --------------------------> d ---------------------------> e
          +-----------------------+     +-----------------------+     +-----------------------+     +-------------------------+
          | amount_fwd: 1025 msat |     | amount_fwd: 1010 msat |     | amount_fwd: 1000 msat |     | amount_fwd: 1000 msat   |
          | expiry: 600030        |     | expiry: 600012        |     | expiry: 600000        |     | expiry: 600000          |
          | channel_id: 1105      |     | channel_id: 561       |     | channel_id: 42        |     | secret: xyz             |
          |-----------------------|     |-----------------------|     |-----------------------|     | total_amount: 1500 msat |
          |     (encrypted)       |     |     (encrypted)       |     |     (encrypted)       |     +-------------------------+
          +-----------------------+     +-----------------------+     +-----------------------+     |           EOF           |
                                                                                                    +-------------------------+

TRAMPOLINE PAYMENT (partially source-routed, multi-part):

    a -------------> b ---------------------------> t1 -----------------------------> t2 -------------------------------> e
          +----------------------+     +---------------------------+     +---------------------------+     +-----------------------------+
          | amount_fwd: 900 msat |     | amount_fwd: 900 msat      |     | amount_fwd: 750 msat      |     | amount_fwd: 1000 msat       |
          | expiry: 600112       |     | expiry: 600112            |     | expiry: 600042            |     | expiry: 600000              |
          | channel_id: 42       |     | secret: aaaaa             |     | secret: zzzzz             |     | secret: xxxxx               | <- randomly generated by t2 (NOT the invoice secret)
          |----------------------|     | total_amount: 1650 msat   |     | total_amount: 1600 msat   |     | total_amount: 1500 msat     | <- t2 is using multi-part to pay e, still 500 msat more to receive
          |     (encrypted)      |     | trampoline_onion:         |     | trampoline_onion:         |     | trampoline_onion:           |
          +----------------------+     | +-----------------------+ |     | +-----------------------+ |     | +-------------------------+ |
                                       | | amount_fwd: 1600 msat | |     | | amount_fwd: 1500 msat | |     | | amount_fwd: 1500 msat   | |
                                       | | expiry: 600042        | |     | | expiry: 600000        | |     | | expiry: 600000          | |
                                       | | node_id: t2           | |     | | node_id: e            | |     | | total_amount: 2500 msat | | <- may be bigger than amount_fwd in case the payment is split among multiple trampoline routes
                                       | +-----------------------+ |     | +-----------------------+ |     | | secret: yyyyy           | | <- invoice secret
                                       | |      (encrypted)      | |     | |      (encrypted)      | |     | +-------------------------+ |
                                       | +-----------------------+ |     | +-----------------------+ |     | |         EOF             | |
                                       +---------------------------+     +---------------------------+     | +-------------------------+ |
                                       |             EOF           |     |             EOF           |     +-----------------------------+
                                       +---------------------------+     +---------------------------+     |             EOF             |
                                                                                                           +-----------------------------+

Notes:
  - there may be two layers of multi-part: a may split the payment between multiple trampoline routes, and inside each
  trampoline route payments may be split into multiple parts.
  - when multi-part is used to reach trampoline nodes, the payment secret in the outer onion is NOT the invoice secret.
  We want only the recipient to receive the invoice payment secret. The payment secrets in outer onions are generated
  randomly by the sender to simply prevent next-to-last non-trampoline nodes from probing their position in the route or
  steal some fees.

TRAMPOLINE PAYMENT TO LEGACY RECIPIENT (the last trampoline node converts to a standard payment to the final recipient):

    a -------------> b ----------------------------> t1 -----------------------------> t2 ---------------------------------- -> e ---------------------------> f
         +-----------------------+     +---------------------------+     +---------------------------------+        +-----------------------+     +-------------------------+
         | amount_fwd: 1750 msat |     | amount_fwd: 1750 msat     |     | amount_fwd: 1600 msat           |        | amount_fwd: 1000 msat |     | amount_fwd: 1000 msat   |
         | expiry: 600112        |     | expiry: 600112            |     | expiry: 600042                  |        | expiry: 600000        |     | expiry: 600000          |
         | channel_id: 42        |     | secret: yyyyy             |     | secret: zzzzz                   |  +---->| channel_id: 42        |---->| secret: xyz             | <- invoice secret (omitted if not supported by invoice)
         +-----------------------+     | total_amount: 1750 msat   |     | total_amount: 1600 msat         |  |     +-----------------------+     | total_amount: 2500 msat | <- t2 is using multi-part to pay 1500 msat to f, for a total payment
         |     (encrypted)       |     | trampoline_onion:         |     | trampoline_onion:               |  |     |     (encrypted)       |     +-------------------------+    of 2500 msat split between multiple trampoline routes (omitted if
         +-----------------------+     | +-----------------------+ |     | +-----------------------------+ |  |     +-----------------------+     |           EOF           |    MPP not supported by invoice).
                                       | | amount_fwd: 1600 msat | |     | | amount_fwd: 1500 msat       | |  |                                   +-------------------------+    The remaining 1000 msat needed to reach the total 2500 msat have
                                       | | expiry: 600042        | |     | | expiry: 600000              | |--+                                                                  been sent via a completely separate trampoline route (not included
                                       | | node_id: t2           | |     | | total_amount: 2500 msat     | |  |     +-----------------------+     +-------------------------+    in this diagram).
                                       | +-----------------------+ |     | | secret: xyz                 | |  |     | amount_fwd: 500 msat  |     | amount_fwd: 500 msat    |
                                       | |      (encrypted)      | |     | | node_id: f                  | |  |     | expiry: 600000        |     | expiry: 600000          |
                                       | +-----------------------+ |     | | invoice_features: 0x0a      | |  +---->| channel_id: 43        |---->| secret: xyz             |
                                       +---------------------------+     | | invoice_routing_info: ..... | |        +-----------------------+     | total_amount: 2500 msat |
                                       |             EOF           |     | +-----------------------------+ |        |     (encrypted)       |     +-------------------------+
                                       +---------------------------+     | |      (encrypted)            | |        +-----------------------+     |           EOF           |
                                                                         | +-----------------------------+ |                                      +-------------------------+
                                                                         +---------------------------------+
                                                                         |             EOF                 |
                                                                         +---------------------------------+

Notes:
  - the last trampoline node learns the payment details (who the recipient is, the payment amount and secret)
  - but it doesn't learn the sender's identity
  - if the invoice doesn't specify an amount, the last trampoline node can pay a lower amount than what the sender
  intended, thus stealing a lot of fees for himself (the wallet should disable paying to a 0-value invoice via
  trampoline to prevent this attack)
  - if f doesn't support MPP, t2 will send a single-part payment or fail if there isn't enough capacity
  - as in normal trampoline scenario, payment secrets in the outer onion are NOT the invoice secret

 */

/** Tlv types used inside a payment onion. */
sealed trait OnionPaymentPayloadTlv extends Tlv

object OnionPaymentPayloadTlv {

  /** Amount to forward to the next node. */
  case class AmountToForward(amount: MilliSatoshi)
      extends OnionPaymentPayloadTlv

  /** CLTV value to use for the HTLC offered to the next node. */
  case class OutgoingCltv(cltv: CltvExpiry) extends OnionPaymentPayloadTlv

  /** Id of the channel to use to forward a payment to the next node. */
  case class OutgoingChannelId(shortChannelId: Long)
      extends OnionPaymentPayloadTlv

  /** Bolt 11 payment details (only included for the last node).
    *
    * @param secret
    *   payment secret specified in the Bolt 11 invoice.
    * @param totalAmount
    *   total amount in multi-part payments. When missing, assumed to be equal
    *   to AmountToForward.
    */
  case class PaymentData(secret: ByteVector32, totalAmount: MilliSatoshi)
      extends OnionPaymentPayloadTlv

  /** Route blinding lets the recipient provide some encrypted data for each
    * intermediate node in the blinded part of the route. This data cannot be
    * decrypted or modified by the sender and usually contains information to
    * locate the next node without revealing it to the sender.
    */
  case class EncryptedRecipientData(data: ByteVector)
      extends OnionPaymentPayloadTlv

  /** Blinding ephemeral public key that should be used to derive shared secrets
    * when using route blinding.
    */
  case class BlindingPoint(publicKey: PublicKey) extends OnionPaymentPayloadTlv

  /** Id of the next node. */
  case class OutgoingNodeId(nodeId: PublicKey) extends OnionPaymentPayloadTlv

  /** When payment metadata is included in a Bolt 11 invoice, we should send it
    * as-is to the recipient. This lets recipients generate invoices without
    * having to store anything on their side until the invoice is paid.
    */
  case class PaymentMetadata(data: ByteVector) extends OnionPaymentPayloadTlv

  /** Invoice feature bits. Only included for intermediate trampoline nodes when
    * they should convert to a legacy payment because the final recipient
    * doesn't support trampoline.
    */
  case class InvoiceFeatures(features: ByteVector)
      extends OnionPaymentPayloadTlv

  /** Invoice routing hints. Only included for intermediate trampoline nodes
    * when they should convert to a legacy payment because the final recipient
    * doesn't support trampoline.
    */
  case class InvoiceRoutingInfo(extraHops: List[Bolt11Invoice.ExtraHops])
      extends OnionPaymentPayloadTlv

  /** An encrypted trampoline onion packet. */
  case class TrampolineOnion(packet: OnionRoutingPacket)
      extends OnionPaymentPayloadTlv

  /** Pre-image included by the sender of a payment in case of a donation */
  case class KeySend(paymentPreimage: ByteVector32)
      extends OnionPaymentPayloadTlv

}

object PaymentOnion {

  import OnionPaymentPayloadTlv._

  /*
   * We use the following architecture for payment onion payloads:
   *
   *                                                              PerHopPayload
   *                                           _______________________/\_______________
   *                                          /                                        \
   *                                 RelayPayload                                   FinalPayload
   *                     _______________/\_________________                              \______
   *                    /                                  \                                    \
   *           ChannelRelayPayload                          \                                    \
   *         ________/\______________                        \                                    \
   *        /                        \                        \                                    \
   * RelayLegacyPayload     ChannelRelayTlvPayload     NodeRelayPayload                      FinalTlvPayload
   *
   * We also introduce additional traits to separate payloads based on their encoding (PerHopPayloadFormat) and on the
   * type of onion packet they can be used with (PacketType).
   */

  sealed trait PerHopPayloadFormat

  /** Legacy fixed-size 65-bytes onion payload. */
  sealed trait LegacyFormat extends PerHopPayloadFormat

  /** Variable-length onion payload with optional additional tlv records. */
  sealed trait TlvFormat extends PerHopPayloadFormat {
    def records: TlvStream[OnionPaymentPayloadTlv]
  }

  /** Payment onion packet type. */
  sealed trait PacketType

  /** A payment onion packet is used when offering an HTLC to a remote node. */
  sealed trait PaymentPacket extends PacketType

  /** A trampoline onion packet is used to defer route construction to
    * trampoline nodes. It is usually embedded inside a [[PaymentPacket]] in the
    * final node's payload.
    */
  sealed trait TrampolinePacket extends PacketType

  /** Per-hop payload from an HTLC's payment onion (after decryption and
    * decoding).
    */
  sealed trait PerHopPayload

  /** Per-hop payload for an intermediate node. */
  sealed trait RelayPayload extends PerHopPayload with PerHopPayloadFormat {

    /** Amount to forward to the next node. */
    val amountToForward: MilliSatoshi

    /** CLTV value to use for the HTLC offered to the next node. */
    val outgoingCltv: CltvExpiry
  }

  sealed trait ChannelRelayPayload extends RelayPayload with PaymentPacket {

    /** Id of the channel to use to forward a payment to the next node. */
    val outgoingChannelId: Long
  }

  /** Per-hop payload for a final node. */
  sealed trait FinalPayload
      extends PerHopPayload
      with PerHopPayloadFormat
      with TrampolinePacket
      with PaymentPacket {
    val amount: MilliSatoshi
    val expiry: CltvExpiry
    val paymentSecret: ByteVector32
    val totalAmount: MilliSatoshi
    val paymentPreimage: Option[ByteVector32]
    val paymentMetadata: Option[ByteVector]
  }

  case class RelayLegacyPayload(
      outgoingChannelId: Long,
      amountToForward: MilliSatoshi,
      outgoingCltv: CltvExpiry
  ) extends ChannelRelayPayload
      with LegacyFormat

  case class ChannelRelayTlvPayload(records: TlvStream[OnionPaymentPayloadTlv])
      extends ChannelRelayPayload
      with TlvFormat {
    override val amountToForward =
      records.records.collectFirst { case v: AmountToForward => v }.get.amount
    override val outgoingCltv =
      records.records.collectFirst { case v: OutgoingCltv => v }.get.cltv
    override val outgoingChannelId =
      records.records
        .collectFirst { case v: OutgoingChannelId => v }
        .get
        .shortChannelId
  }

  object ChannelRelayTlvPayload {
    def apply(
        outgoingChannelId: Long,
        amountToForward: MilliSatoshi,
        outgoingCltv: CltvExpiry
    ): ChannelRelayTlvPayload =
      ChannelRelayTlvPayload(
        TlvStream(
          OnionPaymentPayloadTlv.AmountToForward(amountToForward),
          OnionPaymentPayloadTlv.OutgoingCltv(outgoingCltv),
          OnionPaymentPayloadTlv.OutgoingChannelId(outgoingChannelId)
        )
      )
  }

  case class NodeRelayPayload(records: TlvStream[OnionPaymentPayloadTlv])
      extends RelayPayload
      with TlvFormat
      with TrampolinePacket {
    val amountToForward =
      records.records.collectFirst { case v: AmountToForward => v }.get.amount
    val outgoingCltv =
      records.records.collectFirst { case v: OutgoingCltv => v }.get.cltv
    val outgoingNodeId =
      records.records.collectFirst { case v: OutgoingNodeId => v }.get.nodeId
    // The following fields are only included in the trampoline-to-legacy case.
    val totalAmount = records.records
      .collectFirst { case v: PaymentData => v }
      .map(_.totalAmount)
      .filter(_.toLong != 0L)
      .getOrElse(amountToForward)
    val paymentSecret =
      records.records.collectFirst { case v: PaymentData => v }.map(_.secret)
    val paymentMetadata =
      records.records.collectFirst { case v: PaymentMetadata => v }.map(_.data)
    val invoiceFeatures = records.records
      .collectFirst { case v: InvoiceFeatures => v }
      .map(_.features)
    val invoiceRoutingInfo = records.records
      .collectFirst { case v: InvoiceRoutingInfo => v }
      .map(_.extraHops)
  }

  case class FinalTlvPayload(records: TlvStream[OnionPaymentPayloadTlv])
      extends FinalPayload
      with TlvFormat {
    override val amount =
      records.records.collectFirst { case v: AmountToForward => v }.get.amount
    override val expiry =
      records.records.collectFirst { case v: OutgoingCltv => v }.get.cltv
    override val paymentSecret =
      records.records.collectFirst { case v: PaymentData => v }.get.secret
    override val totalAmount = records.records
      .collectFirst { case v: PaymentData => v }
      .map(_.totalAmount)
      .filter(_.toLong != 0L)
      .getOrElse(amount)
    override val paymentPreimage = records.records
      .collectFirst { case v: KeySend => v }
      .map(_.paymentPreimage)
    override val paymentMetadata =
      records.records.collectFirst { case v: PaymentMetadata => v }.map(_.data)
  }

  def createNodeRelayPayload(
      amount: MilliSatoshi,
      expiry: CltvExpiry,
      nextNodeId: PublicKey
  ): NodeRelayPayload =
    NodeRelayPayload(
      TlvStream(
        AmountToForward(amount),
        OutgoingCltv(expiry),
        OutgoingNodeId(nextNodeId)
      )
    )

  /** Create a trampoline inner payload instructing the trampoline node to relay
    * via a non-trampoline payment.
    */
  def createNodeRelayToNonTrampolinePayload(
      amount: MilliSatoshi,
      totalAmount: MilliSatoshi,
      expiry: CltvExpiry,
      targetNodeId: PublicKey,
      invoice: Bolt11Invoice
  ): NodeRelayPayload = {
    val tlvs = Seq(
      Some(AmountToForward(amount)),
      Some(OutgoingCltv(expiry)),
      invoice.paymentSecret.map(s => PaymentData(s, totalAmount)),
      invoice.paymentMetadata.map(m => PaymentMetadata(m)),
      Some(OutgoingNodeId(targetNodeId)),
      Some(InvoiceFeatures(invoice.features.toByteVector)),
      Some(InvoiceRoutingInfo(invoice.routingInfo))
    ).flatten
    NodeRelayPayload(TlvStream(tlvs))
  }

  def createSinglePartPayload(
      amount: MilliSatoshi,
      expiry: CltvExpiry,
      paymentSecret: ByteVector32,
      paymentMetadata: Option[ByteVector],
      userCustomTlvs: Seq[GenericTlv] = Nil
  ): FinalPayload = {
    val tlvs = Seq(
      Some(AmountToForward(amount)),
      Some(OutgoingCltv(expiry)),
      Some(PaymentData(paymentSecret, amount)),
      paymentMetadata.map(m => PaymentMetadata(m))
    ).flatten
    FinalTlvPayload(TlvStream(tlvs, userCustomTlvs))
  }

  def createMultiPartPayload(
      amount: MilliSatoshi,
      totalAmount: MilliSatoshi,
      expiry: CltvExpiry,
      paymentSecret: ByteVector32,
      paymentMetadata: Option[ByteVector],
      additionalTlvs: Seq[OnionPaymentPayloadTlv] = Nil,
      userCustomTlvs: Seq[GenericTlv] = Nil
  ): FinalPayload = {
    val tlvs = Seq(
      Some(AmountToForward(amount)),
      Some(OutgoingCltv(expiry)),
      Some(PaymentData(paymentSecret, totalAmount)),
      paymentMetadata.map(m => PaymentMetadata(m))
    ).flatten
    FinalTlvPayload(TlvStream(tlvs ++ additionalTlvs, userCustomTlvs))
  }

  /** Create a trampoline outer payload. */
  def createTrampolinePayload(
      amount: MilliSatoshi,
      totalAmount: MilliSatoshi,
      expiry: CltvExpiry,
      paymentSecret: ByteVector32,
      trampolinePacket: OnionRoutingPacket
  ): FinalPayload = {
    FinalTlvPayload(
      TlvStream(
        AmountToForward(amount),
        OutgoingCltv(expiry),
        PaymentData(paymentSecret, totalAmount),
        TrampolineOnion(trampolinePacket)
      )
    )
  }
}

object PaymentOnionCodecs {

  import OnionPaymentPayloadTlv._
  import PaymentOnion._
  import scodec.bits.HexStringSyntax
  import scodec.codecs._
  import scodec.{Attempt, Codec, DecodeResult, Decoder}

  val paymentOnionPayloadLength = 1300
  val trampolineOnionPayloadLength = 400
  val paymentOnionPacketCodec: Codec[OnionRoutingPacket] =
    OnionRoutingCodecs.onionRoutingPacketCodec(paymentOnionPayloadLength)
  val trampolineOnionPacketCodec: Codec[OnionRoutingPacket] =
    OnionRoutingCodecs.onionRoutingPacketCodec(trampolineOnionPayloadLength)

  /** The 1.1 BOLT spec changed the payment onion frame format to use
    * variable-length per-hop payloads. The first bytes contain a varint
    * encoding the length of the payload data (not including the trailing mac).
    * That varint is considered to be part of the payload, so the payload length
    * includes the number of bytes used by the varint prefix.
    */
  val payloadLengthDecoder = Decoder[Long]((bits: BitVector) =>
    varintoverflow
      .decode(bits)
      .map(d =>
        DecodeResult(
          d.value + (bits.length - d.remainder.length) / 8,
          d.remainder
        )
      )
  )

  private val amountToForward: Codec[AmountToForward] =
    ("amount_msat" | ltmillisatoshi).as[AmountToForward]

  private val outgoingCltv: Codec[OutgoingCltv] = ("cltv" | ltu32).xmap(
    cltv => OutgoingCltv(CltvExpiry(cltv)),
    (c: OutgoingCltv) => c.cltv.underlying
  )

  private val outgoingChannelId: Codec[OutgoingChannelId] =
    variableSizeBytesLong(varintoverflow, "short_channel_id" | int64)
      .as[OutgoingChannelId]

  private val paymentData: Codec[PaymentData] = variableSizeBytesLong(
    varintoverflow,
    ("payment_secret" | bytes32) :: ("total_msat" | tmillisatoshi)
  ).as[PaymentData]

  private val encryptedRecipientData: Codec[EncryptedRecipientData] =
    variableSizeBytesLong(varintoverflow, "encrypted_data" | bytes)
      .as[EncryptedRecipientData]

  private val blindingPoint: Codec[BlindingPoint] =
    (("length" | constant(hex"21")) :: ("blinding" | publicKey))
      .as[BlindingPoint]

  private val outgoingNodeId: Codec[OutgoingNodeId] =
    (("length" | constant(hex"21")) :: ("node_id" | publicKey))
      .as[OutgoingNodeId]

  private val paymentMetadata: Codec[PaymentMetadata] =
    variableSizeBytesLong(varintoverflow, "payment_metadata" | bytes)
      .as[PaymentMetadata]

  private val invoiceFeatures: Codec[InvoiceFeatures] =
    variableSizeBytesLong(varintoverflow, bytes).as[InvoiceFeatures]

  private val invoiceRoutingInfo: Codec[InvoiceRoutingInfo] =
    variableSizeBytesLong(
      varintoverflow,
      list(listOfN(uint8, Bolt11Invoice.Codecs.extraHopCodec))
    ).as[InvoiceRoutingInfo]

  private val trampolineOnion: Codec[TrampolineOnion] =
    variableSizeBytesLong(varintoverflow, trampolineOnionPacketCodec)
      .as[TrampolineOnion]

  private val keySend: Codec[KeySend] =
    variableSizeBytesLong(varintoverflow, bytes32).as[KeySend]

  private val onionTlvCodec = discriminated[OnionPaymentPayloadTlv]
    .by(varint)
    .\(UInt64(2)) { case v: AmountToForward => v }(amountToForward)
    .\(UInt64(4)) { case v: OutgoingCltv => v }(outgoingCltv)
    .\(UInt64(6)) { case v: OutgoingChannelId => v }(outgoingChannelId)
    .\(UInt64(8)) { case v: PaymentData => v }(paymentData)
    .\(UInt64(10)) { case v: EncryptedRecipientData => v }(
      encryptedRecipientData
    )
    .\(UInt64(12)) { case v: BlindingPoint => v }(blindingPoint)
    .\(UInt64(16)) { case v: PaymentMetadata => v }(paymentMetadata)
    // Types below aren't specified - use cautiously when deploying (be careful with backwards-compatibility).
    .\(UInt64(66097)) { case v: InvoiceFeatures => v }(invoiceFeatures)
    .\(UInt64(66098)) { case v: OutgoingNodeId => v }(outgoingNodeId)
    .\(UInt64(66099)) { case v: InvoiceRoutingInfo => v }(
      invoiceRoutingInfo
    )
    .\(UInt64(66100)) { case v: TrampolineOnion => v }(trampolineOnion)
    .\(UInt64(5482373484L)) { case v: KeySend => v }(keySend)

  val tlvPerHopPayloadCodec: Codec[TlvStream[OnionPaymentPayloadTlv]] =
    TlvCodecs
      .lengthPrefixedTlvStream[OnionPaymentPayloadTlv](onionTlvCodec)
      .complete

  private val legacyRelayPerHopPayloadCodec: Codec[RelayLegacyPayload] =
    (("realm" | constant(ByteVector.fromByte(0))) ::
      ("short_channel_id" | int64) ::
      ("amt_to_forward" | millisatoshi) ::
      ("outgoing_cltv_value" | cltvExpiry) ::
      ("unused_with_v0_version_on_header" | ignore(8 * 12)))
      .as[RelayLegacyPayload]

  val channelRelayPerHopPayloadCodec: Codec[ChannelRelayPayload] =
    fallback(tlvPerHopPayloadCodec, legacyRelayPerHopPayloadCodec).narrow(
      {
        case Left(tlvs) if tlvs.records.collectFirst {
              case v: AmountToForward => v
            }.isEmpty =>
          Attempt.failure(MissingRequiredTlv(UInt64(2)))
        case Left(tlvs) if tlvs.records.collectFirst { case v: OutgoingCltv =>
              v
            }.isEmpty =>
          Attempt.failure(MissingRequiredTlv(UInt64(4)))
        case Left(tlvs) if tlvs.records.collectFirst {
              case v: OutgoingChannelId => v
            }.isEmpty =>
          Attempt.failure(MissingRequiredTlv(UInt64(6)))
        case Left(tlvs)    => Attempt.successful(ChannelRelayTlvPayload(tlvs))
        case Right(legacy) => Attempt.successful(legacy)
      },
      {
        case legacy: RelayLegacyPayload   => Right(legacy)
        case ChannelRelayTlvPayload(tlvs) => Left(tlvs)
      }
    )

  val nodeRelayPerHopPayloadCodec: Codec[NodeRelayPayload] =
    tlvPerHopPayloadCodec.narrow(
      {
        case tlvs if tlvs.records.collectFirst { case v: AmountToForward =>
              v
            }.isEmpty =>
          Attempt.failure(MissingRequiredTlv(UInt64(2)))
        case tlvs if tlvs.records.collectFirst { case v: OutgoingCltv =>
              v
            }.isEmpty =>
          Attempt.failure(MissingRequiredTlv(UInt64(4)))
        case tlvs if tlvs.records.collectFirst { case v: OutgoingNodeId =>
              v
            }.isEmpty =>
          Attempt.failure(MissingRequiredTlv(UInt64(66098)))
        case tlvs => Attempt.successful(NodeRelayPayload(tlvs))
      },
      { case NodeRelayPayload(tlvs) =>
        tlvs
      }
    )

  val finalPerHopPayloadCodec: Codec[FinalPayload] =
    tlvPerHopPayloadCodec.narrow(
      {
        case tlvs if tlvs.records.collectFirst { case v: AmountToForward =>
              v
            }.isEmpty =>
          Attempt.failure(MissingRequiredTlv(UInt64(2)))
        case tlvs if tlvs.records.collectFirst { case v: OutgoingCltv =>
              v
            }.isEmpty =>
          Attempt.failure(MissingRequiredTlv(UInt64(4)))
        case tlvs if tlvs.records.collectFirst { case v: PaymentData =>
              v
            }.isEmpty =>
          Attempt.failure(MissingRequiredTlv(UInt64(8)))
        case tlvs => Attempt.successful(FinalTlvPayload(tlvs))
      },
      { case FinalTlvPayload(tlvs) =>
        tlvs
      }
    )

  def paymentOnionPerHopPayloadCodec(
      isLastPacket: Boolean
  ): Codec[PaymentPacket] = if (isLastPacket)
    finalPerHopPayloadCodec.upcast[PaymentPacket]
  else channelRelayPerHopPayloadCodec.upcast[PaymentPacket]

  def trampolineOnionPerHopPayloadCodec(
      isLastPacket: Boolean
  ): Codec[TrampolinePacket] = if (isLastPacket)
    finalPerHopPayloadCodec.upcast[TrampolinePacket]
  else nodeRelayPerHopPayloadCodec.upcast[TrampolinePacket]

}
