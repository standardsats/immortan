package immortan

import fr.acinq.bitcoin.Crypto.ripemd160
import fr.acinq.bitcoin.Script.pay2wsh
import fr.acinq.bitcoin._
import fr.acinq.eclair._
import fr.acinq.eclair.FeatureSupport.Optional
import fr.acinq.eclair.Features.{
  AnchorOutputs,
  AnchorOutputsZeroFeeHtlcTx,
  ChannelType,
  StaticRemoteKey
}
import fr.acinq.eclair.blockchain.fee.FeeratePerKw
import fr.acinq.eclair.channel.ChannelFeatures
import fr.acinq.eclair.channel.ChannelTypes
import fr.acinq.eclair.transactions.CommitmentOutput.{InHtlc, OutHtlc}
import fr.acinq.eclair.transactions.Transactions.{
  AnchorOutputsCommitmentFormat,
  CommitmentFormat,
  CommitmentOutputLink,
  ZeroFeeHtlcTxAnchorOutputsCommitmentFormat
}
import fr.acinq.eclair.transactions.{
  CommitmentSpec,
  IncomingHtlc,
  OutgoingHtlc,
  Scripts,
  Transactions
}
import fr.acinq.eclair.crypto.Sphinx._
import fr.acinq.eclair.channel.CMD_ADD_HTLC
import fr.acinq.eclair.crypto.SphinxTestHelpers._
import fr.acinq.eclair.wire.LightningMessageCodecs._
import fr.acinq.eclair.wire._
import scodec.bits.ByteVector
import immortan.crypto.Tools
import utest._

object WireSpec extends TestSuite {
  private def randomPacket: OnionRoutingPacket = OnionRoutingPacket(
    version = 2,
    publicKey = randomBytes(33),
    payload = randomBytes(1300),
    hmac = randomBytes32
  )

  private def makeAdd(
      id: Long,
      amount: MilliSatoshi,
      cltvExpiry: CltvExpiry
  ): UpdateAddHtlc =
    UpdateAddHtlc(
      channelId = randomBytes32,
      id = id,
      amountMsat = amount,
      paymentHash = randomBytes32,
      cltvExpiry = cltvExpiry,
      onionRoutingPacket = randomPacket
    )

  private def makeCommitTx(output: TxOut): Transaction = Transaction(
    version = 2,
    txIn = TxIn(OutPoint(randomBytes32, 0), ByteVector.empty, sequence = 0) :: Nil,
    txOut = output :: Nil,
    lockTime = 0
  )

  private def makeOutgoingLink(
      commitmentFormat: CommitmentFormat
  ): (Transaction, CommitmentOutputLink[OutHtlc], Satoshi) = {
    val add = makeAdd(id = 1L, amount = 200000000L.msat, cltvExpiry = CltvExpiry(500))
    val localHtlcPubkey = randomKey.publicKey
    val remoteHtlcPubkey = randomKey.publicKey
    val revocationPubkey = randomKey.publicKey
    val redeemScript = Scripts.htlcOffered(
      localHtlcPubkey,
      remoteHtlcPubkey,
      revocationPubkey,
      ripemd160(add.paymentHash.bytes),
      commitmentFormat
    )
    val amount = 200000.sat
    val txOut = TxOut(amount, pay2wsh(redeemScript))
    val commitTx = makeCommitTx(txOut)
    val output = CommitmentOutputLink(
      txOut,
      redeemScript,
      OutHtlc(OutgoingHtlc(add))
    )
    (commitTx, output, amount)
  }

  private def makeIncomingLink(
      commitmentFormat: CommitmentFormat
  ): (Transaction, CommitmentOutputLink[InHtlc], Satoshi) = {
    val add = makeAdd(id = 2L, amount = 200000000L.msat, cltvExpiry = CltvExpiry(550))
    val localHtlcPubkey = randomKey.publicKey
    val remoteHtlcPubkey = randomKey.publicKey
    val revocationPubkey = randomKey.publicKey
    val redeemScript = Scripts.htlcReceived(
      localHtlcPubkey,
      remoteHtlcPubkey,
      revocationPubkey,
      ripemd160(add.paymentHash.bytes),
      add.cltvExpiry,
      commitmentFormat
    )
    val amount = 200000.sat
    val txOut = TxOut(amount, pay2wsh(redeemScript))
    val commitTx = makeCommitTx(txOut)
    val output = CommitmentOutputLink(
      txOut,
      redeemScript,
      InHtlc(IncomingHtlc(add))
    )
    (commitTx, output, amount)
  }

  val tests = Tests {
    test("HC wraps normal messages before sending") {
      val packet = OnionRoutingPacket(
        version = 2,
        publicKey = randomBytes(33),
        payload = randomBytes(1300),
        hmac = randomBytes32
      )
      val add = UpdateAddHtlc(
        randomBytes32,
        id = 100L,
        amountMsat = 1000000L.msat,
        paymentHash = randomBytes32,
        cltvExpiry = CltvExpiry(288),
        onionRoutingPacket = packet
      )

      // Normal message gets wrapped because it comes from HC, then falls through unchanged when prepared
      val msg1 @ UnknownMessage(
        LightningMessageCodecs.HC_UPDATE_ADD_HTLC_TAG,
        _
      ) = LightningMessageCodecs.prepareNormal(add)
      val msg2 @ UnknownMessage(
        LightningMessageCodecs.HC_UPDATE_ADD_HTLC_TAG,
        _
      ) = LightningMessageCodecs.prepare(msg1)

      val encoded = lightningMessageCodecWithFallback.encode(msg2).require
      val decoded =
        lightningMessageCodecWithFallback.decode(encoded).require.value
      assert(decoded == msg2)
    }

    test("HC does not wrap extended messages before sending") {
      val resizeMessage = ResizeChannel(newCapacity = 10000000000L.sat)

      // Extended message falls through `prepareNormal`, but gets wrapped when prepared
      val msg1 = LightningMessageCodecs
        .prepareNormal(resizeMessage)
        .asInstanceOf[ResizeChannel]
      val msg2 @ UnknownMessage(
        LightningMessageCodecs.HC_RESIZE_CHANNEL_TAG,
        _
      ) =
        LightningMessageCodecs.prepare(msg1)

      val encoded = lightningMessageCodecWithFallback.encode(msg2).require
      val decoded =
        lightningMessageCodecWithFallback.decode(encoded).require.value
      assert(decoded == msg2)
    }

    test("NC does not wrap normal messages") {
      val packet = OnionRoutingPacket(
        version = 2,
        publicKey = randomBytes(33),
        payload = randomBytes(1300),
        hmac = randomBytes32
      )
      val add = UpdateAddHtlc(
        randomBytes32,
        id = 100L,
        amountMsat = 1000000L.msat,
        paymentHash = randomBytes32,
        cltvExpiry = CltvExpiry(288),
        onionRoutingPacket = packet
      )

      // Normal message coming from normal channel does not get wrapped in any way
      val msg2 = LightningMessageCodecs.prepare(add).asInstanceOf[UpdateAddHtlc]

      val encoded = lightningMessageCodecWithFallback.encode(msg2).require
      val decoded =
        lightningMessageCodecWithFallback.decode(encoded).require.value
      assert(decoded == msg2)
    }

    test("UpdateAddHtlc tag encryption and partId equivalence") {
      LNParams.secret = WalletSecret.random()

      val payload = PaymentOnion.createSinglePartPayload(
        1000000L.msat,
        CltvExpiry(144),
        randomBytes32,
        None
      )
      val packetAndSecrets = create(
        sessionKey,
        1300,
        publicKeys,
        referenceFixedSizePaymentPayloads,
        associatedData
      ).toOption.get
      val fullTag = FullPaymentTag(
        paymentHash = ByteVector32.Zeroes,
        paymentSecret = ByteVector32.One,
        tag = PaymentTagTlv.LOCALLY_SENT
      )
      val cmd = CMD_ADD_HTLC(
        fullTag,
        firstAmount = 1000000L.msat,
        CltvExpiry(144),
        packetAndSecrets,
        payload
      )
      assert(cmd.incompleteAdd.partId == cmd.packetAndSecrets.packet.publicKey)
      assert(cmd.incompleteAdd.fullTag == fullTag)
    }

    test("LCSS") {
      LNParams.secret = WalletSecret.random()

      val payload = PaymentOnion.createSinglePartPayload(
        1000000L.msat,
        CltvExpiry(144),
        randomBytes32,
        None
      )
      val packetAndSecrets = create(
        sessionKey,
        1300,
        publicKeys,
        referenceFixedSizePaymentPayloads,
        associatedData
      ).toOption.get
      val fullTag = FullPaymentTag(
        paymentHash = ByteVector32.Zeroes,
        paymentSecret = ByteVector32.One,
        tag = PaymentTagTlv.LOCALLY_SENT
      )
      val cmd = CMD_ADD_HTLC(
        fullTag,
        firstAmount = 1000000L.msat,
        CltvExpiry(144),
        packetAndSecrets,
        payload
      )

      val add1 = UpdateAddHtlc(
        randomBytes32,
        id = 1000L,
        cmd.firstAmount,
        cmd.fullTag.paymentHash,
        cmd.cltvExpiry,
        cmd.packetAndSecrets.packet,
        cmd.encryptedTag
      )
      val add2 = UpdateAddHtlc(
        randomBytes32,
        id = 1000L,
        cmd.firstAmount,
        cmd.fullTag.paymentHash,
        cmd.cltvExpiry,
        cmd.packetAndSecrets.packet
      )

      val features = List(
        Features.HostedChannels.mandatory,
        Features.ResizeableHostedChannels.mandatory
      )
      val init = InitHostedChannel(
        UInt64(1000000000L),
        htlcMinimumMsat = 100.msat,
        maxAcceptedHtlcs = 12,
        channelCapacityMsat = 10000000000L.msat,
        100000L.msat,
        features
      )

      val lcss = LastCrossSignedState(
        isHost = false,
        refundScriptPubKey = randomBytes(78),
        init,
        blockDay = 12594,
        localBalanceMsat = 100000L.msat,
        remoteBalanceMsat = 100000L.msat,
        localUpdates = 123,
        remoteUpdates = 294,
        List(add1, add2, add1),
        List(add2, add1, add2),
        remoteSigOfLocal = ByteVector64.Zeroes,
        localSigOfRemote = ByteVector64.Zeroes
      )

      assert(
        lastCrossSignedStateCodec
          .decode(lastCrossSignedStateCodec.encode(lcss).require)
          .require
          .value == lcss
      )
    }

    test("Trampoline status") {
      val trampolineOn = TrampolineOn(
        LNParams.minPayment,
        Long.MaxValue.msat,
        feeProportionalMillionths = 1000L,
        exponent = 0.0,
        logExponent = 0.0,
        LNParams.minRoutingCltvExpiryDelta
      )
      val params1 =
        NodeIdTrampolineParams(nodeId = randomKey.publicKey, trampolineOn)
      val params2 =
        NodeIdTrampolineParams(nodeId = randomKey.publicKey, trampolineOn)

      val update1 = TrampolineStatusInit(
        List(List(params1), List(params1, params2)),
        trampolineOn
      )
      val update2 = TrampolineStatusUpdate(
        List(List(params1), List(params1, params2)),
        Map(randomKey.publicKey -> trampolineOn),
        Some(trampolineOn),
        Set(randomKey.publicKey, randomKey.publicKey)
      )

      assert(
        trampolineStatusInitCodec
          .decode(trampolineStatusInitCodec.encode(update1).require)
          .require
          .value == update1
      )
      assert(
        trampolineStatusUpdateCodec
          .decode(trampolineStatusUpdateCodec.encode(update2).require)
          .require
          .value == update2
      )
    }

    test("HC short channel ids are random") {
      val hostNodeId = randomBytes32
      val iterations = 1000000
      val sids =
        List.fill(iterations)(
          Tools.hostedShortChanId(randomBytes32, hostNodeId)
        )
      assert(sids.size == sids.toSet.size)
    }

    test("pick channel features prefers zero-fee channel type") {
      val localFeatures: Features[InitFeature] = Features[InitFeature](
        (StaticRemoteKey, Optional),
        (AnchorOutputs, Optional),
        (AnchorOutputsZeroFeeHtlcTx, Optional),
        (ChannelType, Optional)
      )
      val remoteFeatures: Features[InitFeature] = Features[InitFeature](
        (StaticRemoteKey, Optional),
        (AnchorOutputs, Optional),
        (AnchorOutputsZeroFeeHtlcTx, Optional),
        (ChannelType, Optional)
      )

      val funderChoice =
        ChannelFeatures.pickChannelFeatures(localFeatures, remoteFeatures)
      assert(
        funderChoice.channelType_opt.contains(
          ChannelTypes.AnchorOutputsZeroFeeHtlcTx
        )
      )
      assert(
        funderChoice.commitmentFormat == ZeroFeeHtlcTxAnchorOutputsCommitmentFormat
      )

      val fundeeChoice = ChannelFeatures.pickChannelFeatures(
        localFeatures,
        remoteFeatures,
        Some(ChannelTypes.AnchorOutputs)
      )
      assert(fundeeChoice.channelType_opt.contains(ChannelTypes.AnchorOutputs))
      assert(fundeeChoice.commitmentFormat == AnchorOutputsCommitmentFormat)
    }

    test("channel type tlv survives open and accept codec roundtrip") {
      val open = OpenChannel(
        chainHash = randomBytes32,
        temporaryChannelId = randomBytes32,
        fundingSatoshis = 500000.sat,
        pushMsat = 0.msat,
        dustLimitSatoshis = 354.sat,
        maxHtlcValueInFlightMsat = UInt64(100000000L),
        channelReserveSatoshis = 1000.sat,
        htlcMinimumMsat = 1.msat,
        feeratePerKw = FeeratePerKw(1500.sat),
        toSelfDelay = CltvExpiryDelta(144),
        maxAcceptedHtlcs = 30,
        fundingPubkey = randomKey.publicKey,
        revocationBasepoint = randomKey.publicKey,
        paymentBasepoint = randomKey.publicKey,
        delayedPaymentBasepoint = randomKey.publicKey,
        htlcBasepoint = randomKey.publicKey,
        firstPerCommitmentPoint = randomKey.publicKey,
        channelFlags = 0.toByte,
        tlvStream = TlvStream[OpenChannelTlv](
          ChannelTlv.ChannelTypeTlv(ChannelTypes.AnchorOutputsZeroFeeHtlcTx)
        )
      )
      val decodedOpen =
        openChannelCodec.decode(openChannelCodec.encode(open).require).require.value
      assert(
        decodedOpen.channelType_opt.contains(
          ChannelTypes.AnchorOutputsZeroFeeHtlcTx
        )
      )

      val accept = AcceptChannel(
        temporaryChannelId = randomBytes32,
        dustLimitSatoshis = 354.sat,
        maxHtlcValueInFlightMsat = UInt64(100000000L),
        channelReserveSatoshis = 1000.sat,
        htlcMinimumMsat = 1.msat,
        minimumDepth = 3L,
        toSelfDelay = CltvExpiryDelta(144),
        maxAcceptedHtlcs = 30,
        fundingPubkey = randomKey.publicKey,
        revocationBasepoint = randomKey.publicKey,
        paymentBasepoint = randomKey.publicKey,
        delayedPaymentBasepoint = randomKey.publicKey,
        htlcBasepoint = randomKey.publicKey,
        firstPerCommitmentPoint = randomKey.publicKey,
        tlvStream = TlvStream[AcceptChannelTlv](
          ChannelTlv.ChannelTypeTlv(ChannelTypes.AnchorOutputsZeroFeeHtlcTx)
        )
      )
      val decodedAccept =
        acceptChannelCodec
          .decode(acceptChannelCodec.encode(accept).require)
          .require
          .value
      assert(
        decodedAccept.channelType_opt.contains(
          ChannelTypes.AnchorOutputsZeroFeeHtlcTx
        )
      )
    }

    test("zero-fee anchor htlc txs do not subtract second-stage fees") {
      val dustLimit = 354.sat
      val feeratePerKw = FeeratePerKw(5000.sat)
      val spec = CommitmentSpec(
        feeratePerKw = feeratePerKw,
        toLocal = 1000000000L.msat,
        toRemote = 1000000000L.msat
      )

      assert(
        Transactions.offeredHtlcTrimThreshold(
          dustLimit,
          spec,
          ZeroFeeHtlcTxAnchorOutputsCommitmentFormat
        ) == dustLimit
      )
      assert(
        Transactions.receivedHtlcTrimThreshold(
          dustLimit,
          spec,
          ZeroFeeHtlcTxAnchorOutputsCommitmentFormat
        ) == dustLimit
      )

      val (zeroTimeoutCommitTx, zeroTimeoutOutput, timeoutAmount) =
        makeOutgoingLink(ZeroFeeHtlcTxAnchorOutputsCommitmentFormat)
      val zeroTimeoutTx = Transactions
        .makeHtlcTimeoutTx(
          zeroTimeoutCommitTx,
          zeroTimeoutOutput,
          outputIndex = 0,
          localDustLimit = dustLimit,
          localRevocationPubkey = randomKey.publicKey,
          toLocalDelay = CltvExpiryDelta(144),
          localDelayedPaymentPubkey = randomKey.publicKey,
          feeratePerKw = feeratePerKw,
          commitmentFormat = ZeroFeeHtlcTxAnchorOutputsCommitmentFormat
        )
        .toOption
        .get

      val (legacyTimeoutCommitTx, legacyTimeoutOutput, _) =
        makeOutgoingLink(AnchorOutputsCommitmentFormat)
      val legacyTimeoutTx = Transactions
        .makeHtlcTimeoutTx(
          legacyTimeoutCommitTx,
          legacyTimeoutOutput,
          outputIndex = 0,
          localDustLimit = dustLimit,
          localRevocationPubkey = randomKey.publicKey,
          toLocalDelay = CltvExpiryDelta(144),
          localDelayedPaymentPubkey = randomKey.publicKey,
          feeratePerKw = feeratePerKw,
          commitmentFormat = AnchorOutputsCommitmentFormat
        )
        .toOption
        .get

      assert(zeroTimeoutTx.tx.txOut.head.amount == timeoutAmount)
      assert(legacyTimeoutTx.tx.txOut.head.amount < timeoutAmount)

      val (zeroSuccessCommitTx, zeroSuccessOutput, successAmount) =
        makeIncomingLink(ZeroFeeHtlcTxAnchorOutputsCommitmentFormat)
      val zeroSuccessTx = Transactions
        .makeHtlcSuccessTx(
          zeroSuccessCommitTx,
          zeroSuccessOutput,
          outputIndex = 0,
          localDustLimit = dustLimit,
          localRevocationPubkey = randomKey.publicKey,
          toLocalDelay = CltvExpiryDelta(144),
          localDelayedPaymentPubkey = randomKey.publicKey,
          feeratePerKw = feeratePerKw,
          commitmentFormat = ZeroFeeHtlcTxAnchorOutputsCommitmentFormat
        )
        .toOption
        .get

      val (legacySuccessCommitTx, legacySuccessOutput, _) =
        makeIncomingLink(AnchorOutputsCommitmentFormat)
      val legacySuccessTx = Transactions
        .makeHtlcSuccessTx(
          legacySuccessCommitTx,
          legacySuccessOutput,
          outputIndex = 0,
          localDustLimit = dustLimit,
          localRevocationPubkey = randomKey.publicKey,
          toLocalDelay = CltvExpiryDelta(144),
          localDelayedPaymentPubkey = randomKey.publicKey,
          feeratePerKw = feeratePerKw,
          commitmentFormat = AnchorOutputsCommitmentFormat
        )
        .toOption
        .get

      assert(zeroSuccessTx.tx.txOut.head.amount == successAmount)
      assert(legacySuccessTx.tx.txOut.head.amount < successAmount)
    }
  }
}
