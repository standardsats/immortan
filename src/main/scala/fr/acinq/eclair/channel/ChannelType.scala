package fr.acinq.eclair.channel

import fr.acinq.eclair.transactions.Transactions.{
  AnchorOutputsCommitmentFormat,
  CommitmentFormat,
  DefaultCommitmentFormat,
  ZeroFeeHtlcTxAnchorOutputsCommitmentFormat
}
import fr.acinq.eclair.{FeatureSupport, Features, InitFeature}

sealed trait ChannelType {
  def featureBits: Features[InitFeature]
  lazy val features = featureBits.activated.keySet
}

sealed trait SupportedChannelType extends ChannelType {
  def commitmentFormat: CommitmentFormat
}

object ChannelTypes {
  private def toFeatureBits(
      features: Set[fr.acinq.eclair.Feature with InitFeature]
  ): Features[InitFeature] =
    Features(features.map(_ -> FeatureSupport.Mandatory).toMap)

  case object StaticRemoteKeyOnly extends SupportedChannelType {
    override val featureBits: Features[InitFeature] =
      toFeatureBits(Set(fr.acinq.eclair.Features.StaticRemoteKey))
    override val commitmentFormat: CommitmentFormat = DefaultCommitmentFormat
    override def toString: String = "static_remotekey"
  }

  case object AnchorOutputs extends SupportedChannelType {
    override val featureBits: Features[InitFeature] = toFeatureBits(
      Set(
        fr.acinq.eclair.Features.StaticRemoteKey,
        fr.acinq.eclair.Features.AnchorOutputs
      )
    )
    override val commitmentFormat: CommitmentFormat =
      AnchorOutputsCommitmentFormat
    override def toString: String = "anchor_outputs_legacy"
  }

  case object AnchorOutputsZeroFeeHtlcTx extends SupportedChannelType {
    override val featureBits: Features[InitFeature] = toFeatureBits(
      Set(
        fr.acinq.eclair.Features.StaticRemoteKey,
        fr.acinq.eclair.Features.AnchorOutputsZeroFeeHtlcTx
      )
    )
    override val commitmentFormat: CommitmentFormat =
      ZeroFeeHtlcTxAnchorOutputsCommitmentFormat
    override def toString: String = "anchor_outputs_zero_fee_htlc_tx"
  }

  case class UnsupportedChannelType(featureBits: Features[InitFeature])
      extends ChannelType {
    override def toString: String = s"0x${featureBits.toByteVector.toHex}"
  }

  private val staticRemoteKeySet
      : Set[fr.acinq.eclair.Feature with InitFeature] =
    Set(fr.acinq.eclair.Features.StaticRemoteKey)
  private val anchorOutputsSet
      : Set[fr.acinq.eclair.Feature with InitFeature] =
    Set(
      fr.acinq.eclair.Features.StaticRemoteKey,
      fr.acinq.eclair.Features.AnchorOutputs
    )
  private val anchorOutputsZeroFeeSet
      : Set[fr.acinq.eclair.Feature with InitFeature] =
    Set(
      fr.acinq.eclair.Features.StaticRemoteKey,
      fr.acinq.eclair.Features.AnchorOutputsZeroFeeHtlcTx
    )

  def fromFeatures(features: Features[InitFeature]): ChannelType =
    if (features.unknown.nonEmpty)
      UnsupportedChannelType(features)
    else features.activated.keySet match {
      case fs if fs == staticRemoteKeySet       => StaticRemoteKeyOnly
      case fs if fs == anchorOutputsSet         => AnchorOutputs
      case fs if fs == anchorOutputsZeroFeeSet  => AnchorOutputsZeroFeeHtlcTx
      case _                                    => UnsupportedChannelType(features)
    }
}
