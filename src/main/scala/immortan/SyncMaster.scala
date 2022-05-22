package immortan

import com.softwaremill.quicklens._
import fr.acinq.bitcoin.Crypto.PublicKey
import fr.acinq.eclair.Features.ChannelRangeQueriesExtended
import fr.acinq.eclair.router.Router.Data
import fr.acinq.eclair.router.{Announcements, Sync}
import fr.acinq.eclair.wire.QueryShortChannelIdsTlv.QueryFlagType._
import fr.acinq.eclair.wire._
import immortan.SyncMaster._
import immortan.crypto.Noise.KeyPair
import immortan.crypto.Tools._
import immortan.crypto.{CanBeRepliedTo, StateMachine, Tools}
import immortan.utils.Rx

import scala.collection.LazyZip3._
import java.util.concurrent.Executors
import scala.collection.mutable
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}
import scala.util.Random.shuffle

object SyncMaster {
  final val CMDAddSync = "cmd-add-sync"
  final val CMDGetGossip = "cmd-get-gossip"
  final val CMDShutdown = "cmd-shut-down"

  sealed trait State
  case class Waiting() extends State
  case class ShutDown() extends State
  case class ShortIDSync() extends State
  case class GossipSync() extends State
  case class PHCSync() extends State

  type PositionSet = Set[java.lang.Integer]
  type ConfirmedBySet = Set[PublicKey]
  type ShortChanIdSet = Set[Long]
}

sealed trait SyncWorkerData

case class SyncWorkerShortIdsData(
    ranges: List[ReplyChannelRange] = Nil,
    from: Int
) extends SyncWorkerData {
  // This class contains a list of shortId ranges collected from a single remote peer, we need to make sure all of them are sound, that is, TLV data is of same size as main data
  def isHolistic: Boolean = ranges.forall(rng =>
    rng.shortChannelIds.array.size == rng.timestamps.timestamps.size && rng.timestamps.timestamps.size == rng.checksums.checksums.size
  )
  lazy val allShortIds: Seq[Long] = ranges.flatMap(_.shortChannelIds.array)
}

case class SyncWorkerGossipData(
    syncMaster: SyncMaster,
    queries: Seq[QueryShortChannelIds],
    updates: Set[ChannelUpdate] = Set.empty,
    announces: Set[ChannelAnnouncement] = Set.empty,
    excluded: Set[UpdateCore] = Set.empty
) extends SyncWorkerData

case class CMDShortIdsComplete(sync: SyncWorker, data: SyncWorkerShortIdsData)
case class CMDChunkComplete(sync: SyncWorker, data: SyncWorkerGossipData)
case class SyncDisconnected(sync: SyncWorker, removePeer: Boolean)
case class CMDGossipComplete(sync: SyncWorker)

// This entirely relies on fact that peer sends ChannelAnnouncement messages first, then ChannelUpdate messages

case class SyncWorkerPHCData(
    phcMaster: PHCSyncMaster,
    updates: Set[ChannelUpdate],
    expectedPositions: Map[Long, PositionSet] = Map.empty,
    nodeIdToShortIds: Map[PublicKey, ShortChanIdSet] = Map.empty,
    announces: Map[Long, ChannelAnnouncement] = Map.empty
) extends SyncWorkerData {

  def withNewAnnounce(ann: ChannelAnnouncement): SyncWorkerPHCData = {
    val expectedPositions1 =
      expectedPositions.updated(ann.shortChannelId, ChannelUpdate.fullSet)
    val nodeId1ToShortIds =
      nodeIdToShortIds.getOrElse(ann.nodeId1, Set.empty) + ann.shortChannelId
    val nodeId2ToShortIds =
      nodeIdToShortIds.getOrElse(ann.nodeId2, Set.empty) + ann.shortChannelId
    val nodeIdToShortIds1 = nodeIdToShortIds
      .updated(ann.nodeId1, nodeId1ToShortIds)
      .updated(ann.nodeId2, nodeId2ToShortIds)
    copy(
      expectedPositions = expectedPositions1,
      announces = announces.updated(ann.shortChannelId, ann),
      nodeIdToShortIds = nodeIdToShortIds1
    )
  }

  def withNewUpdate(cu: ChannelUpdate): SyncWorkerPHCData = {
    val oneLessPosition =
      expectedPositions.getOrElse(cu.shortChannelId, Set.empty) - cu.position
    copy(
      expectedPositions =
        expectedPositions.updated(cu.shortChannelId, oneLessPosition),
      updates = updates + cu
    )
  }

  def isAcceptable(ann: ChannelAnnouncement): Boolean = {
    val notTooMuchNode1PHCs = nodeIdToShortIds
      .getOrElse(ann.nodeId1, Set.empty)
      .size < LNParams.syncParams.maxPHCPerNode
    val notTooMuchNode2PHCs = nodeIdToShortIds
      .getOrElse(ann.nodeId2, Set.empty)
      .size < LNParams.syncParams.maxPHCPerNode
    val isCorrect = Tools.hostedShortChanId(
      ann.nodeId1.value,
      ann.nodeId2.value
    ) == ann.shortChannelId
    ann.isPHC && isCorrect && notTooMuchNode1PHCs && notTooMuchNode2PHCs
  }

  def isUpdateAcceptable(cu: ChannelUpdate): Boolean =
    cu.htlcMaximumMsat.exists(cap =>
      cap >= LNParams.syncParams.minPHCCapacity && cap <= LNParams.syncParams.maxPHCCapacity && cap > cu.htlcMinimumMsat
    ) && // Capacity is fine
      announces
        .get(cu.shortChannelId)
        .map(_ getNodeIdSameSideAs cu)
        .exists(
          Announcements checkSig cu
        ) && // We have received a related announce, signature is valid
      expectedPositions
        .getOrElse(cu.shortChannelId, Set.empty)
        .contains(
          cu.position
        ) // Remote node must not send the same update twice
}

case class SyncWorker(
    master: CanBeRepliedTo,
    keyPair: KeyPair,
    remoteInfo: RemoteNodeInfo,
    ourInit: Init
) extends StateMachine[SyncWorkerData, SyncMaster.State] { me =>
  implicit val context: ExecutionContextExecutor =
    ExecutionContext fromExecutor Executors.newSingleThreadExecutor

  def initialState = SyncMaster.Waiting()
  val pair: KeyPairAndPubKey = KeyPairAndPubKey(keyPair, remoteInfo.nodeId)

  def supportsExtQueries(init: Init): Boolean =
    LNParams.isPeerSupports(init)(ChannelRangeQueriesExtended)

  def process(changeMessage: Any): Unit =
    scala.concurrent.Future(me doProcess changeMessage)

  val listener: ConnectionListener = new ConnectionListener {
    override def onOperational(worker: CommsTower.Worker, init: Init): Unit =
      if (me supportsExtQueries init) process(worker) else worker.disconnect()
    override def onHostedMessage(
        worker: CommsTower.Worker,
        remoteMessage: HostedChannelMessage
    ): Unit = process(remoteMessage)
    override def onMessage(
        worker: CommsTower.Worker,
        remoteMessage: LightningMessage
    ): Unit = process(remoteMessage)

    override def onDisconnect(worker: CommsTower.Worker): Unit = {
      val hasExtQueriesSupport = worker.theirInit.forall(supportsExtQueries)
      master process SyncDisconnected(me, removePeer = !hasExtQueriesSupport)
      CommsTower.listeners(worker.pair) -= listener
    }
  }

  // Note that our keyPair is always ranom here
  CommsTower.listen(Set(listener), pair, remoteInfo)

  def doProcess(change: Any): Unit = (change, data, state) match {
    case (data1: SyncWorkerPHCData, null, _: SyncMaster.Waiting) =>
      become(data1, SyncMaster.PHCSync())
    case (data1: SyncWorkerShortIdsData, null, _: SyncMaster.Waiting) =>
      become(data1, SyncMaster.ShortIDSync())
    case (
          data1: SyncWorkerGossipData,
          _,
          _: SyncMaster.Waiting | _: SyncMaster.ShortIDSync
        ) =>
      become(data1, SyncMaster.GossipSync())

    case (
          worker: CommsTower.Worker,
          syncData: SyncWorkerShortIdsData,
          _: SyncMaster.ShortIDSync
        ) =>
      val tlv = QueryChannelRangeTlv.QueryFlags(flag =
        QueryChannelRangeTlv.QueryFlags.WANT_ALL
      )
      val query = QueryChannelRange(
        LNParams.chainHash,
        syncData.from,
        tlvStream = TlvStream(tlv),
        numberOfBlocks = Int.MaxValue
      )
      worker.handler process query

    case (
          reply: ReplyChannelRange,
          syncData: SyncWorkerShortIdsData,
          _: SyncMaster.ShortIDSync
        ) =>
      val updatedData = syncData.copy(ranges = reply +: syncData.ranges)
      if (reply.syncComplete != 1)
        become(updatedData, SyncMaster.ShortIDSync())
      else master process CMDShortIdsComplete(me, updatedData)

    // _:SyncMaster.GossipSync()

    case (
          _: CommsTower.Worker,
          _: SyncWorkerGossipData,
          _: SyncMaster.GossipSync
        ) =>
      // Remote peer is connected, (re-)start remaining gossip sync
      me process CMDGetGossip

    case (
          CMDGetGossip,
          data1: SyncWorkerGossipData,
          _: SyncMaster.GossipSync
        ) =>
      if (data1.queries.isEmpty) {
        // We have no more queries left
        master process CMDGossipComplete(me)
        me process CMDShutdown
      } else {
        // Process the next batch
        val nextBatch = data1.queries.take(1)
        CommsTower.sendMany(nextBatch, pair)
      }

    case (
          update: ChannelUpdate,
          d1: SyncWorkerGossipData,
          _: SyncMaster.GossipSync
        ) if d1.syncMaster.provenButShouldBeExcluded(update) =>
      become(
        d1.copy(excluded = d1.excluded + update.core),
        SyncMaster.GossipSync()
      )
    case (
          update: ChannelUpdate,
          d1: SyncWorkerGossipData,
          _: SyncMaster.GossipSync
        ) if d1.syncMaster.provenAndNotExcluded(update.shortChannelId) =>
      become(
        d1.copy(updates = d1.updates + update.lite),
        SyncMaster.GossipSync()
      )
    case (
          ann: ChannelAnnouncement,
          d1: SyncWorkerGossipData,
          _: SyncMaster.GossipSync
        ) if d1.syncMaster.provenShortIds.contains(ann.shortChannelId) =>
      become(
        d1.copy(announces = d1.announces + ann.lite),
        SyncMaster.GossipSync()
      )
    case (
          na: NodeAnnouncement,
          d1: SyncWorkerGossipData,
          _: SyncMaster.GossipSync
        ) if Announcements.checkSig(na) =>
      d1.syncMaster.onNodeAnnouncement(na)

    case (
          _: ReplyShortChannelIdsEnd,
          data1: SyncWorkerGossipData,
          _: SyncMaster.GossipSync
        ) =>
      // We have completed current chunk, inform master and either continue or complete
      become(
        SyncWorkerGossipData(data1.syncMaster, data1.queries.tail),
        SyncMaster.GossipSync()
      )
      master process CMDChunkComplete(me, data1)
      me process CMDGetGossip

    // _:SyncMaster.PHCSync()

    case (
          worker: CommsTower.Worker,
          _: SyncWorkerPHCData,
          _: SyncMaster.PHCSync
        ) =>
      worker.handler process QueryPublicHostedChannels(LNParams.chainHash)
    case (
          ann: ChannelAnnouncement,
          d1: SyncWorkerPHCData,
          _: SyncMaster.PHCSync
        ) if d1.isAcceptable(ann) && d1.phcMaster.isAcceptable(ann) =>
      become(d1.withNewAnnounce(ann.lite), SyncMaster.PHCSync())
    case (update: ChannelUpdate, d1: SyncWorkerPHCData, _: SyncMaster.PHCSync)
        if d1.isUpdateAcceptable(update) =>
      become(d1.withNewUpdate(update.lite), SyncMaster.PHCSync())

    case (
          _: ReplyPublicHostedChannelsEnd,
          completeSyncData: SyncWorkerPHCData,
          _: SyncMaster.PHCSync
        ) =>
      // Peer has informed us that there is no more PHC gossip left, inform master and shut down
      master process completeSyncData
      me process CMDShutdown

    case (CMDShutdown, _, _) =>
      become(freshData = null, SyncMaster.ShutDown())
      CommsTower forget pair

    case _ =>
  }
}

sealed trait SyncMasterData extends { me =>
  def getNewSync(master: CanBeRepliedTo): SyncWorker = {
    // This relies on (1) baseInfos items are never getting removed
    // This relies on (2) size of baseInfos is >= LNParams.maxNodesToSyncFrom
    val unusedSyncs =
      activeSyncs.foldLeft(baseInfos ++ extInfos)(_ - _.remoteInfo)
    SyncWorker(
      master,
      randomKeyPair,
      shuffle(unusedSyncs.toList).head,
      LNParams.ourInit
    )
  }

  def withoutSync(sd: SyncDisconnected): SyncMasterData = me
    .modify(_.extInfos)
    .usingIf(sd.removePeer)(_ - sd.sync.remoteInfo)
    .modify(_.activeSyncs)
    .using(_ - sd.sync)

  def baseInfos: Set[RemoteNodeInfo]
  def extInfos: Set[RemoteNodeInfo]
  def activeSyncs: Set[SyncWorker]
}

case class PureRoutingData(
    announces: Set[ChannelAnnouncement],
    updates: Set[ChannelUpdate],
    excluded: Set[UpdateCore],
    queriesLeft: Int,
    queriesTotal: Int
)
case class SyncMasterShortIdData(
    baseInfos: Set[RemoteNodeInfo],
    extInfos: Set[RemoteNodeInfo],
    activeSyncs: Set[SyncWorker],
    ranges: Map[PublicKey, SyncWorkerShortIdsData] = Map.empty
) extends SyncMasterData

case class SyncMasterGossipData(
    baseInfos: Set[RemoteNodeInfo],
    extInfos: Set[RemoteNodeInfo],
    activeSyncs: Set[SyncWorker],
    chunksLeft: Int
) extends SyncMasterData {
  def batchQueriesLeft: Int = activeSyncs
    .map(_.data)
    .collect { case data: SyncWorkerGossipData => data.queries.size }
    .sum
}

case class UpdateConifrmState(
    liteUpdOpt: Option[ChannelUpdate],
    confirmedBy: ConfirmedBySet
) {
  def add(cu: ChannelUpdate, from: PublicKey): UpdateConifrmState =
    copy(liteUpdOpt = Some(cu), confirmedBy = confirmedBy + from)
}

abstract class SyncMaster(
    excluded: ShortChanIdSet,
    requestNodeAnnounce: ShortChanIdSet,
    routerData: Data,
    maxConnections: Int
) extends StateMachine[SyncMasterData, SyncMaster.State]
    with CanBeRepliedTo { me =>
  def initialState = SyncMaster.ShortIDSync()

  private[this] val confirmedChanUpdates = mutable.Map
    .empty[UpdateCore, UpdateConifrmState] withDefaultValue UpdateConifrmState(
    None,
    Set.empty
  )
  private[this] val confirmedChanAnnounces = mutable.Map
    .empty[ChannelAnnouncement, ConfirmedBySet] withDefaultValue Set.empty

  var newExcludedChanUpdates: Set[UpdateCore] = Set.empty
  var provenShortIds: ShortChanIdSet = Set.empty
  var totalBatchQueries: Int = 0

  def onChunkSyncComplete(pure: PureRoutingData): Unit
  def onNodeAnnouncement(na: NodeAnnouncement): Unit
  def onTotalSyncComplete(): Unit

  def hasCapacityIssues(update: ChannelUpdate): Boolean =
    update.htlcMaximumMsat.forall(cap =>
      cap < LNParams.syncParams.minCapacity || cap <= update.htlcMinimumMsat
    )
  def provenButShouldBeExcluded(update: ChannelUpdate): Boolean =
    provenShortIds.contains(update.shortChannelId) && hasCapacityIssues(update)
  def provenAndNotExcluded(shortId: Long): Boolean =
    provenShortIds.contains(shortId) && !excluded.contains(shortId)

  implicit val context: ExecutionContextExecutor =
    ExecutionContext fromExecutor Executors.newSingleThreadExecutor
  def process(changeMessage: Any): Unit =
    scala.concurrent.Future(me doProcess changeMessage)

  def doProcess(change: Any): Unit = (change, data, state) match {
    case (setupData: SyncMasterShortIdData, null, _: SyncMaster.ShortIDSync)
        if setupData.baseInfos.nonEmpty =>
      List.fill(maxConnections)(CMDAddSync).foreach(process)
      become(setupData, SyncMaster.ShortIDSync())

    case (CMDAddSync, data1: SyncMasterShortIdData, _: SyncMaster.ShortIDSync)
        if data1.activeSyncs.size < maxConnections =>
      // We are asked to create a new worker AND we don't have enough workers yet: create a new one and instruct it to sync right away

      val newSyncWorker = data.getNewSync(me)
      become(
        data1.copy(activeSyncs = data1.activeSyncs + newSyncWorker),
        SyncMaster.ShortIDSync()
      )
      newSyncWorker process SyncWorkerShortIdsData(ranges = Nil, from = 0)

    case (
          sd: SyncDisconnected,
          data1: SyncMasterShortIdData,
          _: SyncMaster.ShortIDSync
        ) =>
      become(
        data1.copy(ranges = data1.ranges - sd.sync.pair.them).withoutSync(sd),
        SyncMaster.ShortIDSync()
      )
      Rx.ioQueue.delay(5.seconds).foreach(_ => me process CMDAddSync)

    case (
          CMDShortIdsComplete(sync, ranges1),
          data1: SyncMasterShortIdData,
          _: SyncMaster.ShortIDSync
        ) =>
      val ranges2 = data1.ranges.updated(sync.pair.them, ranges1)
      val data2 = data1.copy(ranges = ranges2)
      become(data2, SyncMaster.ShortIDSync())

      if (ranges2.size == maxConnections) {
        // Collected enough channel ranges to start gossip
        val goodRanges = data2.ranges.values.filter(_.isHolistic)
        val accum = mutable.Map.empty[Long, Int].withDefaultValue(0)
        goodRanges
          .flatMap(_.allShortIds)
          .foreach(shortId => accum(shortId) += 1)
        // IMPORTANT: provenShortIds variable MUST be set BEFORE filtering out queries because `reply2Query` uses this data
        provenShortIds = accum.collect {
          case (shortId, confs)
              if confs > LNParams.syncParams.acceptThreshold =>
            shortId
        }.toSet

        val queries: Seq[QueryShortChannelIds] = goodRanges
          .maxBy(_.allShortIds.size)
          .ranges
          .flatMap(reply2Query(_))
          .toList
        val syncData = SyncMasterGossipData(
          data2.baseInfos,
          data2.extInfos,
          data2.activeSyncs,
          LNParams.syncParams.chunksToWait
        )
        totalBatchQueries = queries.size * syncData.activeSyncs.size

        become(syncData, SyncMaster.GossipSync())
        // Transfer every worker into gossip syncing state
        for (currentActiveSync <- syncData.activeSyncs)
          currentActiveSync process SyncWorkerGossipData(me, queries)
        for (currentActiveSync <- syncData.activeSyncs)
          currentActiveSync process CMDGetGossip
      }

    // _:SyncMaster.GossipSync()

    case (
          workerData: SyncWorkerGossipData,
          data1: SyncMasterGossipData,
          _: SyncMaster.GossipSync
        ) if data1.activeSyncs.size < maxConnections =>
      // Turns out one of the workers has disconnected while getting gossip, create one with unused remote nodeId and track its progress
      // Important: we retain pending queries from previous sync worker, that's why we need worker data here

      val newSyncWorker = data1.getNewSync(me)
      become(
        data1.copy(activeSyncs = data1.activeSyncs + newSyncWorker),
        SyncMaster.GossipSync()
      )
      newSyncWorker process SyncWorkerGossipData(me, workerData.queries)

    case (
          sd: SyncDisconnected,
          data1: SyncMasterGossipData,
          _: SyncMaster.GossipSync
        ) =>
      Rx.ioQueue.delay(5.seconds).foreach(_ => me process sd.sync.data)
      become(data1.withoutSync(sd), SyncMaster.GossipSync())

    case (
          CMDChunkComplete(sync, workerData),
          data1: SyncMasterGossipData,
          _: SyncMaster.GossipSync
        ) =>
      for (liteAnnounce <- workerData.announces)
        confirmedChanAnnounces(liteAnnounce) =
          confirmedChanAnnounces(liteAnnounce) + sync.pair.them
      for (liteUpdate <- workerData.updates)
        confirmedChanUpdates(liteUpdate.core) =
          confirmedChanUpdates(liteUpdate.core).add(liteUpdate, sync.pair.them)
      newExcludedChanUpdates ++= workerData.excluded

      if (data1.chunksLeft > 0) {
        // We batch multiple chunks to have less upstream db calls
        val nextData = data1.copy(chunksLeft = data1.chunksLeft - 1)
        become(nextData, SyncMaster.GossipSync())
      } else {
        val pure = getPureNormalNetworkData
        // Current batch is ready, send it out and start a new one right away
        val nextData = data1.copy(chunksLeft = LNParams.syncParams.chunksToWait)
        me onChunkSyncComplete pure.copy(queriesLeft =
          nextData.batchQueriesLeft
        )
        become(nextData, SyncMaster.GossipSync())
      }

    case (
          CMDGossipComplete(sync),
          data1: SyncMasterGossipData,
          _: SyncMaster.GossipSync
        ) =>
      val nextData = data1.copy(activeSyncs = data1.activeSyncs - sync)

      if (nextData.activeSyncs.nonEmpty) {
        become(nextData, SyncMaster.GossipSync())
      } else {
        become(null, SyncMaster.ShutDown())
        // This one will have zero queries left by default
        me onChunkSyncComplete getPureNormalNetworkData
        confirmedChanAnnounces.clear()
        confirmedChanUpdates.clear()
        onTotalSyncComplete()
      }

    case _ =>
  }

  def getPureNormalNetworkData: PureRoutingData = {
    val goodAnnounces = confirmedChanAnnounces.collect {
      case (announce, confirmedByNodes)
          if confirmedByNodes.size > LNParams.syncParams.acceptThreshold =>
        announce
    }.toSet
    val goodUpdates = confirmedChanUpdates.values.collect {
      case UpdateConifrmState(Some(update), confs)
          if confs.size > LNParams.syncParams.acceptThreshold =>
        update
    }.toSet
    val pureRoutingData = PureRoutingData(
      goodAnnounces,
      goodUpdates,
      newExcludedChanUpdates,
      queriesLeft = 0,
      queriesTotal = totalBatchQueries
    )
    // Clear up useless items AFTER we have created PureRoutingData snapshot
    for (announce <- goodAnnounces) confirmedChanAnnounces -= announce
    for (update <- goodUpdates) confirmedChanUpdates -= update.core
    newExcludedChanUpdates = Set.empty
    pureRoutingData
  }

  def reply2Query(reply: ReplyChannelRange): Iterator[QueryShortChannelIds] = {
    val stack = reply.shortChannelIds.array
      .lazyZip(reply.timestamps.timestamps)
      .lazyZip(reply.checksums.checksums)

    val shortIdFlagSeq = for {
      (shortId, theirTimestamps, theirChecksums) <- stack
      if provenAndNotExcluded(shortId)
      nodeAnnounceFlags =
        if (requestNodeAnnounce contains shortId)
          INCLUDE_NODE_ANNOUNCEMENT_1 | INCLUDE_NODE_ANNOUNCEMENT_2
        else 0
      finalFlag = computeFlag(
        shortId,
        theirTimestamps,
        theirChecksums
      ) | nodeAnnounceFlags if finalFlag != 0
    } yield (shortId, finalFlag)

    val groupedShortIdFlagSeqs =
      shortIdFlagSeq.toList.grouped(LNParams.syncParams.messagesToAsk)

    for {
      requestChunk <- groupedShortIdFlagSeqs
      (chunkShortIds, chunkRequestFlags) = requestChunk.unzip
      sids = EncodedShortChannelIds(
        reply.shortChannelIds.encoding,
        chunkShortIds
      )
      tlv = QueryShortChannelIdsTlv.EncodedQueryFlags(
        reply.shortChannelIds.encoding,
        chunkRequestFlags
      )
    } yield QueryShortChannelIds(
      LNParams.chainHash,
      tlvStream = TlvStream(tlv),
      shortChannelIds = sids
    )
  }

  private def computeFlag(
      shortlId: Long,
      theirTimestamps: ReplyChannelRangeTlv.Timestamps,
      theirChecksums: ReplyChannelRangeTlv.Checksums
  ) =
    if (routerData.channels contains shortlId) {
      val (stamps, checksums) =
        Sync.getChannelDigestInfo(routerData.channels)(shortlId)
      val shouldRequestUpdate1 = Sync.shouldRequestUpdate(
        stamps.timestamp1,
        checksums.checksum1,
        theirTimestamps.timestamp1,
        theirChecksums.checksum1
      )
      val shouldRequestUpdate2 = Sync.shouldRequestUpdate(
        stamps.timestamp2,
        checksums.checksum2,
        theirTimestamps.timestamp2,
        theirChecksums.checksum2
      )

      val flagUpdate1 =
        if (shouldRequestUpdate1) INCLUDE_CHANNEL_UPDATE_1 else 0
      val flagUpdate2 =
        if (shouldRequestUpdate2) INCLUDE_CHANNEL_UPDATE_2 else 0
      0 | flagUpdate1 | flagUpdate2
    } else {
      INCLUDE_CHANNEL_ANNOUNCEMENT | INCLUDE_CHANNEL_UPDATE_1 | INCLUDE_CHANNEL_UPDATE_2
    }
}

case class CompleteHostedRoutingData(
    announces: Set[ChannelAnnouncement],
    updates: Set[ChannelUpdate] = Set.empty
)
case class SyncMasterPHCData(
    baseInfos: Set[RemoteNodeInfo],
    extInfos: Set[RemoteNodeInfo],
    activeSyncs: Set[SyncWorker],
    attemptsLeft: Int = 12
) extends SyncMasterData

abstract class PHCSyncMaster(routerData: Data)
    extends StateMachine[SyncMasterData, SyncMaster.State]
    with CanBeRepliedTo { me =>
  implicit val context: ExecutionContextExecutor =
    ExecutionContext fromExecutor Executors.newSingleThreadExecutor
  def initialState = SyncMaster.PHCSync()

  def process(changeMessage: Any): Unit =
    scala.concurrent.Future(me doProcess changeMessage)

  // These checks require graph
  def isAcceptable(ann: ChannelAnnouncement): Boolean = {
    val node1HasEnoughIncomingChans = routerData.graph.vertices
      .getOrElse(ann.nodeId1, Nil)
      .size >= LNParams.syncParams.minNormalChansForPHC
    val node2HasEnoughIncomingChans = routerData.graph.vertices
      .getOrElse(ann.nodeId2, Nil)
      .size >= LNParams.syncParams.minNormalChansForPHC
    node1HasEnoughIncomingChans && node2HasEnoughIncomingChans
  }

  def onSyncComplete(pure: CompleteHostedRoutingData): Unit

  def doProcess(change: Any): Unit = (change, data, state) match {
    case (setupData: SyncMasterPHCData, null, _: SyncMaster.PHCSync)
        if setupData.baseInfos.nonEmpty =>
      become(freshData = setupData, SyncMaster.PHCSync())
      me process CMDAddSync

    case (CMDAddSync, data1: SyncMasterPHCData, _: SyncMaster.PHCSync)
        if data1.activeSyncs.isEmpty =>
      // We are asked to create a new worker AND we don't have a worker yet: create one
      // for now PHC sync happens with a single remote peer

      val newSyncWorker = data1.getNewSync(me)
      become(
        data1.copy(activeSyncs = data1.activeSyncs + newSyncWorker),
        SyncMaster.PHCSync()
      )
      newSyncWorker process SyncWorkerPHCData(me, updates = Set.empty)

    case (sd: SyncDisconnected, data1: SyncMasterPHCData, _: SyncMaster.PHCSync)
        if data1.attemptsLeft > 0 =>
      become(
        data1.copy(attemptsLeft = data1.attemptsLeft - 1).withoutSync(sd),
        SyncMaster.PHCSync()
      )
      Rx.ioQueue.delay(5.seconds).foreach(_ => me process CMDAddSync)

    case (_: SyncWorker, _, _: SyncMaster.PHCSync) =>
      // No more reconnection attempts left
      become(null, SyncMaster.ShutDown())

    case (d1: SyncWorkerPHCData, _, _: SyncMaster.PHCSync) =>
      // Worker has informed us that PHC sync is complete, shut everything down
      me onSyncComplete CompleteHostedRoutingData(
        d1.announces.values.toSet,
        d1.updates
      )
      become(null, SyncMaster.ShutDown())

    case _ =>
  }
}
