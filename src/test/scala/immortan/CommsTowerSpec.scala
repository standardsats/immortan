package immortan

import java.net.{InetSocketAddress, Socket}
import fr.acinq.bitcoin.Block
import fr.acinq.eclair.wire.{Init, LightningMessage, Pong}
import immortan.crypto.Tools
import immortan.utils.TestUtils._
import utest._

object CommsTowerSpec extends TestSuite {
  val tests = Tests {
    test("Successfully connect, send Ping, get Pong") {
      val remoteInfo = (new SyncParams).acinq
      if (!isReachable(remoteInfo.address.socketAddress.getHostString, remoteInfo.address.socketAddress.getPort)) {
        println("  [SKIP] ACINQ node unreachable — no network")
      } else {
        var responses = List.empty[LightningMessage]
        LNParams.connectionProvider = new RequestsConnectionProvider
        LNParams.chainHash = Block.LivenetGenesisBlock.hash
        LNParams.ourInit = LNParams.createInit

        val listener1 = new ConnectionListener {
          override def onOperational(
              worker: CommsTower.Worker,
              theirInit: Init
          ): Unit = worker.sendPing()
          override def onMessage(
              worker: CommsTower.Worker,
              msg: LightningMessage
          ): Unit = responses ::= msg
        }

        val kpap1 = KeyPairAndPubKey(Tools.randomKeyPair, remoteInfo.nodeId)
        CommsTower.listen(Set(listener1), kpap1, remoteInfo)

        // We have connected, sent Ping, got Pong
        WAIT_UNTIL_TRUE(responses.headOption.exists(_.isInstanceOf[Pong]))

        //

        val listener2 = new ConnectionListener {
          override def onOperational(
              worker: CommsTower.Worker,
              theirInit: Init
          ): Unit = responses ::= theirInit
          override def onMessage(
              worker: CommsTower.Worker,
              msg: LightningMessage
          ): Unit = responses ::= msg
        }

        // Remote node is already connected with this local data
        CommsTower.listen(Set(listener2), kpap1, remoteInfo)

        // Only listener2.onOperational was called
        WAIT_UNTIL_TRUE(responses.headOption.exists(_.isInstanceOf[Init]))
        WAIT_UNTIL_TRUE(responses.lift(1).exists(_.isInstanceOf[Pong]))
        WAIT_UNTIL_TRUE(responses.size == 2)

        //

        val listener3 = new ConnectionListener {
          override def onOperational(
              worker: CommsTower.Worker,
              theirInit: Init
          ): Unit = worker.sendPing()
          override def onMessage(
              worker: CommsTower.Worker,
              msg: LightningMessage
          ): Unit = responses ::= msg
        }

        // We connect as another local node to the same remote node (two socket connections)
        val kpap2 = KeyPairAndPubKey(Tools.randomKeyPair, remoteInfo.nodeId)
        CommsTower.listen(Set(listener3), kpap2, remoteInfo)

        // Only listener3.onOperational was called
        WAIT_UNTIL_TRUE(responses.size == 3)
      }
    }
  }
}
