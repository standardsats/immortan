package testrunner

import utest._

import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext}

object UTestRunner {

  val suites: Seq[(String, TestSuite)] = Seq(
    "Bech32Spec"                    -> fr.acinq.bitcoin.Bech32Spec,
    "PsbtSpec"                      -> fr.acinq.bitcoin.PsbtSpec,
    "TransactionsSpec"              -> fr.acinq.eclair.transactions.TransactionsSpec,
    "SphinxSpec"                    -> fr.acinq.eclair.crypto.SphinxSpec,
    "GraphSpec"                     -> immortan.GraphSpec,
    "DbSpec"                        -> immortan.DbSpec,
    "PathfinderSpec"                -> immortan.PathfinderSpec,
    "InputParserSpec"               -> immortan.InputParserSpec,
    "PaymentTrampolineRoutingSpec"  -> immortan.PaymentTrampolineRoutingSpec,
    "PaymentIncomingFinalSpec"      -> immortan.PaymentIncomingFinalSpec,
    "TrampolineFeerateSpec"         -> immortan.TrampolineFeerateSpec,
    "StateMachineSpec"              -> immortan.StateMachineSpec,
    "FeeRatesSpec"                  -> immortan.FeeRatesSpec,
    "MPPSpec"                       -> immortan.MPPSpec,
    "WireSpec"                      -> immortan.WireSpec,
    "TrampolineBroadcasterSpec"     -> immortan.TrampolineBroadcasterSpec,
    "CommsTowerSpec"                -> immortan.CommsTowerSpec
  )

  def main(args: Array[String]): Unit = {
    implicit val ec: ExecutionContext = ExecutionContext.global
    val failures = new AtomicInteger(0)

    for ((name, suite) <- suites) {
      println(s"\n=== $name ===")
      val f = TestRunner.runAsync(suite.tests, (path, result) => {
        result.value match {
          case scala.util.Success(_) =>
            println(s"  + ${path.mkString(".")}")
          case scala.util.Failure(ex) =>
            println(s"  - ${path.mkString(".")}: ${ex.getClass.getSimpleName}: ${ex.getMessage}")
            failures.incrementAndGet()
        }
      })
      Await.result(f, Duration.Inf)
    }

    println()
    val total = failures.get()
    if (total > 0) {
      println(s"$total test(s) FAILED")
      System.exit(total)
    } else {
      println("All tests passed.")
      System.exit(0)
    }
  }
}
