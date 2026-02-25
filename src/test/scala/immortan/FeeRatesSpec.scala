package immortan

import fr.acinq.bitcoin._
import fr.acinq.eclair.blockchain.fee.{
  FeeratePerKB,
  FeeratePerKw,
  FeeratesPerKB
}
import immortan.utils.FeeRates._
import immortan.utils.{BitGoFeeRateStructure, BitgoFeeProvider, EsploraFeeProvider}
import utest._

object FeeRatesSpec extends TestSuite {
  val tests = Tests {
    test("Provider structures are correctly parsed") {
      val esplora = new EsploraFeeProvider("unused")
      val esploraStructure: esplora.EsploraFeeStructure = Map(
        "1" -> BigDecimal("1.014"),
        "2" -> BigDecimal("1.000"),
        "6" -> BigDecimal("0.900"),
        "1008" -> BigDecimal("0.723")
      )
      assert(esplora.extractFeerate(esploraStructure, 1) == FeeratePerKB(1014.sat))
      assert(esplora.extractFeerate(esploraStructure, 6) == FeeratePerKB(900.sat))
      assert(
        esplora.extractFeerate(esploraStructure, 1008) == FeeratePerKB(723.sat)
      )

      val bitgoStructure = BitGoFeeRateStructure(
        feeByBlockTarget =
          Map("1" -> 1507L, "2" -> 1300L, "6" -> 1100L, "144" -> 1000L),
        feePerKb = 1507L
      )
      assert(BitgoFeeProvider.extractFeerate(bitgoStructure, 1) == FeeratePerKB(1507.sat))
      assert(BitgoFeeProvider.extractFeerate(bitgoStructure, 6) == FeeratePerKB(1100.sat))
      assert(
        BitgoFeeProvider.extractFeerate(bitgoStructure, 144) == FeeratePerKB(1000.sat)
      )
    }

    test("Feerates are correctly smoothed") {
      val fr1 = FeeratesPerKB(
        mempoolMinFee = FeeratePerKB(50000.sat),
        block_1 = FeeratePerKB(2100000.sat),
        blocks_2 = FeeratePerKB(1800000.sat),
        blocks_6 = FeeratePerKB(1500000.sat),
        blocks_12 = FeeratePerKB(1100000.sat),
        blocks_36 = FeeratePerKB(500000.sat),
        blocks_72 = FeeratePerKB(200000.sat),
        blocks_144 = FeeratePerKB(150000.sat),
        blocks_1008 = FeeratePerKB(50000.sat)
      )

      val fr2 = FeeratesPerKB(
        mempoolMinFee = FeeratePerKB(500000.sat),
        block_1 = FeeratePerKB(21000000.sat),
        blocks_2 = FeeratePerKB(18000000.sat),
        blocks_6 = FeeratePerKB(15000000.sat),
        blocks_12 = FeeratePerKB(11000000.sat),
        blocks_36 = FeeratePerKB(5000000.sat),
        blocks_72 = FeeratePerKB(2000000.sat),
        blocks_144 = FeeratePerKB(1500000.sat),
        blocks_1008 = FeeratePerKB(500000.sat)
      )

      val history = List(fr1, fr2)
      val smoothed = smoothedFeeratesPerKw(history)
      assert(smoothed.blocks_72 == FeeratePerKw(275000.sat))
    }
  }
}
