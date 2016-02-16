import org.scalatest._
import poker._
import PokerHand._

class PokerSpec extends FlatSpec with Matchers{

  "Poker hands" should "compare correctly" in {
    PokerHand("3H 6H 7H 8H 9H")
  }

  it should "compare cards correctly" in {
    Card("3H") should be > Card("2H")
    Card("3S") should be > Card("3H")
    Card("2S") should be < Card("3H")
  }

  it should "identify a flush correctly and compare against flushes correctly" in {
    PokerHand("3H 6H 7H 8H 9H") shouldBe a[Flush]
    PokerHand("3H 6H 7H 8H 2H") should be > PokerHand("3D 6D 7D 8D 9D")
    PokerHand("3H 6H 7H 8H 2H") should be < PokerHand("4H 5H AH 9H KH")
  }

  it should "identify a straight correctly" in {
    PokerHand("3H 4D 5H 6S 7H") shouldBe a[Straight]
    PokerHand("3H 4D 5H 6S 7H") should be > PokerHand("2S 3H 4D 5H 6S")
    PokerHand("3H 4D 5H 6S 7H") should be < PokerHand("4D 5H 6S 7H 8H")
  }

  it should "identify a straight flush correctly" in {
    PokerHand("3H 4H 5H 6H 7H") shouldBe a[StraightFlush]
    PokerHand("3H 4D 5H 6S 7H") should be > PokerHand("2H 3H 4D 5H 6S")
    PokerHand("3H 4D 5H 6S 7H") should be < PokerHand("4D 5H 6S 7H 8H")
  }

  it should "identify 4 of a kind" in {
    PokerHand("QH QS QD QC KC") shouldBe a[FourOfAKind]
    PokerHand("QH QS QD QC KC") should be > PokerHand("JH JS JD JC KC")
  }

  it should "identify a full house" in {
    PokerHand("QH QS QD KH KC") shouldBe a[FullHouse]
    PokerHand("QH QS QD KH KC") shouldBe a[FullHouse]
  }

  it should "identify 3 of a kind" in {
    PokerHand("QH QS QD JC KC") shouldBe a[ThreeOfAKind]
  }
  
  it should "identify 2 pairs" in {
    PokerHand("QH QS JD JC KC") shouldBe a[TwoPairs]
  }

  it should "identify a one pair" in {
    PokerHand("QH QS JD 7C KC") shouldBe a[OnePair]
  }

  it should "identify a high card" in {
    PokerHand("QH AS JD 7C KC") shouldBe a[HighCard]
  }
}
