package poker

import PokerHand._

sealed abstract class PokerHand(val cards: List[Card], val weight: Int) {
  val sortedCards = cards.sorted
  val sortedSuit = suitSet(cards).sorted
  val highestCard = sortedCards.head
  val highestSuit = sortedSuit.head
  val sortedCardGroupByGroupSize = groupByValue(cards).toSeq.sortWith((x, y) => x._2.size > y._2.size && x._1 > y._1)
  val highestCardGroup = sortedCardGroupByGroupSize.head._1

  def >(hand: PokerHand): Boolean = weight >= hand.weight && classWeight > hand.classWeight

  def classWeight: Int
}

case class StraightFlush(override val cards: List[Card]) extends PokerHand(cards, 9) {
  override def classWeight: Int = highestSuit.weight * 100 + highestCard.value.weight
}

case class FourOfAKind(override val cards: List[Card]) extends PokerHand(cards, 8) {
  override def classWeight: Int = highestSuit.weight
}

case class FullHouse(override val cards: List[Card]) extends PokerHand(cards, 7) {
  override def classWeight: Int = highestSuit.weight
}

case class Flush(override val cards: List[Card]) extends PokerHand(cards, 6) {
  override def classWeight: Int = highestSuit.weight * 100 + highestCard.value.weight
}

case class Straight(override val cards: List[Card]) extends PokerHand(cards, 5) {
  override def classWeight: Int = highestCard.weight
}

case class ThreeOfAKind(override val cards: List[Card]) extends PokerHand(cards, 4) {
  override def classWeight: Int = highestSuit.weight
}

case class TwoPairs(override val cards: List[Card]) extends PokerHand(cards, 3) {
  override def classWeight: Int = highestCard.value.weight
}

case class OnePair(override val cards: List[Card]) extends PokerHand(cards, 2) {
  override def classWeight: Int = highestCard.value.weight
}

case class HighCard(override val cards: List[Card]) extends PokerHand(cards, 1) {
  override def classWeight: Int = highestCard.value.weight
}

object PokerHand {

  def apply(hand: String): PokerHand = getCards(hand) match {
    case cards if isFlush(cards) && isStraight(cards) => StraightFlush(cards)
    case cards if isFlush(cards) => Flush(cards)
    case cards if isStraight(cards) => Straight(cards)
    case cards => (valueSet(cards).size, maxSets(cards)) match {
      case (2, maxSet) => maxSet match {
        case 4 => FourOfAKind(cards)
        case 3 => FullHouse(cards)
      }
      case (3, maxSet) => maxSet match {
        case 3 => ThreeOfAKind(cards)
        case 2 => TwoPairs(cards)
      }
      case (4, _) => OnePair(cards)
      case _ => HighCard(cards)
    }
  }

  def valueSet(cards: List[Card]): List[Value] = cards.map(c => c.value).distinct.sorted

  def maxSets(cards: List[Card]): Int =
    groupByValue(cards).map(l => l._2.size).max

  def groupByValue(cards: List[Card]): Map[Value, List[Card]] =
    cards.groupBy(c => c.value)

  def suitSet(cards: List[Card]): List[Suit] = cards.map(c => c.suit).distinct.sorted

  def getCards(hand: String): List[Card] = hand.split(" ").map(card => Card(card)).toList

  def isFlush(cards: List[Card]): Boolean = suitSet(cards).size == 1

  def isStraight(cards: List[Card]): Boolean =
    straightValue(cards) == 0 && valueSet(cards).size == 5

  def straightValue(cards: List[Card]): Int =
    cards.map(c => c.value.weight).sum % 5

  implicit val pokerHandOrdering = new Ordering[PokerHand] {
    override def compare(x: PokerHand, y: PokerHand): Int =
      ((x.weight - y.weight) * 100) + (x.classWeight - y.classWeight)
  }
}
