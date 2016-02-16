package poker

case class Card(value: Value, suit: Suit) {
  override def toString: String = s"${value.value}${suit.suit}"

  val weight = value.weight * 10 + suit.weight

  def >(card: Card) = weight > card.weight
}

sealed abstract class Suit(val suit: String, val weight: Int) {
  def >(suit: Suit): Boolean = this.weight > suit.weight
}

object Suit {

  object Club extends Suit("C", 1)
  object Diamond extends Suit("D", 2)
  object Heart extends Suit("H", 3)
  object Spade extends Suit("S", 4)

  val suits = Map(
    "C" -> Club,
    "D" -> Diamond,
    "H" -> Heart,
    "S" -> Spade
  )

  def apply(suit: String): Suit =  suits.getOrElse(suit, throw new IllegalStateException())

  implicit val suiteOrder = new Ordering[Suit] {
    override def compare(x: Suit, y: Suit): Int = x.weight - y.weight
  }
}

sealed abstract class Value(val value: String, val weight: Int) {
  def >(value: Value): Boolean = this.weight > value.weight
}

object Value {

  object Two extends Value("2", 2)
  object Three extends Value("3", 3)
  object Four extends Value("4", 4)
  object Five extends Value("5", 5)
  object Six extends Value("6", 6)
  object Seven extends Value("7", 7)
  object Eight extends Value("8", 8)
  object Nine extends Value("9", 9)
  object Jack extends Value("J", 10)
  object Queen extends Value("Q", 11)
  object King extends Value("K", 12)
  object Ace extends Value("A", 13)

  val values = Map(
    "2" -> Two,
    "3" -> Three,
    "4" -> Four,
    "5" -> Five,
    "6" -> Six,
    "7" -> Seven,
    "8" -> Eight,
    "9" -> Nine,
    "J" -> Jack,
    "Q" -> Queen,
    "K" -> King,
    "A" -> Ace
  )

  def apply(value: String): Value = values.getOrElse(value, throw new IllegalStateException())

  implicit val valueOrder = new Ordering[Value] {
    override def compare(x: Value, y: Value): Int = x.weight - y.weight
  }
}


object Card {
  def apply(card: String): Card = Card(Value(card(0).toString), Suit(card(1).toString))

  implicit val cardOrder = new Ordering[Card] {
    override def compare(x: Card, y: Card): Int =
      if (x.value != y.value)
        x.value.weight - y.value.weight
      else
        x.suit.weight - y.suit.weight
  }
}
