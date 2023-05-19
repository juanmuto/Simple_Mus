/*
case class MusValue(value1: Int, value2: Int... )
case class Card(... value: MusValue)

case class MusValue(ranking:Int, value1: Int, value2: Int... )


final case class Mano(cartas: List[Cart])

def repartir(jugadores: Int) : List[Manos]

inicializar
repartir
comparar
*/
// case class MusResultado(grande: Int, chica: int. par: int, juego: int)

import scala.language.implicitConversions
import scala.util.Random

case class Rank(value: Int) extends AnyVal
object Rank {
  implicit def fromString(s: String): Rank = Rank(s.toInt)
}
final case class Card(rank: Rank, suit: Suit, valor: Int, juego: Int)
case class musValue(value: Int) {
  lazy val Valor: Int = value match {
    case (1) => 1
    case (2) => 1
    case (3) => 8
    case (4) => 2
    case (5) => 3
    case (6) => 4
    case (7) => 5
    case (10) => 6
    case (11) => 7
    case (12) => 8
    case _ => 0
  }
  lazy val Juego: Int = value match {
    case (1) => 1
    case (2) => 1
    case (3) => 10
    case (4) => 4
    case (5) => 5
    case (6) => 6
    case (7) => 7
    case (10) => 10
    case (11) => 10
    case (12) => 10
  }
}

case class Suit(name: String, shortName: String)
object Suit {
  object Espadas extends Suit("Espadas", "e")

  object Copas extends Suit("Copas", "c")

  object Oros extends Suit("Oros", "o")

  object Bastos extends Suit("Bastos", "b")

  val all = List(Espadas, Copas, Oros, Bastos)
}
class Deck {
    val cards = collection.mutable.ListBuffer() ++ Random.shuffle(initDeck)

    def dealCard: Option[Card] = cards.isEmpty match {
      case true => None
      case false =>
        val card = cards.head
        cards -= card
        Some(card)
    }

    def initDeck =
      for {
        suit <- Suit.all
        rank <- (1 to 7) ++ (10 to 12)
      }
      yield new Card(Rank(rank), suit, musValue(rank).Juego, musValue(rank).Valor)

  }


object Main extends App {
  val deck1 = new Deck
  println(deck1.cards)
  val card1 = deck1.dealCard
  println(deck1.cards)
  println(card1(1))
}