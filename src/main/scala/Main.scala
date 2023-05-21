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

final case class Card(rank: Rank, suit: Suit, valor: Int, juego: Int)
object Card {
    implicit val valorOrdering: Ordering[Card] = Ordering.by(_.valor)

}
class Deck {
    val cards = collection.mutable.ListBuffer() ++ Random.shuffle(initDeck)

  //TODO check whole dealing process
    def dealCard: Option[Card] = cards.isEmpty match {
      case true => None
      case false =>
        val card = cards.head
        cards -= card
        Some(card)
    }
    def dealCards(players: List[Player]): List[Player] = {
      val updatedPlayers = (1 to 4).foldLeft(players) { (currentPlayers, _) =>
        currentPlayers.map { player =>
          dealCard match {
            case Some(card) => player.copy(cards = card :: player.cards)
            case None => player // Handle the case where no more cards are available
          }
        }
      }
      updatedPlayers
    }
    def initDeck =
      for {
        suit <- Suit.all
        rank <- (1 to 7) ++ (10 to 12)
      }
      yield new Card(Rank(rank), suit, musValue(rank).Valor, musValue(rank).Juego)

  }
case class Player(name: String, cards: List[Card], mano: Int)
  //Todo: Randomise mano or cycle
object Player {
    val player1 = Player("Player 1", Nil, 1)
    val player2 = Player("Player 2", Nil, 2)
    val player3 = Player("Player 3", Nil, 3)
    val player4 = Player("Player 4", Nil, 4)

    val all = List(player1, player2, player3, player4)
  }


object initialiseGame extends App {
  val deck = new Deck
  val dealtPlayers = deck.dealCards(Player.all)
  println(dealtPlayers)
  //TODO Provide text description of each player's hand
  //TODO assign Mano Property to one player
  //Grande winner:

  val maxValor = dealtPlayers.map(_.cards.maxBy(_.valor).valor).max
  val winners = dealtPlayers.filter(_.cards.exists(_.valor == maxValor))

  if (winners.length == 1) {
    val winner = winners.head
    println(s"The winner of Grande is ${winner.name} with the highest valor card: ${winner.cards.maxBy(_.valor)}")
  } else {
    val remainingCards = dealtPlayers.flatMap(_.cards).filter(_.valor < maxValor)
    val currentMaxValor = remainingCards.map(_.valor).max
    val remainingWinners = dealtPlayers.filter(p => p.cards.exists(c => c.valor == currentMaxValor))

    if (remainingWinners.length == 1) {
      val winner = remainingWinners.head
      println(s"The winner of Grande is ${winner.name} with the highest valor card: ${winner.cards.maxBy(_.valor)}")
    } else {
      val winner = remainingWinners.minBy(_.mano)
      println(s"The winner of Grande is ${winner.name} with the lowest mano value: ${winner.mano}")
    }
  }
}




