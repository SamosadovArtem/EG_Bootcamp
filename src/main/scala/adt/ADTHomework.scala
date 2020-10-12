package adt

import adt.ADTHomework.Rank.Rank
import adt.ADTHomework.Suit.Suit

import scala.annotation.tailrec
import scala.util.Random

object Boot extends App {
  import ADTHomework._

  val johnDoe = TexasPlayer(
    TexasPlayerHand(
      Card(Rank.Jack, Suit.Club), Card(Rank.Jack, Suit.Diamond)
    ),
    "John Doe"
  )

  val johnSmith = TexasPlayer(
    TexasPlayerHand(
      Card(Rank.Ace, Suit.Spade), Card(Rank.Two, Suit.Club)
    ),
    "John Smith"
  )

  val table = HoldemTableCards(
    Card(Rank.King, Suit.Spade),
    Card(Rank.Nine, Suit.Heart),
    Card(Rank.Ten, Suit.Diamond),
    Card(Rank.Queen, Suit.Diamond),
    Card(Rank.Eight, Suit.Diamond)
  )

  val board = Board.createTexas(Seq(johnDoe, johnSmith), table)
  println(board.getWinners.map(_.name))

  println("Random game")
  val deck = Deck()
  val firstRandomCard = deck.getRandomCard
  val secondRandomCard = deck.getRandomCard
  val johnDoeRandom = TexasPlayer(
    TexasPlayerHand(
      firstRandomCard, secondRandomCard
    ),
    "John Doe"
  )
  println(s"${johnDoeRandom.name} player has two cards: $firstRandomCard and $secondRandomCard")


  val thirdRandomCard = deck.getRandomCard
  val fourthRandomCard = deck.getRandomCard
  val johnSmithRandom = TexasPlayer(
    TexasPlayerHand(
      thirdRandomCard, fourthRandomCard
    ),
    "John Smith"
  )
  println(s"${johnSmithRandom.name} player has two cards: $thirdRandomCard and $fourthRandomCard")

  val tableRandom = HoldemTableCards(
    deck.getRandomCard,
    deck.getRandomCard,
    deck.getRandomCard,
    deck.getRandomCard,
    deck.getRandomCard
  )

  println(s"Cards on the table: $tableRandom")

  val boardRandom = Board.createTexas(Seq(johnDoeRandom, johnSmithRandom), tableRandom)
  println(s"Winners are ${boardRandom.getWinners.map(_.name)}")

}

object ADTHomework {

  object Suit {
    sealed trait Suit
    case object Heart extends Suit
    case object Diamond extends Suit
    case object Spade extends Suit
    case object Club extends Suit
  }

  object Rank {
    sealed trait Rank {
      def power: Int
    }
    case object Two extends Rank {
      override def power: Int = 1
    }
    case object Three extends Rank {
      override def power: Int = 2
    }
    case object Four extends Rank {
      override def power: Int = 3
    }
    case object Five extends Rank {
      override def power: Int = 4
    }
    case object Six extends Rank {
      override def power: Int = 5
    }
    case object Seven extends Rank {
      override def power: Int = 6
    }
    case object Eight extends Rank {
      override def power: Int = 7
    }
    case object Nine extends Rank {
      override def power: Int = 8
    }
    case object Ten extends Rank {
      override def power: Int = 9
    }
    case object Jack extends Rank {
      override def power: Int = 10
    }
    case object Queen extends Rank {
      override def power: Int = 11
    }
    case object King extends Rank {
      override def power: Int = 12
    }
    case object Ace extends Rank {
      override def power: Int = 13
    }
  }


  final case class Card(rank: Rank, suit: Suit)

  final case class Deck() {
    private var missedCards: Seq[Card] = Seq()

    @tailrec
    def getRandomCard: Card = {
      val rank = getRandomRank
      val suit = getRandomSuit
      val card = Card(rank, suit)
      if (isCardInDeck(card)) {
        getRandomCard
      } else {
        missedCards  = missedCards :+ card
        card
      }
    }
    def isCardInDeck(card: Card): Boolean = missedCards.contains(card)

    def getRandomRank: Rank = Random.between(1, 14) match {
      case 1  => Rank.Two
      case 2  => Rank.Three
      case 3  => Rank.Four
      case 4  => Rank.Five
      case 5  => Rank.Six
      case 6  => Rank.Seven
      case 7  => Rank.Eight
      case 8  => Rank.Nine
      case 9  => Rank.Ten
      case 10 => Rank.Jack
      case 11 => Rank.Queen
      case 12 => Rank.King
      case _  => Rank.Ace
    }

    def getRandomSuit: Suit = Random.between(1, 5) match {
      case 1 => Suit.Diamond
      case 2 => Suit.Heart
      case 3 => Suit.Club
      case _ => Suit.Spade
    }
  }

  sealed trait Holdem
  final case class Omaha() extends Holdem
  final case class Texas() extends Holdem

  sealed trait TableCards {
    def cards: Seq[Card]
  }

  final case class HoldemTableCards(first: Card, second: Card, third: Card, fourth: Card, fifth: Card) extends TableCards {
    override def cards: Seq[Card] = Seq(first, second, third, fourth, fifth)
  }

  sealed trait PlayerHand {
    def cards: Seq[Card]
  }

  final case class TexasPlayerHand(first: Card, second: Card) extends PlayerHand {
    override def cards: Seq[Card] = Seq(first, second)
  }
  final case class OmahaPlayerHand(first: Card, second: Card, third: Card, fourth: Card) extends PlayerHand {
    override def cards: Seq[Card] = Seq(first, second, third, fourth)
  }

  sealed trait Player {
    def name: String
    def hand: PlayerHand
  }

  case class TexasPlayer(playerHand: TexasPlayerHand, name: String) extends Player {
    override def hand: PlayerHand = playerHand
  }

  case class OmahaPlayer(playerHand: OmahaPlayerHand, name: String) extends Player {
    override def hand: PlayerHand = playerHand
  }


  sealed trait Combination {
    def power: Int
  }

  case class HighCard(card:Card) extends Combination {
    override def power: Int = card.rank.power
  }
  object Pair extends Combination {
    override def power: Int = 14
  }
  object TwoPair extends Combination {
    override def power: Int = 15
  }
  object ThreeOfAKing extends Combination {
    override def power: Int = 16
  }
  object Straight extends Combination {
    override def power: Int = 17
  }
  object Flush extends Combination {
    override def power: Int = 18
  }
  object FullHouse extends Combination {
    override def power: Int = 19
  }
  object StraightFlush extends Combination {
    override def power: Int = 20
  }
  object RoyalFlush extends Combination {
    override def power: Int = 21
  }

  object Service {
    //I've implemented two combinations just for test purposes
    def getCombination(player: Player, tableCards: TableCards): Combination = player.hand.cards ++ tableCards.cards match {
      case cards if cards.distinctBy(_.suit).length == 1 => Flush
      case _ =>
        val power = (player.hand.cards ++ tableCards.cards).maxBy(_.rank.power)
        HighCard(power)
    }

    def getWinners(players: Seq[Player], tableCards: TableCards): Seq[Player] = {
      val (_, winnersWithScore) = players
        .map(player => (player, getCombination(player, tableCards).power))
        .groupBy {
          case (_, score) => score
        }.maxBy {
        case (score, _) => score
      }

      winnersWithScore.map {
        case (winner, _) => winner
      }
    }
  }

  sealed trait Board {
    def players: Seq[Player]
    def tableCards: TableCards
    def getWinners: Seq[Player]
  }
  object Board {
    def createOmaha(oPlayers: Seq[OmahaPlayer], table: TableCards): Board = new Board {
      override def players: Seq[Player] = oPlayers
      override def tableCards: TableCards = table
      override def getWinners: Seq[Player] = Service.getWinners(oPlayers, table)
    }

    def createTexas(tPlayers: Seq[TexasPlayer], table: TableCards): Board = new Board {
      override def players: Seq[Player] = tPlayers
      override def tableCards: TableCards = table
      override def getWinners: Seq[Player] = Service.getWinners(tPlayers, table)
    }
  }

}
