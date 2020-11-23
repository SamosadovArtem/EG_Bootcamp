package http

import java.util.UUID

import cats.effect.{Blocker, ExitCode, IO, IOApp}
import cats.implicits.catsSyntaxFlatMapOps
import http.DTOs.{GameAttributes, GameDto}
import io.circe.generic.auto._
import org.http4s._
import org.http4s.circe.CirceEntityCodec._
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder

import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.util.Random

object Consts {
  val ClientIdCookieName = "clientId"
  case class ClientId(value: String) extends AnyVal
  case class GameData(rightAnswer: Int, attempts: Int)
  val Port = 8080
  def printLine(string: String = ""): IO[Unit] = IO(println(string))
}


object DTOs {

  case class GameAttributes(current: Int)
  case class GameDto(
                      `type`: String = "game-dto",
                      attributes: GameAttributes
                    )

  case class GameResultAttributes(result: String)
  case class GameResultDto(
                            `type`: String = "game-result",
                            attributes: GameResultAttributes
                          )

  case class ErrorAttributes(errorCode: String, reason: String)
  sealed trait ErrorDto {val attributes: ErrorAttributes}
  case class ForbiddenErrorDto(attributes: ErrorAttributes =
                               ErrorAttributes(
                                 errorCode = "USER_NOT_REGISTERED",
                                 reason = "User have to create game before playing"
                               )
                              ) extends ErrorDto

  case class GameStartAttributes(message: String = "Game Successfully Started")
  case class GameStarDto(`type`: String = "game-start", attributes: GameStartAttributes = GameStartAttributes())

}

object Matchers {
  object MinMatcher extends QueryParamDecoderMatcher[Int](name = "min")
  object MaxMatcher extends QueryParamDecoderMatcher[Int](name = "max")
  object AttemptsMatcher extends QueryParamDecoderMatcher[Int](name = "attempts")
}

object Server {
  import Consts._
  import DTOs._
  import Matchers._

  sealed trait GameResult
  case object GameSuccess extends GameResult
  case object GameWrongResult extends GameResult
  case object GameOver extends GameResult

  //mutable.Map here is unsafe from the multithread perspective, but just for test purposes
  def gameRoutes(clientCache: mutable.Map[String, GameData]) = {
    HttpRoutes.of[IO] {
      case req @ POST  -> Root / "number" => {
        req.as[GameDto].flatMap { game =>
          val out = for {
            cookie <- IO.fromOption(req.cookies.find(_.name == ClientIdCookieName))(new RuntimeException("Cookie not found"))
            gameData <- cookie match {
              case RequestCookie(_, id) => IO.fromOption(clientCache.get(id))(new RuntimeException("User not found"))
            }
            gameResult <- process(gameData, game.attributes.current)
            _ <- updateUserResult(gameResult, cookie.content, clientCache)
            response <- Ok(toResponseDto(gameResult))
          } yield response

          out.handleErrorWith(_ => Forbidden(ForbiddenErrorDto()))
        }
      }

      case _ @ GET -> Root / "start" :? MinMatcher(minimum) :? MaxMatcher(maximum) :? AttemptsMatcher(attempts) => {
        val id = UUID.randomUUID().toString
        val secretNumber = Random.between(minimum, maximum)
        clientCache(id) = GameData(secretNumber, attempts)
        Ok(GameStarDto()).map(_.addCookie(ClientIdCookieName, id))
      }
    }
  }

  private def process(game: GameData, currentNumber: Int): IO[GameResult] = {
    game.rightAnswer == currentNumber match {
      case true if game.attempts >= 1 => IO(GameSuccess)
      case false if game.attempts >= 1 => IO(GameWrongResult)
      case _ => IO(GameOver)
    }
  }

  private def updateUserResult(result: GameResult, id: String, users: mutable.Map[String, GameData]): IO[Unit] = result match {
    case GameSuccess | GameOver => IO.delay(users.remove(id)) *> IO.unit
    case GameWrongResult => IO.delay((users(id) = GameData(users(id).rightAnswer, users(id).attempts - 1))) *> IO.unit
  }

  private def toResponseDto(result: GameResult): IO[GameResultDto] = result match {
    case GameSuccess => IO(GameResultDto(attributes = GameResultAttributes("success")))
    case GameWrongResult => IO(GameResultDto(attributes = GameResultAttributes("wrong")))
    case GameOver => IO(GameResultDto(attributes = GameResultAttributes("failure")))
  }

}

object Client {
  import Consts._
  import org.http4s.client._

  case class RandomGameNumbers(min: Int, max: Int, attempts: Int)

  private val backUrl = uri"http://localhost:8080"

  def registerGame(client: Client[IO]) = {

    for {
      numbers <- generateRandomNumbers()
      id <- {
        client.get(
          (backUrl / "start").withQueryParams(
            Map(
              ("min" -> numbers.min.toString),
              ("max" -> numbers.max.toString),
              ("attempts" -> numbers.attempts.toString)
            )
          )
        )(
          resp => resp.cookies
            .find(_.name == ClientIdCookieName)
            .fold(
              throw new RuntimeException(s"Cookie with name = $ClientIdCookieName was not found")
            )(cookie => IO.pure(cookie.content))
        )
      }
    } yield (id, numbers)
  }

  def getGameResult(client: Client[IO], numbers: RandomGameNumbers, id: String): IO[Unit] = {
    import DTOs._
    val number = Random.between(numbers.min, numbers.max)

    val req =
      Request[IO](method = Method.POST, uri = backUrl.withPath("/number"))
        .withEntity(GameDto(attributes = GameAttributes(number)))
        .addCookie(ClientIdCookieName, id)

    client.expect[GameResultDto](req).flatMap {
      case GameResultDto(_, attributes) if attributes.result == "success" => printLine("You won") *> IO.unit
      case GameResultDto(_, attributes) if attributes.result == "failure" => printLine("You lost") *> IO.unit
      case _ => getGameResult(client, numbers, id)
    }
  }

  private def generateRandomNumbers(): IO[RandomGameNumbers] = for {
    min <- IO.delay(Random.between(1, 100))
    max <- IO.delay(min + Random.between(1, 100))
    attempts <- IO.delay(Random.between(1, 10))
  } yield RandomGameNumbers(min, max, attempts)

}

object RunClient extends IOApp {
  import Consts._

  override def run(args: List[String]): IO[ExitCode] = {
    BlazeClientBuilder[IO](ExecutionContext.global).resource
      .parZip(Blocker[IO]).use { case (client, blocker) =>
      for {
        _   <- printLine(string = "Starting game and getting id:")
        registeredGame <- Client.registerGame(client)
        (id, numbers) = registeredGame
        _   <- Client.getGameResult(client, numbers, id)
        _   <- printLine("Next step")
        _   <- printLine("-" * 8)
      } yield ExitCode.Success
    }

  }
}

object RunServer extends IOApp {
  import Server._
  import Consts._

  def httpApp () =
    gameRoutes(mutable.Map()).orNotFound

  override def run(args: List[String]): IO[ExitCode] = for {
    _ <- BlazeServerBuilder[IO](ExecutionContext.global)
      .bindHttp(Port)
      .withHttpApp(httpApp())
      .serve
      .compile
      .drain
      .as(ExitCode.Success)
  } yield ExitCode.Success

}
