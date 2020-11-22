package http

import java.util.UUID

import cats.effect.{ExitCode, IO, IOApp}
import io.circe.generic.auto._
import org.http4s._
import org.http4s.circe.CirceEntityCodec._
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder

import collection.mutable.Map
import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.util.Random

object Consts {
  val ClientIdCookieName = "clientId"
  case class ClientId(value: String) extends AnyVal
  case class GameData(rightAnswer: Int, attempts: Int)
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

  def gameRoutes(clientCache: mutable.Map[String, GameData]) = {
    HttpRoutes.of[IO] {
      case req @ POST  -> Root / "number" => {
        req.as[GameDto].flatMap { game =>
          req.cookies.find(_.name == ClientIdCookieName).flatMap {
            case RequestCookie(_, id) => clientCache.get(id)
          }
            .map(gameData => process(gameData, game.attributes.current))
            .map(toResponseDto)
            .map(Ok(_))
            .getOrElse(Forbidden(ForbiddenErrorDto()))
        }
      }

      case req @ GET -> Root / "start" :? MinMatcher(minimum) :? MaxMatcher(maximum) :? AttemptsMatcher(attempts) => {
        val id = UUID.randomUUID().toString
        val secretNumber = Random.between(minimum, maximum)
        clientCache(id) = GameData(secretNumber, attempts)
        Ok(GameStarDto()).map(_.addCookie(ClientIdCookieName, id))
      }



    }
  }

  private def process(game: GameData, currentNumber: Int): GameResult = {
    game.rightAnswer == currentNumber match {
      case true if game.attempts >= 1 => GameSuccess
      case false if game.attempts >= 1 => GameWrongResult
      case _ => GameOver
    }
  }

  private def updateUserResult(result: GameResult, id: String, users: mutable.Map[String, GameData]) = result match {
    case GameSuccess | GameOver => users.remove(id)
    case GameWrongResult => users(id) = GameData(users(id).rightAnswer, users(id).attempts - 1)
  }

  private def toResponseDto(result: GameResult) = result match {
    case GameSuccess => GameResultDto(attributes = GameResultAttributes("success"))
    case GameWrongResult => GameResultDto(attributes = GameResultAttributes("wrong"))
    case GameOver => GameResultDto(attributes = GameResultAttributes("failure"))
  }

}

object Boot extends IOApp {
  import Server._

  def httpApp () =
    gameRoutes(mutable.Map()).orNotFound

  override def run(args: List[String]): IO[ExitCode] = for {
    _ <- BlazeServerBuilder[IO](ExecutionContext.global)
      .withHttpApp(httpApp())
      .serve
      .compile
      .drain
      .as(ExitCode.Success)
  } yield ExitCode.Success


}
