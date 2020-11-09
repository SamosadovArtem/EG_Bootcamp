package effects

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

object Boot extends App {

  final case class Pure[A](a: A) extends IO[A]

  final private case class Map[A, B](a: IO[A], f: A => B) extends IO[B] with (A => IO[B]) {
    override def apply(a: A): IO[B] =
      Pure(f(a))
  }
  final private case class FlatMap[A, B](a: IO[A], f: A => IO[B]) extends IO[B]

  final case class Delay[A](thunk: () => A) extends IO[A]

  final case class IOError[A](e: Throwable) extends IO[A]

  class IO[+A] {

    private def runInternal(input: IO[Any]): A = {
      val out = getInitValue(input)
      val inputChain: List[Any => Any] = List()
      val chain: List[Any => Any] = getChain(input, inputChain)
      applyAll(out, chain.reverse).asInstanceOf[A]
    }

    private def run(): A = {
      val out = getInitValue(this)
      val inputChain: List[Any => Any] = List()
      val chain: List[Any => Any] = getChain(this, inputChain)
      applyAll(out, chain.reverse).asInstanceOf[A]
    }

    def applyAll(v: Any, chain: List[Any => Any]): Any = {
      chain match {
        case x :: xs =>
          val res = x(v)
          res match {
            case input: IO[A] =>
              val subResult = runInternal(input)
              applyAll(subResult, xs)
            case _ =>
              applyAll(res, xs)
          }
        case _ => v
      }
    }

    def getChain[A](input: IO[A], chain: List[Any => Any]): List[Any => Any] = {
      input match {
        case Pure(_) => chain
        case Map(a, f) =>
          val res: List[Any => Any] = chain.appended(f)
          getChain(a, res)
        case FlatMap(a, f) =>
          val res: List[Any => Any] = chain.appended(f)
          getChain(a, res)
        case Delay(f) =>
          val out: Any => Any = _ => f()
          getChain(input, chain.appended(out))
        case IOError(e) =>
          val f: Any => Any = _ => e
          getChain(input, chain.appended(f))
      }
    }


    def getInitValue[A](input: IO[A]): Any = {
      input match {
        case Pure(a) => a
        case Map(a, _) => getInitValue(a)
        case FlatMap(a, _) => getInitValue(a)
        case Delay(f) => f()
        case IOError(e) => e
      }
    }
    def map[B](f: A => B): IO[B] = Map(this, f)
    def flatMap[B](f: A => IO[B]): IO[B] = FlatMap(this, f)
    def *>[B](another: IO[B]): IO[B] = flatMap(_ => another)
    def as[B](newValue: => B): IO[B] = map(_ => newValue)
    def void: IO[Unit] = IO.unit
    def attempt: IO[Either[Throwable, A]] = IO(Try(run()).toEither)
    def option: IO[Option[A]] = attempt.map(either => either.toOption)
    def handleErrorWith[AA >: A](f: Throwable => IO[AA]): IO[AA] = Try(run()) match {
      case Failure(exception) => f(exception)
      case Success(value)     => IO(value)
    }
    def redeem[B](recover: Throwable => B, map: A => B): IO[B] = attempt.map(_.fold(recover, map))
    def redeemWith[B](recover: Throwable => IO[B], bind: A => IO[B]): IO[B] = attempt.flatMap(_.fold(recover, bind))
    def unsafeRunSync(): Any = run()
    def unsafeToFuture()(implicit ex: ExecutionContext): Future[A] = Future {run()}
  }

  object IO {
    def apply[A](body: => A): IO[A] = delay(body)
    def suspend[A](thunk: => IO[A]): IO[A] = delay(thunk.run())
    def delay[A](body: => A): IO[A] = Delay(() => body)
    def pure[A](a: A): IO[A] = Pure(a)
    def fromEither[A](e: Either[Throwable, A]): IO[A] = e match {
      case Left(value)  => raiseError(value)
      case Right(value) => IO(value)
    }
    def fromOption[A](option: Option[A])(orElse: => Throwable): IO[A] = option match {
      case None        => raiseError(orElse)
      case Some(value) => IO(value)
    }
    def fromTry[A](t: Try[A]): IO[A] = t match {
      case Failure(exception) => raiseError(exception)
      case Success(value)     => IO(value)
    }
    def none[A]: IO[Option[A]] = pure(None)
    def raiseError[A](e: Throwable): IO[A] = IOError(e)
    def raiseUnless(cond: Boolean)(e: => Throwable): IO[Unit] = if (cond) unit else raiseError(e)
    def raiseWhen(cond: Boolean)(e: => Throwable): IO[Unit] = if(cond) raiseError(e) else unit
    def unlessA(cond: Boolean)(action: => IO[Unit]): IO[Unit] = if (cond) unit else action
    def whenA(cond: Boolean)(action: => IO[Unit]): IO[Unit] = if (cond) action else unit
    val unit: IO[Unit] = pure(())
  }

  val out = IO.pure(1)
    .map(_ => 2)
    .flatMap(_ => IO.pure(3))
    .flatMap(_ => IO.pure(4))
    .map(res => res + 2)
    .unsafeRunSync()
  println(out)
}
