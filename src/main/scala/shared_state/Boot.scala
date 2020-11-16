package shared_state

import cats.implicits._

import cats.Monad
import cats.effect.concurrent.Ref
import cats.effect.{Clock, Concurrent, ExitCode, IO, IOApp, Timer}

import scala.concurrent.duration._

object Boot extends IOApp {

  trait Cache[F[_], K, V] {
    def get(key: K): F[Option[V]]

    def put(key: K, value: V): F[Unit]
  }

  class RefCache[F[_] : Clock : Monad, K, V](
    state: Ref[F, Map[K, (Long, V)]],
    expiresIn: FiniteDuration
  ) extends Cache[F, K, V] {

    def get(key: K): F[Option[V]] =
      state.get.map(
        keyValueMap => keyValueMap.get(key).map {
          case (_, value) => value
        }
      )

    def put(key: K, value: V): F[Unit] = Clock[F].realTime(MILLISECONDS).flatMap {
      time => state.update(cache => cache ++ Map(key -> (time + expiresIn.toMillis, value)))
    }

  }

  object Cache {
    def of[F[_] : Clock, K, V](
      expiresIn: FiniteDuration,
      checkOnExpirationsEvery: FiniteDuration
    )(implicit T: Timer[F], C: Concurrent[F]): F[Cache[F, K, V]] = {

      def removeExpiredCache(cacheState: Ref[F, Map[K, (Long, V)]]): F[Unit] = for {
        now <- T.sleep(checkOnExpirationsEvery) *> Clock[F].realTime(MILLISECONDS)
        currentKeyValueMap <- cacheState.get
        updatedKeyValueMap = currentKeyValueMap.filter {
          case (_, (inputTimestamp, _)) => now - inputTimestamp <= expiresIn.toMillis
        }
        _ <- cacheState.set(updatedKeyValueMap)
      } yield()

      for {
        cache <- Ref.of[F, Map[K, (Long, V)]](Map.empty)
        _ <- C.start(removeExpiredCache(cache).foreverM.void)
      } yield new RefCache[F, K, V](cache, expiresIn)
    }

  }

  override def run(args: List[String]): IO[ExitCode] = {

    for {
      cache <- Cache.of[IO, Int, String](10.seconds, 4.seconds)
      _ <- cache.put(1, "Hello")
      _ <- cache.put(2, "World")
      _ <- cache.get(1).flatMap(s => IO {
        println(s"first key $s")
      })
      _ <- cache.get(2).flatMap(s => IO {
        println(s"second key $s")
      })
      _ <- IO.sleep(12.seconds)
      _ <- cache.get(1).flatMap(s => IO {
        println(s"first key $s")
      })
      _ <- cache.get(2).flatMap(s => IO {
        println(s"second key $s")
      })
      _ <- IO.sleep(12.seconds)
      _ <- cache.get(1).flatMap(s => IO {
        println(s"first key $s")
      })
      _ <- cache.get(2).flatMap(s => IO {
        println(s"second key $s")
      })
    } yield ExitCode.Success
  }

  /*
  Output:
  first key Some(Hello)
  second key Some(World)
  first key Some(Hello)
  second key Some(World)
  first key None
  second key None
  */
}
