package typeclass


import typeclass.Boot.MyTwitter.{FbiNote, Twit}

import scala.collection.immutable.ArraySeq
import scala.collection.mutable

object Boot {

  val StandardHeaderSize = 12

  object SuperVipCollections4s {
    type SizeScore = Int

    trait GetSizeScore[T] {
      def apply(value: T): SizeScore
    }

    object syntax {

      implicit class GetSizeScoreOps[T: GetSizeScore](inner: T) {
        def sizeScore: SizeScore = implicitly[GetSizeScore[T]].apply(inner)
      }

    }
    final class MutableBoundedCache[K: GetSizeScore, V: GetSizeScore](maxSizeScore: SizeScore) {

      import syntax._
      private val map = mutable.LinkedHashMap.empty[K, V]

      def put(key: K, value: V): Unit = {
        val scoreToAdd = key.sizeScore + value.sizeScore
        reduceCurrentScore(scoreToAdd)
        map.put(key, value)
      }

      def get(key: K): Option[V] = map.get(key)

      private def reduceCurrentScore(scoreToAdd: SizeScore): Unit = {
        val fullScore = getCurrentScore + scoreToAdd
        if (fullScore > maxSizeScore) {
          map.remove(map.head._1)
          reduceCurrentScore(scoreToAdd)
        }
      }

      private def getCurrentScore: SizeScore = map.map {
        case (key, value) => key.sizeScore + value.sizeScore
      }.sum
    }

    final case class PackedMultiMap[K, +V](inner: ArraySeq[(K, V)])

    object PackedMultiMap {
      def empty[K, V]: PackedMultiMap[K, V] = PackedMultiMap()

      def apply[K, V](values: (K, V)*): PackedMultiMap[K, V] = PackedMultiMap(inner = ArraySeq(values: _*))
    }

    trait Iterate[-F[_]] {
      def iterator[T](f: F[T]): Iterator[T]
    }

    trait Iterate2[-F[_, _]] {
      def iterator1[T, S](f: F[T, S]): Iterator[T]

      def iterator2[T, S](f: F[T, S]): Iterator[S]
    }

    object instances {

      import syntax._

      implicit val iterableOnceIterate: Iterate[Iterable] = new Iterate[Iterable] {
        override def iterator[T](f: Iterable[T]): Iterator[T] = f.iterator
      }

      implicit val arrayIterate: Iterate[Array] = new Iterate[Array] {
        override def iterator[T](f: Array[T]): Iterator[T] = f.iterator
      }
      implicit val mapIterate: Iterate2[Map] = new Iterate2[Map] {
        override def iterator1[T, S](f: Map[T, S]): Iterator[T] = f.keysIterator

        override def iterator2[T, S](f: Map[T, S]): Iterator[S] = f.valuesIterator
      }
      implicit val packedMultiMapIterator: Iterate2[PackedMultiMap] = new Iterate2[PackedMultiMap] {
        override def iterator1[T, S](f: PackedMultiMap[T, S]): Iterator[T] = f.inner.map {
          case (t, _) => t
        }.iterator

        override def iterator2[T, S](f: PackedMultiMap[T, S]): Iterator[S] = f.inner.map {
          case (_, v) => v
        }.iterator
      }

      implicit val scoredByte: GetSizeScore[Byte] = _ => 1
      implicit val scoredInt:  GetSizeScore[Int]  = _ => 4
      implicit val scoredLong: GetSizeScore[Long] = _ => 8
      implicit val scoredChar: GetSizeScore[Char] = _ => 2
      implicit val scoredString: GetSizeScore[String] = value => StandardHeaderSize + value.length * 2

      implicit def scoredList[T : GetSizeScore]: GetSizeScore[List[T]] = v =>
        StandardHeaderSize + v.map(_.sizeScore).sum
      implicit def scoredVector[T : GetSizeScore]: GetSizeScore[Vector[T]] = v =>
        StandardHeaderSize + v.map(_.sizeScore).sum
      implicit def scoredPackedMultiMap[K : GetSizeScore]: GetSizeScore[PackedMultiMap[K, K]] = v =>
        StandardHeaderSize + v.inner.map {case(k,_) => k.sizeScore}.sum + v.inner.map {case(_,v) => v.sizeScore}.sum
      implicit def scoredPackedArray[T : GetSizeScore]: GetSizeScore[Array[T]] = v => {
        StandardHeaderSize + v.map(_.sizeScore).sum
      }
      implicit def scoredMap[K : GetSizeScore]: GetSizeScore[Map[K, K]] = v =>
        StandardHeaderSize + v.keysIterator.map(_.sizeScore).sum + v.values.map(_.sizeScore).sum

      implicit val fbiNote: GetSizeScore[FbiNote] = {
        case FbiNote(month, favouriteChar, watchedPewDiePieTimes)
        => month.sizeScore + favouriteChar.sizeScore + watchedPewDiePieTimes.sizeScore
      }
      implicit val scoreTwit: GetSizeScore[Twit] = {
        case Twit(id, userId, hashTags, attributes, fbiNotes)
        => id.sizeScore + userId.sizeScore + hashTags.sizeScore + fbiNotes.sizeScore + attributes.sizeScore
      }
    }
  }

  object MyTwitter {
    import SuperVipCollections4s._

    final case class Twit(
                           id: Long,
                           userId: Int,
                           hashTags: Vector[String],
                           attributes: PackedMultiMap[String, String],
                           fbiNotes: List[FbiNote],
                         )

    final case class FbiNote(
                              month: String,
                              favouriteChar: Char,
                              watchedPewDiePieTimes: Long,
                            )

    trait TwitCache {
      def put(twit: Twit): Unit
      def get(id: Long): Option[Twit]
    }

    def createTwitCache(maxSizeScore: SizeScore): TwitCache = new TwitCache {
      import instances._
      val map = new MutableBoundedCache[Long, Twit](maxSizeScore)

      override def put(twit: Twit): Unit = map.put(twit.id, twit)

      override def get(id: Long): Option[Twit] = map.get(id)
    }
  }
}
