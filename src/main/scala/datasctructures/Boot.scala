package datasctructures

 /*
 Homework

 Implement a special sort which sorts the keys of a map (K) according to their associated
 values (V).

 In case of "ties" (equal values) it should group these keys K into Set-s in the results.

 The input is a map where keys (K) are the values to be sorted and values are their associated numeric
 values.

 The output is a list (in ascending order according to the associated `Int` values) of tuples of `Set`-s
 with values from K, and the associated value V for these values in the `Set`.

 For example:

 Input `Map("a" -> 1, "b" -> 2, "c" -> 4, "d" -> 1, "e" -> 0, "f" -> 2, "g" -> 2)` should result in
 output `List(Set("e") -> 0, Set("a", "d") -> 1, Set("b", "f", "g") -> 2, Set("c") -> 4)`.
 */

object Boot extends App {
  def sortConsideringEqualValues[T](map: Map[T, Int]): List[(Set[T], Int)] = {
    map.groupMap {
      case (_, v) => v
    } {
      case (k, _) => k
    }.map {
      case (count, allValues) => allValues.toSet -> count
    }.toList.sortBy {
      case (_, num) => num
    }
  }

  val input = Map("a" -> 1, "b" -> 2, "c" -> 4, "d" -> 1, "e" -> 0, "f" -> 2, "g" -> 2)
  val expected = List(Set("e") -> 0, Set("a", "d") -> 1, Set("b", "f", "g") -> 2, Set("c") -> 4)
  assert(sortConsideringEqualValues(input) == expected)

  println("Finished successfully")
  println(s"Output for $input is ${sortConsideringEqualValues(input)}")


}
