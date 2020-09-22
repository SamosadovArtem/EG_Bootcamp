package basics

import scala.annotation.tailrec

object Boot extends App {

  @tailrec
  def gcd(a: Int, b: Int): Int = {
    if (b == 0) {
      a
    } else {
      gcd(b, a % b)
    }

  }

  def lcm(a: Int, b: Int): Int = Math.abs(a * b) / gcd(a, b)

  val gcdRes = gcd(54, 24)
  val lcmRes = lcm(4, 6)

  assert(gcdRes == 6)
  assert(lcmRes == 12)

  println(s"Calculations finished successfully. gcd(54, 24) == $gcdRes, lcm(4, 6) == ${lcmRes}")

}
