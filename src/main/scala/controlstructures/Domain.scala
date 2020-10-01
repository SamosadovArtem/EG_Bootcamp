package controlstructures

object Domain {

  implicit class FancyStringList(input: List[Double]){
    def fancyString: String  = {
      input.map(_.toString).reduce(_ + " " +_)
    }
  }

  sealed trait Command
  object Command {
    final case class AverageCommand(nums: List[Double])               extends Command
    final case class DivideCommand(dividend: Double, divisor: Double) extends Command
    final case class MaxCommand(nums: List[Double])                   extends Command
    final case class MinCommand(nums: List[Double])                   extends Command
    final case class SumCommand(nums: List[Double])                   extends Command
  }

  sealed trait Result {
    def fancyString: String

  }

  object Result {
    final case class AverageResult(input: List[Double], result: Double) extends Result {
      override def fancyString: String = s"the avg of ${input.fancyString} is ${result}"
    }
    final case class DivideResult(dividend: Double, divisor: Double, result: Double)  extends Result {
      override def fancyString: String = s"$dividend divided by $divisor is ${result}"
    }
    final case class MaxResult(input: List[Double], result: Double) extends Result {
      override def fancyString: String = s"the maximum of ${input.fancyString} is $result"
    }
    final case class MinResult(input: List[Double], result: Double) extends Result {
      override def fancyString: String = s"the minimum of ${input.fancyString} is $result"
    }
    final case class SumResult(input: List[Double], result: Double) extends Result {
      override def fancyString: String = s"the sum of ${input.fancyString} is ${result}"
    }

  }

}
