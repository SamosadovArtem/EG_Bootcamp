package controlstructures

import controlstructures.Domain.Command.{AverageCommand, DivideCommand, MaxCommand, MinCommand, SumCommand}
import controlstructures.Domain.Result.{AverageResult, DivideResult, MaxResult, MinResult, SumResult}
import controlstructures.Domain.{Command, Result}


trait Service {

  def calculate(command: Command): Either[String, Result]

}

object Service {

  def dsl(): Service = {
    case AverageCommand(nums)               => Right(AverageResult(nums, nums.sum / nums.length))
    case DivideCommand(dividend, divisor)   => Right(DivideResult(dividend, divisor,  dividend / divisor))
    case MaxCommand(nums)                   => Right(MaxResult(nums, nums.max))
    case MinCommand(nums)                   => Right(MinResult(nums, nums.min))
    case SumCommand(nums)                   => Right(SumResult(nums, nums.sum))
    case _                                  => Left("Command not found")
  }
}
