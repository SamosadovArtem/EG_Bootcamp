package controlstructures

import controlstructures.Domain.Command
import controlstructures.Domain.Command.{AverageCommand, DivideCommand, MaxCommand, MinCommand, SumCommand}

trait Controller {
  def run(): Unit
}

object Controller {
  def dsl(service: Service)(implicit console: InputSource): Controller = () => {
    console.putStr("App is working, print q to quit")

    @scala.annotation.tailrec
    def commandLoop(): Unit = {

      val input = console.getStr

      if (input == "q") return

      val res = for {
        parseRes <- pareInput(input)
        out <- service.calculate(parseRes)
      } yield out

      res match {
        case Left(err) => console.putError(err)
        case Right(res) => console.putSuccess(res.fancyString)
      }

      commandLoop()

    }

    commandLoop()

    def pareInput(input: String):Either[String, Command] = {
      if (input == null || input.isEmpty) return Left("Empty string is not allowed")
      input.split("\\s").toList match {
        case x if x.length == 1         => Left("Please put numbers after the command")
        case x :: xs if x == "sum"      => validateInput(xs).map(nums => nums.map(_.toDouble)).map(SumCommand)
        case x :: xs if x == "divide"   =>
          validateDividion(xs).map(nums => nums.map(_.toDouble)).map(input => DivideCommand(input.head, input.last))
        case x :: xs if x == "average"  => validateInput(xs).map(nums => nums.map(_.toDouble)).map(AverageCommand)
        case x :: xs if x == "min"      => validateInput(xs).map(nums => nums.map(_.toDouble)).map(MinCommand)
        case x :: xs if x == "max"      => validateInput(xs).map(nums => nums.map(_.toDouble)).map(MaxCommand)
        case _ => Left("Command not supported")
      }
    }

    def validateInput(input: List[String]) = {
      for {
        res <- Validators.onlyNumbers(input)
      } yield res
    }

    def validateDividion(input: List[String]) = {
      for {
        numbers <- Validators.onlyNumbers(input)
        res <- Validators.onlyTwoNumbers(numbers)
      } yield res
    }

  }

  object Validators {
    def onlyNumbers(input: List[String]):Either[String, List[String]] = {
      input.map(v => v.toDoubleOption).forall(_.isDefined) match {
        case true => Right(input)
        case false => Left("only numbers allowed")
      }
    }

    def onlyTwoNumbers(input: List[String]):Either[String, List[String]] = {
      input.length match {
        case 2 => Right(input)
        case _ => Left("Please put two numbers after the command")
      }
    }
  }

}
