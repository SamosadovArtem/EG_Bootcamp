package error_handling

import java.time.LocalDate
import cats.data.ValidatedNec
import cats.syntax.all._
import scala.util.{Success, Try}



object DSL {

  final case class CardName(name: String) extends AnyVal
  final case class CardNumber(number: String) extends AnyVal
  final case class CardExpDate(date: LocalDate) extends AnyVal
  final case class SecurityCode(code: Int) extends AnyVal
}

object Homework {

  import DSL._

  val NaiveCreditCardPattern = "\\d{4}-*\\d{4}-*\\d{4}-*\\d{4}"
  val NaiveNamePattern = "^[a-zA-Z ]+$"
  val SecurityCodeLength = 3


  case class CreditCard(
    name: CardName,
    number: CardNumber,
    date: CardExpDate,
    securityCode: SecurityCode
  )

  sealed trait ValidationError {
    def message: String
  }

  object ValidationError {

    case class InvalidName(message: String) extends ValidationError

    case class InvalidNumber(message: String) extends ValidationError

    case class InvalidDate(message: String) extends ValidationError

    case class DateShouldBeFuture(message: String) extends ValidationError

    case class InvalidSecurityCode(message: String) extends ValidationError

    case class SecurityCodeTooShort(message: String) extends ValidationError

    case class SecurityCodeTooLong(message: String) extends ValidationError

    case class SecurityCodeOnlyDigits(message: String) extends ValidationError

  }

  object CreditCardValidator {

    import ValidationError._

    type AllErrorsOr[A] = ValidatedNec[ValidationError, A]

    def validate(
      name: String,
      number: String,
      expirationDate: String,
      securityCode: String,
    ): AllErrorsOr[CreditCard] = (
      validateName(name),
      validateNumber(number),
      validateDate(expirationDate),
      validateSecurityCode(securityCode)
      ).mapN(CreditCard)

    private def validateName(name: String): AllErrorsOr[CardName] =
      if (name.matches(NaiveNamePattern)) {
        CardName(name).validNec
      } else {
        InvalidName(
          s"name $name is invalid"
        ).invalidNec
      }

    private def validateNumber(number: String): AllErrorsOr[CardNumber] =
      if (number.matches(NaiveCreditCardPattern)) {
        CardNumber(number).validNec
      } else {
        InvalidNumber(
          s"Card number $number is invalid"
        ).invalidNec
      }

    private def validateDate(date: String): AllErrorsOr[CardExpDate] = {

      def validateDateString(date: String): AllErrorsOr[LocalDate] = Try(LocalDate.parse(date)) match {
        case Success(value) => value.validNec
        case _ => InvalidDate(
          s"date $date is invalid"
        ).invalidNec
      }

      def validateDateFuture(date: LocalDate): AllErrorsOr[LocalDate] = if (date.isAfter(LocalDate.now())) {
        date.validNec
      } else {
        DateShouldBeFuture(
          s"date $date should be > then ${LocalDate.now()}"
        ).invalidNec
      }

      validateDateString(date) andThen validateDateFuture map CardExpDate
    }

    private def validateSecurityCode(code: String): AllErrorsOr[SecurityCode] = {

      def validateDigits(code: String): AllErrorsOr[Int] = code.toIntOption match {
        case Some(value) => value.validNec
        case _ => SecurityCodeOnlyDigits(
          s"code $code is invalid, should contains only digits"
        ).invalidNec
      }

      def tooShort(code: String): AllErrorsOr[String] =
        if (code.length < SecurityCodeLength) {
          SecurityCodeTooShort(
            s"code $code invalid, should contains 3 digits"
          ).invalidNec
        } else {
          code.validNec
        }

      def tooLong(code: String): AllErrorsOr[String] =
        if (code.length > SecurityCodeLength) {
          SecurityCodeTooLong(
            s"code $code invalid, should contains 3 digits"
          ).invalidNec
        } else {
          code.validNec
        }

      tooShort(code) *> tooLong(code) andThen validateDigits map SecurityCode
    }
  }

}
