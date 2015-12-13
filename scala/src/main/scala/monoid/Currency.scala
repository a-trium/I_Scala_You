package monoid

trait Currency
case class USD(amount: Int)
case class EUR(amount: Int)

case class CurrencyOp(amount: Int) {
  def USD = monoid.USD(amount)
  def EUR = monoid.EUR(amount)
}


object Currency {
  implicit def convertToCurOp(amount: Int) =
    CurrencyOp(amount)
}


