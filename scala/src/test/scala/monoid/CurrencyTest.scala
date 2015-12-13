package monoid

import org.scalatest.{Matchers, FunSuite}

/**
 * @author choduk88@sk.com 
 * @since 2015. 12. 11..
 */
class CurrencyTest extends FunSuite with Matchers{
  test("implicit test") {
    import Currency._
    1.USD shouldBe USD(1)
    1.EUR shouldBe EUR(1)

    import scalaz._, Scalaz._

    implicit def monoidUSD = new Monoid[USD] {
      override def zero: USD = USD(Monoid[Int].zero)
      override def append(u1: USD, u2: => USD): USD =
        USD(u1.amount |+| u2.amount)
    }

    implicit def monoidEUR = new Monoid[EUR] {
      override def zero: EUR = EUR(Monoid[Int].zero)
      override def append(u1: EUR, u2: => EUR): EUR =
        EUR(u1.amount |+| u2.amount)
    }

    1.USD |+| 2.USD shouldBe 3.USD
    1.EUR |+| 2.EUR shouldBe 3.EUR

  }
}
