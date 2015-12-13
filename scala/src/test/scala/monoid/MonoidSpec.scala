package monoid

import org.scalatest.{Matchers, FunSuite}

/**
 * @author choduk88@sk.com 
 * @since 2015. 12. 11..
 */
class MonoidSpec extends FunSuite with Matchers{
  test("1") {
    1 shouldBe 2
  }

  type Filter[A] = A => Boolean
  case class User(name: String, city: String)
  val users = List(User("Kelly", ".LONDON"), User("John", "..NY"), User("Cark", ".KAW"))

  test("Filters are monoid"){
    import scalaz._
    import Scalaz._
    import Tags._
    import syntax._

    val london: Filter[User] = u => u.city endsWith(".LONDON")
    val ny: Filter[User]     = (_:User).city endsWith(".NY")

    implicit def monoidFilter[A] = new Monoid[Filter[A]] {
      override def zero: Filter[A] =
        a => false

      override def append(f1: Filter[A], f2: => Filter[A]): Filter[A] =
        a => f1(a) || f2(a)
    }

    users filter (london |+| ny)
    println(users filter (london |+| ny))
  }
}
