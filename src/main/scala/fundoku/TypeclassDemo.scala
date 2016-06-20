package fundoku

object TypeclassDemo extends App {

  case class Car(name: String)

  trait Addable[A] {
    def add(a1: A, a2: A): A
  }

  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  implicit val optionFunctor = new Functor[Option] {
    def map[A, B](fa: Option[A])(f: A => B): Option[B] =
      fa match {
        case Some(a) => Some(f(a))
        case None => None
      }
  }

  trait Applicative[F[_]] {
    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
  }

  def operationInvolvingMap[F[_]: Functor, A, B](a: F[A])(f: A => B): F[B] = {
    implicitly[Functor[F]].map(a)(f)
  }


  implicit val plusInt = new Addable[Int] {
    override def add(a1: Int, a2: Int): Int = {
      a1 + a2
    }
  }

  def addNoContextBounds[A](a1: A, a2: A)(implicit plusA: Addable[A]): A = {
    plusA.add(a1, a2)
  }

  def addWithContextBounds[A: Addable](a1: A, a2: A): A = {
    implicitly[Addable[A]].add(a1, a2)
  }

  val c1 = new Car("car1")
  val c2 = new Car("car2")

  implicit val plusCar = new Addable[Car] {
    override def add(a1: Car, a2: Car): Car = Car(a1.name + a2.name)
  }

  println(addWithContextBounds(c1, c2))


  def map2[A, B, C](a: A, b: B)(f: (A, B) => C): C = {
    f(a, b)
  }
}
