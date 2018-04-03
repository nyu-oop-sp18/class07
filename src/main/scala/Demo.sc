import scala.util.{Try, Success, Failure}
import scala.language.postfixOps

val res = Try(1 / 0)

val x = res match {
  case Success(x) => x
  case Failure(ex) => Double.PositiveInfinity
}

for {
  v <- res
} yield v + 1

val nat: Stream[Int] = {
  def loop(v: Int): Stream[Int] = v #:: loop(v + 1)
  loop(0)
}

def isOdd(x: Int) = x % 2 > 0

nat map (x => x * x) filter isOdd takeWhile (_ < 10000) sum

nat.map(Math.sqrt(_)).scan(0.0)(_ + _).takeWhile(_ < 1000.0).size

