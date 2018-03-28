import scala.util.{Try, Success, Failure}

val res = Try(1 / 0)

val x = res match {
  case Success(x) => x
  case Failure(ex) => Double.PositiveInfinity
}

for {
  v <- res
} yield v + 1