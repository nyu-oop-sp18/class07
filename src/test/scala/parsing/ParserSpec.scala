package parsing

//import scala.language.postfixOps
import org.scalatest.FlatSpec

class ParserSpec extends FlatSpec {

  import parsing.Parser._

  /* some ideas for unit tests 
  assert('a'.parse('a').get === 'a')
  
  assert(('a' orElse 'b').parse('a').get === 'a')

  assert(('a' orElse 'b').parse('b').get === 'b')

  assert(('a' andThen 'b').parse("ab").get === ('a', 'b'))
  
  assert(repeat('a').parse("aaa").get === List('a', 'a', 'a'))

  assert(repeatN(3)('a').parse("aaa").get === List('a', 'a', 'a'))
  
  assert((repeat('a') map (_.size)).parse("aaa").get === 3)

  assert((digit flatMap (repeatN(_)('a'))).parse("3aaa").get === List('a', 'a', 'a'))

  assert((attempt('a' andThen 'a') orElse ('a' andThen 'b')).parse("ab").get === ('a', 'b'))
  */
}
