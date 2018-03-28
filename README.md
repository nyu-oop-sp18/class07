# Class 7

## Object-Functional Design

When you design a software component, it is helpful to start
your design by considering the interface of the component with its
environment (i.e. other components) before designing the internal
representation and implementation details of the component. That is,
you should first think about what objects and methods the component
should provide, what are the type signatures of these methods, what
properties should the interactions between objects and their methods
satisfy, which aspects of the internal state of the component should
be observable by the environment - if any, etc. These considerations
put constraints on the possible implementations of the component that
inform your design process down the road and help you catch design
flaws early on.

In the context of object-functional design where objects have no
observable state, we can often think of a software component as an
*algebra* in the mathematical sense, i.e. as a set of objects equipped
with operations that satisfy certain algebraic laws. We have already
discussed monoids as one example of such an algebra. Identifying the
algebra behind a specific programming problem requires some thought
and practice. However, this effort is usually a good investment
because a principled design approach tends to yield cleaner code that
is easier to maintain and extend compared to an ad hoc design.

We will practice object-functional design in a mini project whose goal
is to implement a parsing library. The purpose of today's class is to
brain storm about the basic design of such a library and to set some
"sign posts" for your implementation of the project.

## Designing a Parser Combinator Library

Parsing is the problem of transforming unstructured data given as a
character sequence into structured data. This problem is ubiquitous in
programming and there exists an abundance of tools to help with this
task, ranging from regular expression libraries to full blown parser
generators that automatically generate efficient parser code from a
grammatical description of the syntax of the language to be parsed.

Here, our goal is not so much efficiency but flexibility and good
error reporting capabilities. To this end, we will design a *parser
combinator* library that allows us to build complex parsers by
combining simpler subparsers using certain composition operations
defined on them.

Ultimately, we want to be able to use this library to build parsers
for parsing arbitrarily complex languages such HTML or even
Scala. Though, for the purpose of designing the interface of our
library, we will restrict ourselves to fairly simple parsing tasks in
the following.

### Basics

We start with the simplest parsing task we can possibly imagine:
parsing a single character `c`. Thus, let us define a combinator that
given `c`, returns a parser that accepts only the input sequence
consisting of the single character `c`:

```scala
def char(c: Char): Parser[Char]
```

The type `Parser[A]` stands for a parser that provides functionality
for parsing the input character sequence and producing a result value
of type `A` if parsing succeeds. The type `A` could e.g. be the type
of an abstract syntax tree constructed from the input character
sequence. For `char(c)` the result of a successful parse is simply `c`
itself.

In general, we can thus think of `Parser[A]` as implementing a
function from input character sequences to values of type `A`. The
following trait captures this idea:

```scala
trait Parser[A] {
  def parse(loc: Location): A
}
```

Note that the function `parse` takes as input a `Location` instead of
a character sequence. We want to support parsing from different input
sources such as strings, files, etc. Moreover, since we construct
parsers compositionally from subparsers, a subparser must be able to
start parsing *"in the middle"* of the input sequence. Thus, think of
`Location` as abstracting both from the source of the input sequence
as well as from the way we keep track of the position where the parser
should start its parse within that input sequence.

It will be your responsibility to design the representation of the
type `Location` for different input sources. In the following we keep
`Location` abstract and only assume that we have an implicit
conversion from `String` and `Char` to `Location` that we can use to
parse a `String` or `Char`, respectively:

```scala
trait Location

object Location {
  implicit def fromString(s: String): Location = ???
  implicit def fromChar(c: Char): Location = fromString(c.toString)
}
```

#### Digression: Scala's `Try` Type

So far, we have assumed that parsing the input sequence will succeed
and produce a result of type `A`. However, what happens if the input
sequence cannot be parsed due to a parse error? For instance, suppose
we try to parse the string `"b"` using the parser `char('a')`:

```scala
char('a').parse("b")
```

What should happen in this case? We could simply throw an exception
inside `parse` indicating the parse error together with an appropriate
error message. However, exceptions are a heavyweight mechanism and
we'd like to keep our design free of side effects.

TODO: add discussion of `Try`

We can thus modify the signature of `parse` to return a `Try[A]`
instead of an `A` to indicate that parsing may fail with an exception
that we simply wrap in the return value:

```scala
trait Parser[A] {
  def parse(loc: Location): Try[A]
}
```

Now the client is free to either

1. throw the exception as a side-effect when calling `get` on the
result of a failed `parse`, or

1. continue working with the `Try[A]` in a side-effect free fashion
using `for` expressions as in our discussion above.

The following equation specifies our first algebraic law that we want
to hold for the combinator `char` and all `Char` values `c`:

```scala
char(c).parse(c).get === c
```

Let us also add our `char` combinator as an implicit factory method to
the companion object of type `Parser`:

```scala
object Parser {
  implicit def char(c: Char): Parser[Char]
}
```

Then we can simplify the above law further to the equation:

```scala
c.parse(c).get === c
```

by making the call to `char` implicit.

### An Algebra of Combinators

Parsing single characters is fairly boring. Let's take the complexity
of our parsing tasks to the next level: parsing sequences of *two*
characters. For instance, to obtain a parser for the sequence `"cd"`
we could compose the parser `char('c')` with the parser `char('d')`
using a form of product construction. The combinator `andThen`
generalizes this idea:

```scala
trait Parser[A] {
  ...
  def andThen[B](pb: Parser[B]): Parser[(A, B)]
}
```

Given two parsers `pa: Parser[A]` and `pb: Parser[B]`, the parser `pa
andThen pb` works by first applying `pa` to parse the first part of
the input sequence. If `pa` succeeds with some result value `a: A`, it
then applies `pb` to the remainder of the input. If `pb` also succeeds
with some result value `b: B`, then the composite parser returns the
pair `(a, b)`. If either `pa` or `pb` fails, then the composite parser
fails as well.

The following equation captures the successful cases for the product
composition of `char` parsers: for all `Char` values `c` and `d`

```scala
(c andThen d).parse(c.toString + d.toString) === (c, d)
```

Using repeated composition of parsers via `andThen` we can construct
parsers that can parse input sequences of fixed length. What
about sequences of unspecified length? A common scenario is that the
input sequence to be parsed consists of a repeated pattern that can be
parsed by the same subparser. Let's throw in a combinator for that:

```scala
def repeat: Parser[List[A]]
```

Given a parser `pa: Parser[A]`, the parser `pa.repeat` applies `pa` as
many times as needed to parse the input sequence, producing the list
of result values of the subparses if successful.

For instance, we should have

```scala
('a' andThen 'b').repeat.parse("abab") === List(('a', 'b'), ('a', 'b'))
```

Here is a more general law: for all `Int` values `n` and `Char` values `c`

```scala
c.repeat.parse(fill(n)(c).toString).get === fill(n)(c)
```

Let's also throw in a variant of `repeat` that succeeds if the pattern
accepted by the current parser repeats exactly `n` times for some
given `n`:

```scala
def repeat(n: Int): Parser[List[A]]
```

The corresponding variant of our previous law for `repeat` is as
follows: for all `n` and `c`

```scala
c.repeat(n).parse(fill(n)(c).toString).get === fill(n)(c)
```

So far, the parsers we have seen can only produce result values that
are tuples or lists build from `Char` values. That's not very
satisfactory. Suppose, e.g., that we want to write a parser that
accepts a sequence of `c` characters and returns the length of the
sequence as result. We could write a dedicated combinator for this
task, but that combinator would be fairly specific. Instead, why not
use the parser `char(c).repeat` and transform it in a way that it
returns an `Int` instead of a `List[Char]`. I know you saw ithis coming,
our old friend `map` is back:

```scala
trait Parser[A] {
  ...
  def map[B](f: A => B): Parser[B]
}
```

Given a parser `pa: Parser[A]` and a function `f: A => B`, the parser
`pa.map(f)` applies `pa` to parse the input sequence producing `a: A`
as an intermediate result value if successful, and then computes `f(a):
B` as the final result of the parse.

Here is a law capturing the example parsing task of counting the
number of `c` characters in an input sequence of `c`s:

```scala
(c.repeat map (_.size)).parse(fill(n)(c).toString) === n
```

and here is a more concrete test case:

```scala
('c'.repeat map (_.size)).parse("ccc") === 3
```

One important functionality that we are missing is to specify
alternatives between different subparsers. E.g., what if we want to
parse input sequences that consists of a `c` or a `d` character?
Clearly we need another combinator for that:

```scala
trait Parser[A] {
  ...
  def orElse(p: Parser[A]): Parser[A]
}
```

Given two parsers `pa1, pa2: Parser[A]` the parser `pa1 orElse pa2`
first applies `pa1` and returns its result on success or else applies
`pa2` on the *same* input sequence returning `pa2`'s result.

Here are two obvious laws for this combinator: for all `Char` values
`c` and `d`:

```scala
(c orElse d).parse(c).get === c
(c orElse d).parse(d).get === d
```

### Parsing Regular Expressions

TODO: example

Though, writing parsers for basic regular expressions this way is
quite cumbersome and inefficient. Scala already comes with an
efficient regular expression library, so we would like to take
advantage of that. Let's add a factory method that takes a regular
expression `r` and builds a parser that accepts inputs according to `r`
and returns the matched string as result:

```scala
object Parser {
  ...
  implicit def regex(r: Regex): Parser[String]
}
```

The implementation of the parser returned by `regex` should take
advantage of Scala's regular expression library to do the actual
matching on the input sequence.

In Scala, we can use the notation `s.r` to build a `Regex` object from
a string `s` that describes the regular expression. For instance, we
can use this feature to define a factory method `digit` that yields a
parser that accepts a digit and returns its `Int` value:

```scala
object Parser {
  ...
  def digit: Parser[Int] = regex("[0..9]".r) map (_.toInt)
}
```

### The Combinator to Rule all Combinators: `flatMap`

Parsing regular languages (and more generally context-free languages)
is fun, but what if we want to go beyond that? For instance, suppose
we want to parse a context-sensitive language such as all character
sequences of the form: *"a digit d, followed by a sequence of 'c's of
length d"*. To implement a parser for such a language we need to be
able to choose subparsers based on the intermediate results produced
by other subparsers. This leads us to the final combinator that we add
to our combinator algebra:

```scala
trait Parser[A] {
  ...
  def flatMap[B](f: A => Parser[B]): Parser[B]
}
```

That is, you can think of `flatMap` as a marriage between `andThen` and
`map` where the second subparser is computed from the result of the
first subparser.

Here is how we can use `flatMap` to describe a parser for the context-sensitive
language that we described above:

```scala
(digit flatMap ('a'.repeat(_))).parse("3aaa").get === List('a', 'a', 'a')
```

Many of the combinators that we have discussed so far can be
implemented using `flatMap`. For instance, here is how we can
implement `andThen` using `flatMap` and `map`:

```scala
def andThen[B](pb: Parser[B]): Parser[(A, B)] =
  this flatMap { a => pb map { b => (a, b) } }
```

By exploiting the fact that `for` expressions reduce to `flatMap` and
`map` calls, the above implementation of `andThen` can be simplified
to the following quite elegant code:

```scala
def andThen[B](pb: Parser[B]): Parser[(A, B)] =
  for {
    a <- this
    b <- pb
  } yield (a, b)
```

Feel free to add additional combinators and factory methods as you see
fit. Though, try to reuse existing combinators such as `flatMap` as
much as possible.

### Parse State



```scala
private[parsing] sealed trait ParseState[+A]{ val loc: Location }
```

```scala
trait Parser[A] {
  private[parsing] def apply(loc: Location): ParseState[A]
  
  def parse(loc: Location): Try[A] = 
    apply(loc) match { ... }

  def flatMap[B](f: A => Parser[B]): Parser[B] =
    // provide implementation of Parse[B].apply that 
    // takes care of all the plumbing
    { loc => apply(loc) match {
        ...
      }
    }
}
```

### Efficiency Considerations



```scala
def attempt[A](p: Parser[A]): Parser[A]
```

```scala
(attempt('a' andThen 'a') orElse ('a' andThen 'b')).parse("ab").get === ('a', 'b')
```

### Error Reporting
