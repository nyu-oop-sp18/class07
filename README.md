# Class 7

## Object-Functional Design

When you design a software component, it is helpful to start your
design by considering the interface of the component with its
environment (i.e. other components) before designing the component's
internal representation and implementation details. That is, you
should first answer questions such as

* What objects and methods the component should provide?

* What are the type signatures of these methods?

* What properties should the interactions between objects and their
methods satisfy?

* Which aspects of the internal state of the component should
be observable by the environment - if any?

* ...

These considerations put constraints on the possible implementations
of the component that inform your design process down the road and
help you catch design flaws early on.

In the context of object-functional design where objects have no
observable state, we can often think of a software component as an
*algebra* in the mathematical sense, i.e. as a set of objects equipped
with operations that satisfy certain algebraic laws. We have already
discussed monoids as one example of such an algebra. Identifying the
algebra behind a specific programming problem requires some thought
and practice. However, this effort is usually a good time investment
because a principled design approach tends to yield cleaner code that
is easier to generalize, extend, and maintain compared to code
obtained by an ad hoc design process.

We will practice object-functional design in a mini project whose goal
is to implement a parsing library. The purpose of today's class is to
brain storm about the basic design of such a library and to set some
"sign posts" for your implementation.

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

Ultimately, we want to be able to use this library to build simple
parser implementations for parsing arbitrarily complex languages such
as HTML or even Scala. For the purpose of designing the interface of
our library, we will restrict ourselves to fairly simple parsing tasks
in the following.

### Basics

We start with the simplest parsing task one can possibly imagine:
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
sequence. For `char(c)`, the result of a successful parse is simply
`c` itself.

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
(a string, a file, a stream, etc.) as well as from the way we keep
track of the position where the parser should start its parse within
that input sequence.

It will be your responsibility to design the representation of the
type `Location` for different input sources. In the following we keep
`Location` abstract and only assume that we have an implicit
conversion from `String` and `Char` to a `Location` representing the
start position of the `String` respectively `Char` to be parsed:

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
we'd also like to keep our library design free of side effects.

Scala provides the type `Try[A]`, which is quite helpful in this kind
of situation. This type implements a container that allows us to
perform a computation that potentially throws exceptions, while
containing either the result value of the computation or the exception
being thrown in the container.

Let's take a look at an example:

```scala
val t: Try[Int] = Try(1 / 0)
t: Try[Int]
```
Note that `1 / 0` throws an `ArithmeticException` due to the division
by 0. However, this exception is caught by the `Try` constructor and
contained within the resulting value `t`.

The `Try` type is implemented by two case classes:

* `Success`: which holds the result value of the computation if no
  exception has been thrown, and
  
* `Failure`: which holds the exception otherwise.

We can for instance now use pattern matching to inspect the outcome of
the computation and extract the value, or "release" the exception from
the `Try` container by throwing it:

```scala
val x: Int = t match {
  case Success(x) => x
  case Failure(ex) => throw x // Alternatively, return an Int to recover from ex
}
```

The same behavior as the above code can also be achieved more succinctly
by using the predefined method `get`:

```scala
val x = t.get
```

What makes the `Try` type interesting, is that it provides `map` and
`flatMap` combinators that allow us to continue to work with the
result value of the computation (in the successful case) in a *monadic
fashion*:

```scala
val t1: Try[Int] = for {
  x <- t
} yield (x + 1) * 3
```

The result `t1` of this `for` expression is `Success((x + 1) * 3)` if `t`
is `Success(x)` and `Failure(ex)` if `t` is `Failure(ex)`. If at any
point an exception is thrown within such a `for` expression operating
on a `Try` container, the exception is simply caught within the
container and propagated to the final resulting `Try` value. This way,
we can work with exceptions without having to deal with the side
effects of unstructured control flow when an exception is
thrown. Instead, exceptions are simply propagated along the normal
control flow of the program and can be dealt with at the point we deem
it to be necessary. The `Try` type is thus useful when we want to
write side-effect free code that needs to inter-operate with code that
can throw exceptions. By using `Try` we can then avoid cluttering our code
with `try/catch` blocks.

We modify the signature of `parse` to return a `Try[A]` instead of an
`A` to indicate that parsing may fail. Instead of throwing the
exception explicitly within the library, we simply wrap it in a `Try`
value at the end of the parse and return that to the client:

```scala
trait Parser[A] {
  def parse(loc: Location): Try[A]
}
```

If a client calls `parse` on a parser and the parser returns as
`Failure` to indicate a parse error, the client is free to either

1. try to recover from the error immediately (e.g. by providing a
   default value of type `A` for a failed parse),

1. throw the exception stored in the `Failure` immediately to escalate
   the error, or

1. delay the handling of the exception and continue working with the
`Try[A]` in a side-effect free fashion (using `for` expressions as in
our discussion above).

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
parsed by the same subparser. Let's throw in a combinator for that in
our companion object:

```scala
def repeat[A](p: Parser[A]): Parser[List[A]]
```

Given a parser `pa: Parser[A]`, the parser `repeat(a)` applies `pa` as
many times as needed to parse the input sequence, producing the list
of result values of the subparses if successful.

For instance, we should have

```scala
repeat('a' andThen 'b').parse("abab") === List(('a', 'b'), ('a', 'b'))
```

Here is a more general law which states that parsing a list of `c`
characters of length `n` using `repeat(c)` gives us back the same
list: for all `Int` values `n` and `Char` values `c`

```scala
repeat(c).parse(fill(n)(c).toString).get === fill(n)(c)
```

Note that `fill(n)(c)` creates a list of length `n` that contains `n`
copies of character `c`, which we then concatenate to the `String` to
be parsed.

Let's also throw in a variant of `repeat` that succeeds if the pattern
accepted by the current parser repeats exactly `n` times for some
given `n`:

```scala
def repeatN[A](n: Int)(p: Parser[A]): Parser[List[A]]
```

The corresponding variant of our previous law for `repeat` is as
follows: for all `n` and `c`

```scala
repeatN(n)(c).parse(fill(n)(c).toString).get === fill(n)(c)
```

So far, the parsers we have seen can only produce result values that
are tuples or lists built from `Char` values. That's not very
satisfactory. Suppose, e.g., that we want to write a parser that
accepts a sequence of `c` characters and returns the length of the
accepted sequence as result. We could write a dedicated combinator for
this task, but that combinator would be fairly specific. Instead, why
not use the parser `repeat(char(c))` and transform it in a way that it
returns an `Int` instead of a `List[Char]`? I know you saw this
coming, our old friend `map` is back:

```scala
trait Parser[A] {
  ...
  def map[B](f: A => B): Parser[B]
}
```

Given a parser `pa: Parser[A]` and a function `f: A => B`, the parser
`pa.map(f)` applies `pa` to parse the input sequence producing `a: A`
as an intermediate result value if successful, and then computes `f(a):
B` as the final result value of the parse.

Here is a law capturing the example parsing task of counting the
number of `c` characters in an input sequence of `c`s:

```scala
(repeat(c) map (_.size)).parse(fill(n)(c).toString) === n
```

and here is a more concrete test case:

```scala
(repeat('c') map (_.size)).parse("ccc") === 3
```

One important functionality that we are still missing is to specify
alternatives between different subparsers. E.g., what if we want to
parse input sequences that consist of a `c` or a `d` character?
Clearly we need another combinator for that:

```scala
trait Parser[A] {
  ...
  def orElse(p: Parser[A]): Parser[A]
}
```

Given two parsers `pa1, pa2: Parser[A]` the parser `pa1 orElse pa2`
first applies `pa1` and returns its result on success. If `pa1`
failrs, the composite parser applies `pa2` on the *same* input
sequence as `pa1` and returns `pa2`'s result.

Here are two obvious laws for this combinator: for all `Char` values
`c` and `d`:

```scala
(c orElse d).parse(c).get === c
(c orElse d).parse(d).get === d
```

### Parsing Regular Expressions

Using the combinators we have designed so far, we can easily express
parsers for regular languages. For instance, here is a parser that
parses arbitrarily long sequences of digit characters (as described by
the regular expression `"[0-9]*"`). The parser then computes the `Int`
value encoded in the decimal representation:

```scala
def digit: Parser[Int] = ('0' to '9') reduce (_ orElse _) map (_.toString.toInt)
def digits: Parser[Int] = (digit andThen repeat(digit)) map { case (d, ds) => (d :: ds) reduce (_ * 10 + _) }
```

It's instructive to go through these two definitions step by step. For
`digit`, we first create a `Seq[Char]` containing the sequence of
character values from `'0'` to `'9'`. From this `Seq[Char]`, we then
produce a `Parser[Char]` that accepts either of these 10 possible
digit characters and returns the corresponding character upon
success. We do this by reducing the `Seq[Char]` using a function that
consecutively maps each `Char` in the sequence to a `Parser[Char]`
(via an implicit conversion) and then combines it with its predecessor
via the `orElse` combinator. Now we only need to convert the resulting
`Parser[Char]` into a `Parser[Int]` using the `map` combinator. The
function applied in the `map` takes the result `Char` produced by the
`Parser[Char]` and converts it into the represented `Int` value. We do this
by converting the `Char` first to a `String` and then to an `Int`. If we
call `toInt` directly on the `Char` value, we get back its ASCII code,
which is not what we want.

The definition of `digits` is similar. The parser `(digit andThen
repeat(digit))` accepts non-empty sequences of digit characters and
produces a `(Int, List[Int])` corresponding to the `Int` values
represented by each digit in the parsed sequence. The first component
is the first digit of the sequence, and the second component holds the
list of the remaining digits. The resulting parser is thus of type
`Parser[(Int, List[Int])]` and we convert to a `Parser[Int]` using
`map`. The function applied in the `map` takes the pair `(d, ds):
(Int, List[Int])`, produced by the intermediate `Parser[(Int, List[Int])]`,
and converts it into a single `Int` value according to the decimal
representation encoded in the list `d :: ds`.

Writing parsers for basic regular expressions in this way is a bit
cumbersome. We could define additional combinators for common types of
regular expressions to make this more pleasant, but ultimately
handling regular expressions using our combinators will be fairly
inefficient.

Like most languages, Scala already comes with an efficient regular
expression library, so we would like to take advantage of that
functionality. Let's add a factory method that takes a regular
expression `r` and builds a parser that accepts inputs according to
`r` and returns the matched string as result:

```scala
object Parser {
  ...
  implicit def regex(r: Regex): Parser[String]
}
```

The implementation of the parser returned by `regex` should take
advantage of Scala's regular expression library to do the actual
matching of `r` against the input sequence.

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

### Efficiency Considerations

The `orElse` operator introduces nondeterminism into our parser
logic. To implement this nondeterminism, we may need to backtrack on
the input sequence, discarding a partial unsuccessful parse. This can
be quite costly in practice. For instance, consider the parser

```scala
('a' andThen 'b') orElse ('a' andThen 'a')
```

Should this parser succeed on the input sequence `"aa"`? Intuitively,
the answer is "yes" because the second subparser of `orElse` accepts
this sequence. However, the problem is that the `'a'` portion of the
first subparser matches the first character of `"aa"`. Thus, when the
first subparser fails on parsing the second `'a'` of the input, it has
already consumed the first `'a'`. To be able to accept the input with
the second subparser, we need to backtrack to the beginning of the
input sequence and then retry. If we have many `orElse` combinations,
this may lead to an unacceptable running time of the parser.

Of course, we can implement our parsers more carefully. For instance,
the above parser can be rewritten as

```scala
'a' andThen ('b' orElse 'a')
```

which avoids the backtracking. However, this approach may make the
parser implementation harder to read and is generally not possible if
we want to build complex parsers compositionally from simpler building
blocks.

To solve this issue, we make a design choice and let `orElse` commit
to the left subparser as soon as that subparser has consumed at least
one input character. If the committed subparser fails, the `orElse`
parser should itself fail without trying its right subparser. In
particular, the following should hold:

```scala
(('a' andThen 'b') orElse ('a' andThen 'a')).parse("aa").isFailure
```

Still, we want to be able to express parsers that can parse `"ab"` or
`"aa"` using backtracking as described above. However, for efficiency
reasons we don't want to make backtracking the default behavior of
`orElse`. To control whether a parser is backtrackable, we add an
additional combinator:

```scala
def attempt[A](p: Parser[A]): Parser[A]
```

The parser `attempt(p)` should behave like `p`, except that if `p`
fails, its returned parse state is reset to an uncommitted state,
indicating that it is backtrackable. In particular, we should have:

```scala
(attempt('a' andThen 'b') orElse ('a' andThen 'a')).parse("aa").get === ('a', 'a')
```

### One Combinator to Rule them all: `flatMap`

Parsing regular languages (and more generally context-free languages)
is fun, but what if we want to go beyond that? For instance, suppose
we want to implement a parser for a context-sensitive language such as
the one consisting of all character sequences of the form: *"a digit
d, followed by a sequence of 'c's of length d"*. Example strings of
this language are `"1c"` and `"3ccc"`. To implement a parser for such
languages, we need to be able to choose subparsers based on the
intermediate results produced by other subparsers. This leads us to
the final combinator that we add to our combinator algebra:

```scala
trait Parser[A] {
  ...
  def flatMap[B](f: A => Parser[B]): Parser[B]
}
```

Given a parser `pa: Parser[A]` and a function `f: A => Parser[B]`, the
parser `pa.flatMap(f)` first applies `pa` to the input sequence
producing a partial parse with intermediate result `a: A` if
successful. Using `a` it then constructs the parser `f(a): Parser[B]`
and uses this subparser to parse the remaining input. `f(a)` then
produces the final result of type `B`. If `pa` or `f(a)` fail then so
does `pa.flatMap(f)`.

That is, you can think of `flatMap` as a marriage between `andThen` and
`map` where the right subparser is computed from the result of the
left subparser using the function `f`.

Here is how we can use `flatMap` to implement a parser for the
context-sensitive language that we described above:

```scala
(digit flatMap (repeat(_)('a'))).parse("3aaa").get === List('a', 'a', 'a')
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
much as possible. E.g. it is a good exercise to think about how `map`
and `orElse` can be implemented with `flatMap` to better understand
how `flatMap` can be used (even if you decide to implement these
combinators more directly in the end).


### Parse State

Let us now move on to some design considerations related to the
internal implementation of our library.

Clearly, in order to implement combinators such as `flatMap` and
`orElse`, we need to keep track of some information about the state of
the parser in addition to information about the result value and
whether the parse has been successful. For instance, `flatMap` needs
to know the location in the input sequence that has been reached after
the first subparser has completed and control is handed over to the
second subparser. Similarly, `orElse` needs to know whether the first
subparser has committed to the input sequence after completing its
parse in order to decide whether it should backtrack and invoke the
second subparser.

How shall we keep track of the parse state? We want to maintain a
clean functional design that does not rely on mutable
state. Otherwise, we might break the algebraic properties of our
combinators. 

The key insight is to think about a parser more generally as a
function that takes a location in the input sequence and produces a
parse state as a result. The resulting parse state captures 

* whether the parser succeeded, 

* the result value on success, 

* what location the parser has reached after completing its parse,
  etc.

In particular, even upon a successful parse, the parser may only have
consumed a prefix of the input sequence starting from the given
location.

The different parser combinators then thread the parse state from one
subparser to the next. A parse state indicating a failed parse is
simply propagated through subsequent subparsers, unless it is wrapped
in an `attempt` parser that is handled by `orElse`.

Let's conjure up a type `ParseState[A]` that represents the parse
state of our parser and update `Parser` appropriately:

```scala
trait Parser[A] {
  protected def apply(loc: Location): ParseState[A]
  
  def parse(loc: Location): Try[A] = 
    // run this parser using `apply` and check whether the final state
    // indicates success and consummation of the entire input
    apply(loc) match { ... }

  def flatMap[B](f: A => Parser[B]): Parser[B] =
    // Provide implementation of `apply` method that 
    // takes care of the necessary plumbing for the logic of 
    // the Parser[B] returned by flatMap.
    { loc => apply(loc) match {
        ...
      }
    }
    
  ...
}
```

Note that we have introduced an abstract method `apply` to `Parser`
that will implement the logic of each specific parser. It is the only
abstract method of `Parser`. That is, combinators such as `flatMap`
only need to implement the `apply` method for the new `Parser` object
being returned by the combinator.

The actual type for parse state could look like this:

```scala 
private[parsing] sealed trait ParseState[A]
``` 

You will need to keep track of certain information in the parse state
and it is your job to design the details of the implementation. For
instance, you could have separate derived case classes for
representing the parse states of successful and failing parses.

### Error Reporting

One important aspect of the design of our parser library that we
haven't talked about yet is error reporting.

We want to extend our library with the capability of tagging parsers
with informative error messages that explain the parse error.

For instance, suppose we implement a parser that parses Scala code and
suppose we apply it to a syntactically incorrect `val` declaration like this:

```scala
val 0 = 1
```

The parser should produce an exception indicating the position in the
input sequence where the parse error occurred (e.g. line and column
number etc.), together with a detailed error message that explains the
context of the error.

For instance, in the above example the parser will fail when it reads
the character `0` as it expects the beginning of an identifier, which
must start with a letter or underscore character. Thus, a low level
error message that the parser could produce at this point could be

```"Found '0' but expected letter or underscore"```

However, this information alone is not very helpful. The parser could
provide additional information about the context where the error
occurred. In this case, the subparser that parses the first character of
the identifier could be part of a composite parser for parsing an
identifier. This composite parser could augment the error message provided
by the subparser with the information

```" while parsing identifier"```

Similarly, the parser for the identifier could be again a subparser of
another composite parser for `val` declarations. The latter could
further augment the error message by adding

```" in val declaration"```.

So the final error message produced by our parser could look like
this:

```Error (1,4): Found '0' but expected letter or underscore while parsing identifier in val declaration```

To support this kind of error reporting functionality we add two more
combinators to our library. The first one is

```scala
def label[A](msg: String)(p: Parser[A]): Parser[A]
```

The parser `label(msg)(p)` should behave like `p`. However, if `p`
fails, then the error message contained in the error state produced by
`p` should be replaced by `msg`.

In addition, we add a second combinator related to error reporting
that enables us to *tag* the error message of a subparser with
additional information:

```scala
def tag[A](msg: String)(p: Parser[A]): Parser[A]
```

The parser `tag(msg)(p)` should again behave like `p`. However, if `p`
fails, then the error message in `p`'s final error state should be
extended with `msg`.
