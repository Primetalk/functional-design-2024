package net.degoes

/*
 * INTRODUCTION
 *
 * In Functional Design, type safety of data models can be greatly improved by
 * making them statically typed.
 */

/** EXECUTABLE - EXERCISE SET 1
  *
  * Consider an application (such as the spreadsheet example) that needs to calculate values in a
  * user-defined way.
  */
object executable_typed:
  trait Spreadsheet:
    def cols: Int
    def rows: Int

    def valueAt(col: Int, row: Int): CalculatedValue[Any]

    final def scan(range: Range): LazyList[Cell[Any]] =
      val minRow = range.minRow.getOrElse(0)
      val maxRow = range.maxRow.getOrElse(rows - 1)

      val minCol = range.minCol.getOrElse(0)
      val maxCol = range.maxCol.getOrElse(cols - 1)

      (for
        col <- (minCol to maxCol).to(LazyList)
        row <- (minRow to maxRow).to(LazyList)
      yield Cell(col, row, valueAt(col, row)))

  final case class Range(
    minRow: Option[Int],
    maxRow: Option[Int],
    minCol: Option[Int],
    maxCol: Option[Int]
  )
  object Range:
    def column(i: Int): Range = Range(None, None, Some(i), Some(i))

    def row(i: Int): Range = Range(Some(i), Some(i), None, None)

  final case class Cell[A](col: Int, row: Int, contents: CalculatedValue[A])

  /** EXERCISE 1
    *
    * Design a data type called `CalculatedValue[A]`, whose type parameter `A` represents the type
    * of value dynamically computed from the spreadsheet.
    */
  final case class CalculatedValue[+A](eval: Spreadsheet => A):
    self =>

    /** EXERCISE 2
      *
      * Add an operator that returns a new `CalculatedValue` that is the negated version of this
      * one.
      */
    def unary_-[A1 >: A](using Numeric[A1]): CalculatedValue[A1] = 
      CalculatedValue(s => summon[Numeric[A1]].negate(eval(s)))

    /** EXERCISE 3
      *
      * Add a binary operator `+` that returns a new `CalculatedValue` that is the sum of the two
      * calculated values.
      */
    def +[A1 >: A](that: CalculatedValue[A1])(using Numeric[A1]): CalculatedValue[A1] =
      CalculatedValue(s => summon[Numeric[A1]].plus(eval(s), that.eval(s)))

    /** EXERCISE 4
      *
      * Add a binary operator `-` that returns a new `CalculatedValue` that is the difference of the
      * two calculated values.
      */
    def -[A1 >: A](that: CalculatedValue[A1])(using Numeric[A1]): CalculatedValue[A1] =
      CalculatedValue(s => summon[Numeric[A1]].minus(eval(s), that.eval(s)))

    protected def binaryOp[A1 >: A](that: CalculatedValue[A1])(error: String)(
      f: PartialFunction[(A1, A1), A1]
    ): CalculatedValue[A1] = 
      CalculatedValue(s => f(eval(s), that.eval(s)))
  end CalculatedValue
  object CalculatedValue:

    /** EXERCISE 5
      *
      * Add a constructor that makes an `CalculatedValue` from a `Value`.
      */
    def const[Value](contents: Value): CalculatedValue[Value] = CalculatedValue(_ => contents)

    /** EXERCISE 6
      *
      * Add a constructor that provides access to the value of the specified cell, identified by
      * col/row.
      */
    def at(col: Int, row: Int): CalculatedValue[Any] = CalculatedValue(s => s.valueAt(col, row).eval(s))
end executable_typed

/** EXECUTABLE - EXERCISE SET 2
  *
  * Consider an application (such as the spreadsheet example) that needs to calculate values in a
  * user-defined way.
  */
object declarative_typed:
  trait Spreadsheet:
    def cols: Int
    def rows: Int

    def valueAt(col: Int, row: Int): CalculatedValue[Any]

    final def scan(range: Range): LazyList[Cell[Any]] =
      val minRow = range.minRow.getOrElse(0)
      val maxRow = range.maxRow.getOrElse(rows - 1)

      val minCol = range.minCol.getOrElse(0)
      val maxCol = range.maxCol.getOrElse(cols - 1)

      (for
        col <- (minCol to maxCol).to(LazyList)
        row <- (minRow to maxRow).to(LazyList)
      yield Cell(col, row, valueAt(col, row)))

  final case class Range(
    minRow: Option[Int],
    maxRow: Option[Int],
    minCol: Option[Int],
    maxCol: Option[Int]
  )
  object Range:
    def column(i: Int): Range = Range(None, None, Some(i), Some(i))

    def row(i: Int): Range = Range(Some(i), Some(i), None, None)

  final case class Cell[A](col: Int, row: Int, contents: CalculatedValue[A])

  enum Operation:
    case Plus
    case Minus

  /** EXERCISE 1
    *
    * Design a data type called `CalculatedValue[A]`, whose type parameter `A` represents the type
    * of value dynamically computed from the spreadsheet.
    */
  enum CalculatedValue[+A]:
    case Integer(value: Int) extends CalculatedValue[Int]
    case Str(value: String)  extends CalculatedValue[String]
    case Negate(other: CalculatedValue[A])
    case BinOp[A](op: Operation, num: Numeric[A], a: CalculatedValue[A], b: CalculatedValue[A]) extends CalculatedValue[A]
    case Concat(a: CalculatedValue[String], b: CalculatedValue[String]) extends CalculatedValue[String]
    case Error(error: String) extends CalculatedValue[Nothing]
    case BinOpArbitrary[A](f: PartialFunction[(A, A), A], error: String, a: CalculatedValue[A], b: CalculatedValue[A]) extends CalculatedValue[A]

    def self = this

    /** EXERCISE 2
      *
      * Add an operator that returns a new `CalculatedValue` that is the negated version of this
      * one.
      */
    def unary_-[A1 >: A](using Numeric[A1]): CalculatedValue[A1] = 
      Negate(this)
    

    /** EXERCISE 3
      *
      * Add a binary operator `+` that returns a new `CalculatedValue` that is the sum of the two
      * calculated values.
      */
    def +[A1 >: A](that: CalculatedValue[A1])(using Numeric[A1]): CalculatedValue[A1] =
      BinOp(Operation.Plus, summon[Numeric[A1]], this, that)

    // def ++[A1 >: String](that: CalculatedValue[A1])(using A <:< String) = 
    //   BinOp(Operation.Plus, this, that)

    /** EXERCISE 4
      *
      * Add a binary operator `-` that returns a new `CalculatedValue` that is the difference of the
      * two calculated values.
      */
    def -[A1 >: A](that: CalculatedValue[A1])(using Numeric[A1]): CalculatedValue[A1] =
      BinOp(Operation.Minus, summon[Numeric[A1]], this, that)        

    protected def binaryOp[A1 >: A](that: CalculatedValue[A1])(error: String)(
      f: PartialFunction[(A1, A1), A1]
    ): CalculatedValue[A1] = 
      BinOpArbitrary(f, error, this, that)
        
  end CalculatedValue
  object CalculatedValue:

    /** EXERCISE 5
      *
      * Add a constructor that makes an `CalculatedValue` from a `Value`.
      */
    def const[Value](contents: Value): CalculatedValue[Value] = ???

    /** EXERCISE 6
      *
      * Add a constructor that provides access to the value of the specified cell, identified by
      * col/row.
      */
    def at(col: Int, row: Int): CalculatedValue[Any] = ???

  import CalculatedValue.*

  def calculate[A](expr: CalculatedValue[A]): A =
    expr match
      case Integer(v) => v
      case Str(v)     => v
      case Error(error) => throw RuntimeException(error)
      case Negate(other) => calculate(other) match
        case i: Int => (-i).asInstanceOf[A]
        case v => throw IllegalArgumentException(s"Cannot negate $v")
      case BinOpArbitrary(f, error, a, b) => 
        val aa = calculate(a)
        val bb = calculate(b)
        f.lift(aa, bb) match 
          case Some(a) => a
          case None    => throw RuntimeException(error)
      case BinOp(op, num, a, b) => 
        val aa = calculate(a)
        val bb = calculate(b)
        op match
          case Operation.Plus  => num.plus(aa, bb)
          case Operation.Minus => num.minus(aa, bb)
      case CalculatedValue.Concat(a, b) =>
        val aa = calculate(a)
        val bb = calculate(b)
        aa + bb
        
end declarative_typed

/** PARSERS - GRADUATION PROJECT
  */
object parser:
  type Error = String
  type Input = String

  // `Parser[A]` is a model of a series of parse operations that consume
  // characters and ultimately use the consumed input construct a value of
  // type `A`.
  enum Parser[+A]:
    case Succeed[A](a: A)                                                extends Parser[A]
    case Fail(error: Error)                                              extends Parser[Nothing]
    case OneChar                                                         extends Parser[Char]
    case Map[A, B](parser: Parser[A], f: A => B)                         extends Parser[B]
    case Repeat[A](value: Parser[A], min: Option[Int], max: Option[Int]) extends Parser[List[A]]
    case Sequence[A, B](parserA: Parser[A], parserB: Parser[B])          extends Parser[(A, B)]
    case OrElse[A, B](parserA: Parser[A], parserB: Parser[B])            extends Parser[Either[A, B]]
    
    def self = this

    def atLeast(n: Int): Parser[List[A]] = Parser.Repeat(self, Some(n), None)

    def atMost(n: Int): Parser[List[A]] = Parser.Repeat(self, None, Some(n))

    def between(min: Int, max: Int): Parser[List[A]] = Parser.Repeat(self, Some(min), Some(max))

    def map[B](f: A => B): Parser[B] = Parser.Map(self, f)

    def ~>[B](that: Parser[B]): Parser[B] = (self ~ that).map(_._2)

    def <~[B](that: Parser[B]): Parser[A] = (self ~ that).map(_._1)

    def ~[B](that: Parser[B]): Parser[(A, B)] = Sequence(this, that) // EXERCISE 3 - Parser.Sequence(self, that)

    def |[A1 >: A](that: Parser[A1]): Parser[A1] = Map(OrElse(this, that), e => e.fold(identity,identity)) // EXERCISE 4 - Parser.OrElse(self, that)

    def * : Parser[List[A]] = Parser.Repeat(self, None, None)

    def + : Parser[List[A]] = atLeast(1)
  end Parser
  object Parser:

    /** EXERCISE 1
      *
      * Add a constructor that models the production of the specified value (of any type at all),
      * without consuming any input.
      *
      * NOTE: Be sure to modify the `parse` method below, so that it can handle the new operation.
      */
    // final case class Succeed()

    /** EXERCISE 2
      *
      * Add a constructor that models failure with a string error message.
      *
      * NOTE: Be sure to modify the `parse` method below, so that it can handle the new operation.
      */
    // final case class Fail()

    /** EXERCISE 3
      *
      * Add an operator that can try one parser, but if that fails, try another parser.
      *
      * NOTE: Be sure to modify the `parse` method below, so that it can handle the new operation.
      */
    // final case class OrElse()

    /** EXERCISE 4
      *
      * Add an operator that parses one thing, and then parses another one, in sequence, producing a
      * tuple of their results.
      *
      * NOTE: Be sure to modify the `parse` method below, so that it can handle the new operation.
      */
    // final case class Sequence[A, B]()
  end Parser

  import Parser.*

  def parse[A](parser: Parser[A], input: Input): Either[Error, (Input, A)] =
    parser match
      case OneChar =>
        input.headOption
          .map((a: Char) => Right(input.drop(1) -> a))
          .getOrElse(Left("Expected a character but found an empty string"))

      case Map(parser, f) =>
        parse(parser, input).map:
          case (input, a) => (input, f(a))

      case repeat: Repeat[a] =>
        val min = repeat.min.getOrElse(0)
        val max = repeat.max.getOrElse(Int.MaxValue)

        (min to max)
          .foldLeft[Either[Error, (Input, List[a])]](Right((input, Nil))):
            case (e @ Left(_), _) => e

            case (Right((input, as)), _) =>
              parse[a](repeat.value, input) match
                case Left(error)       => if as.length >= min then Right((input, as)) else Left(error)
                case Right((input, a)) => Right((input, a :: as))
          .map:
            case (input, as) => (input, as.reverse)

      case Succeed(a) => Right((input, a))
      case Fail(error) => Left(error)
      case Sequence(parserA, parserB) => 
        parse(parserA, input) match
          case Left(error) => Left(error)
          case Right((input, a)) => 
            parse(parserB, input)
              .map{ case (input, b) => (input, (a, b)) }
      case OrElse(parserA, parserB) => 
        parse(parserA, input) match
          case Left(error) => parse(parserB, input).map{ case (input, b) => (input, Right(b)) }
          case Right((input, a)) => Right((input, Left(a)))      
end parser
