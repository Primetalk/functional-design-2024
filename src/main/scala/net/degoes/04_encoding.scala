package net.degoes

import net.degoes.spreadsheet2.CalculatedValue.at

/*
 * INTRODUCTION
 *
 * In Functional Design, there are two ways to represent models:
 *
 * 1. Using a function or interface, whose methods execute the solution. This is
 *    called the "executable" encoding in this course. It's a direct, executable
 *    encoding of a domain. If some functional domain is modeled with a class
 *    or case class, or an open trait that is implemented by classes, then it's
 *    probably an executable encoding.
 *
 * 2. Using a pure data structure, which declaratively describes the solution, but
 *    which does not perform the solution. It's an abstract, "declarative"
 *    encoding of a domain. If some functional domain type is modeled with a
 *    enum, then it's probably a declarative encoding, where the subtypes
 *    of the enum model individual operations and constructors in the
 *    domain.
 *
 * In the second encoding, a so-called "executor" or "interpreter" or "compiler"
 * translates the data structure, which merely models a solution, into either
 * executable code or into another lower-level domain, which provides the
 * capabilities modeled by the functional domain.
 *
 * Executable encodings are "open" for new constructors and operators: anyone
 * can add new constructors and operators, without updating existing code. On
 * the other hand, executable encodings are not "introspectable": because they
 * are not data, but rather, opaque executable machinery, it is not possible to
 * add new ways to execute the models without rewriting all constructors and
 * operators.
 *
 * Declarative encodings are "closed" for new constructors and operators: no
 * one can add new constructors and operators, without updating existing code.
 * Yet, because they are pure data, it is easy to add new ways to execute the
 * models, for example, serializers, optimizers, converters, and so forth,
 * assuming their component parts have the same properties (not all
 * declarative encodings do; if you embed a function inside a declarative
 * encoding, it becomes opaque).
 *
 * Summarizing the difference between executable and declarative encodings:
 *
 *  - Executable encodings have unbounded constructors/operators, but a fixed
 *    number of ways to execute them.
 *  - Declarative encodings have fixed constructors/operators, but an unbounded
 *    number of ways to execute them.
 *
 * Note: Tagless-final is an executable encoding, but one where, by making the
 * "solutions" polymorphic, the choice of executor can be deferred arbitrarily.
 *
 * Legacy code prefers executable encodings; while many benefits of Functional
 * Design can be seen best using abstract encodings.
 *
 */

/** EDUCATION - EXERCISE SET 1
  *
  * Consider a console-based educational application that tests the user's knowledge of key
  * concepts.
  */
object education_executable:
  import education.*

  enum Quiz2:
    case SingleQuestionQuiz[A](q: Question[A], isBonus: Boolean)
    case Appends(quizes: Quiz2*)

    def self = this

    /** EXERCISE 1
      *
      * Add an operator `+` that appends this quiz to the specified quiz. Model this as pure data
      * using a constructor for Quiz in the companion object.
      */
    def +(that: Quiz2): Quiz2 =
      (this, that) match
        case (s1 @ SingleQuestionQuiz(_, _), s2 @ SingleQuestionQuiz(_, _)) =>
          Appends(s1, s2)
        case (Appends(quizes1*), Appends(quizes2*))                         =>
          Appends((quizes1 ++ quizes2)*)
        case (Appends(quizes1*), s2)                                        =>
          Appends((quizes1 :+ s2)*)
        case (s1, Appends(quizes2*))                                        =>
          Appends((s1 +: quizes2)*)

    /** EXERCISE 2
      *
      * Add a unary operator `bonus` that marks this quiz as a bonus quiz. Model this as pure data
      * using a constructor for Quiz in the companion object.
      */
    def bonus: Quiz2 =
      this match
        case SingleQuestionQuiz(q, _) => SingleQuestionQuiz(q, isBonus = true)
        case Appends(quizes*)         =>
          Appends(quizes.map(_.bonus)*)
  end Quiz2

  object Quiz2:
    def apply[A](question: Question[A]): Quiz2 =
      SingleQuestionQuiz(question, false)

  private def grade[A](f: String => A, checker: Checker[A]): QuizResult =
    Quiz.grade(f, checker)

  /** EXERCISE 3
    *
    * Implement an interpreter for the `Quiz` model that translates it into the interactive console
    * operations that it describes, returning a QuizResult value.
    */
  def run(quiz: Quiz2): QuizResult =
    quiz match
      case Quiz2.SingleQuestionQuiz(q, isBonus) =>
        println(q.question)
        q match
          case Question.Text(question, checker)                    => grade(identity(_), checker)
          case Question.MultipleChoice(question, choices, checker) =>
            val choicePrintout =
              choices.zipWithIndex.map { case (c, i) => s"${i}. ${c}" }.mkString("\n")

            println("Your options are: \n" + choicePrintout)

            grade(_.toInt, checker)
          case Question.TrueFalse(question, checker)               =>
            grade(_.toLowerCase().startsWith("t"), checker)

      case Quiz2.Appends(quizes*) =>
        quizes.map(run).foldLeft(QuizResult.empty)(_ + _)

end education_executable

/** DATA TRANSFORM - EXERCISE SET 2
  *
  * Consider an email marketing platform, which allows users to upload contacts.
  */
object contact_processing2:
  import contact_processing.*

  enum Operation:
    case Rename(oldName: String, newName: String)
    case Delete(name: String)

  enum SchemaMapping2:
    case Sequence(mappings: SchemaMapping2*)
    case Alternative(mappings: SchemaMapping2*)
    case SingleOperation(op: Operation)

    /** EXERCISE 1
      *
      * Add a `+` operator that models combining two schema mappings into one, applying the effects
      * of both in sequential order.
      */
    def +(that: SchemaMapping2): SchemaMapping2 =
      (this, that) match
        case (Sequence(mappings1*), Sequence(mappings2*)) =>
          Sequence(mappings1 ++ mappings2*)
        case (Sequence(mappings1*), s2)                   =>
          Sequence(mappings1 :+ s2*)
        case (s1, Sequence(mappings2*))                   =>
          Sequence(s1 +: mappings2*)
        case (s1, s2)                                     =>
          Sequence(s1, s2)

    /** EXERCISE 2
      *
      * Add an `orElse` operator that models combining two schema mappings into one, applying the
      * effects of the first one, unless it fails, and in that case, applying the effects of the
      * second one.
      */
    def orElse(that: SchemaMapping2): SchemaMapping2 =
      (this, that) match
        case (Alternative(mappings1*), Alternative(mappings2*)) =>
          Alternative(mappings1 ++ mappings2*)
        case (Alternative(mappings1*), s2)                      =>
          Alternative(mappings1 :+ s2*)
        case (s1, Alternative(mappings2*))                      =>
          Alternative(s1 +: mappings2*)
        case (s1, s2)                                           =>
          Alternative(s1, s2)
  end SchemaMapping2

  object SchemaMapping2:

    /** EXERCISE 3
      *
      * Add a constructor for `SchemaMapping` models renaming the column name.
      */
    def rename(oldName: String, newName: String): SchemaMapping2 =
      SchemaMapping2.SingleOperation(Operation.Rename(oldName, newName))

    /** EXERCISE 4
      *
      * Add a constructor for `SchemaMapping` that models deleting the column of the specified name.
      */
    def delete(name: String): SchemaMapping2 =
      SchemaMapping2.SingleOperation(Operation.Delete(name))

  /** EXERCISE 5
    *
    * Implement an interpreter for the `SchemaMapping` model that translates it into into changes on
    * the contact list.
    */
  def run(mapping: SchemaMapping2, contacts: ContactsCSV): MappingResult[ContactsCSV] =
    mapping match
      case SchemaMapping2.Sequence(mappings*)    =>
        mappings.foldLeft(MappingResult.Success(contacts, Nil))((contacts, mapping) =>
          contacts.flatMap(run(mapping, _))
        )
      case SchemaMapping2.Alternative(mappings*) =>
        mappings.foldLeft(MappingResult.Failure[ContactsCSV](Nil))((res, mapping) =>
          res.orElse(run(mapping, contacts))
        )
      case SchemaMapping2.SingleOperation(op)    =>
        op match
          case Operation.Rename(oldName, newName) =>
            MappingResult.Success(contacts.rename(oldName, newName), Nil)
          case Operation.Delete(name)             =>
            MappingResult.Success(contacts.delete(name), Nil)

  /** BONUS EXERCISE
    *
    * Implement an optimizer for the `SchemaMapping` model that pushes deletes to the front of the
    * schema mapping in cases where doing so wouldn't later the result.
    */
  def optimize(schemaMapping: SchemaMapping2): SchemaMapping2 =
    schemaMapping match
      case SchemaMapping2.Sequence(mappings*)    =>
        SchemaMapping2.Sequence(
          optimizeSequence(mappings.toList)*
        )
      case SchemaMapping2.Alternative(mappings*) =>
        SchemaMapping2.Alternative(mappings.map(optimize)*)
      case s                                     => s

  def optimizeSequence(
    lst: List[SchemaMapping2],
    accum: List[SchemaMapping2] = Nil
  ): List[SchemaMapping2] =
    lst match
      case Nil                                                                => accum.reverse
      case s @ SchemaMapping2.SingleOperation(Operation.Delete(name)) :: tail =>
        optimizeSequence(tail, insertDeleteIntoReverseSequence(name, accum, Nil).reverse)
      case SchemaMapping2.Alternative(mappings*) :: tail                      =>
        optimizeSequence(tail, SchemaMapping2.Alternative(mappings.map(optimize)*) :: accum)
      case head :: tail                                                       =>
        optimizeSequence(tail, head :: accum)

  /** Добавляем удаление колонки в цепочку отображений. Фактически удаление колонки добавляется в
    * начало всех шагов. При этом может быть, будет удалена другая колонка, если было
    * переименование.
    *
    * @param name
    *   \- колонка, которую надо удалить.
    * @param reverseInput
    *   \- операции в обратном порядке, которые были выполнены до delete
    * @param result
    *   \- операции в прямом порядке, которые надо будет выполнить после delete.
    * @return
    *   операции в прямом(!) порядке, включая delete в качестве первой операции.
    */
  def insertDeleteIntoReverseSequence(
    name: String,
    reverseInput: List[SchemaMapping2],
    result: List[SchemaMapping2]
  ): List[SchemaMapping2] =
    reverseInput match
      case head :: tail =>
        head match
          case SchemaMapping2.Sequence(mappings*)                                    =>
            insertDeleteIntoReverseSequence(name, mappings.toList reverse_::: reverseInput, result)
          case SchemaMapping2.Alternative(mappings*)                                 =>
            insertDeleteIntoReverseSequence(
              name,
              tail,
              SchemaMapping2.Alternative(mappings.map(deleteFromAlternative(name, _))*) :: result
            )
          case SchemaMapping2.SingleOperation(Operation.Delete(`name`))              =>
            // игнорируем дублирующее удаление колонки
            insertDeleteIntoReverseSequence(name, tail, result)
          case s @ SchemaMapping2.SingleOperation(Operation.Delete(other))           =>
            insertDeleteIntoReverseSequence(name, tail, s :: result)
          case s @ SchemaMapping2.SingleOperation(Operation.Rename(oldName, `name`)) =>
            // если удаляем после переименования, то можно удалить исходное имя.
            insertDeleteIntoReverseSequence(oldName, tail, result)
          case s @ SchemaMapping2.SingleOperation(Operation.Rename(`name`, newName)) =>
            throw new IllegalArgumentException("encountered rename a -> b and then delete a")
          case s @ SchemaMapping2.SingleOperation(Operation.Rename(_, _))            =>
            // если удаляем после переименования, то можно удалить исходное имя.
            insertDeleteIntoReverseSequence(name, tail, s :: result)
      case Nil          =>
        SchemaMapping2.SingleOperation(Operation.Delete(name)) :: result

  /** Удаляем колонку из преобразования.
    *
    * @param name
    * @param mapping
    * @return
    */
  def deleteFromAlternative(name: String, mapping: SchemaMapping2): SchemaMapping2 =
    mapping match
      case SchemaMapping2.Sequence(mappings*)    =>
        SchemaMapping2.Sequence(
          insertDeleteIntoReverseSequence(name, mappings.reverse.toList, Nil).tail*
        )
      case SchemaMapping2.Alternative(mappings*) =>
        SchemaMapping2.Alternative(mappings.map(deleteFromAlternative(name, _))*)
      case SchemaMapping2.SingleOperation(op)    =>
        SchemaMapping2.SingleOperation(
          op match
            case Operation.Rename(_, `name`)  =>
              Operation.Delete(name)
            case Operation.Rename(_, _)       =>
              op
            case delete @ Operation.Delete(_) =>
              delete
        )

end contact_processing2

/** EMAIL CLIENT - EXERCISE SET 3
  *
  * Consider a web email interface, which allows users to filter emails and direct them to specific
  * folders based on custom criteria.
  */
object email_filter2:
  final case class Address(emailAddress: String)
  final case class Email(sender: Address, to: List[Address], subject: String, body: String)

  final case class FieldGetter[T, V](fieldName: String, fieldAccessor: T => V)

  type ElementOf[V] = V match
    case List[el] => el
    case Seq[el]  => el
    case Set[el]  => el

  enum Predicate[T]:
    // case Equals(value: T)
    case Contains(value: T)
    case OneOf(values: Set[T])
    case Intersects(values: Set[ElementOf[T]])

  enum EmailFilter:
    case And(filters: EmailFilter*)
    case Or(filters: EmailFilter*)
    case Not(filter: EmailFilter)
    case FieldPredicate[V](field: FieldGetter[Email, V], predicate: Predicate[V])

    def self = this

    /** EXERCISE 1
      *
      * Add an "and" operator that models matching an email if both the first and the second email
      * filter match the email.
      */
    def &&(that: EmailFilter): EmailFilter =
      (this, that) match
        case (And(fs1*), And(fs2*)) =>
          And(fs1 ++ fs2*)
        case (And(fs1*), f2)        =>
          And(fs1 :+ f2*)
        case (f1, And(fs2*))        =>
          And(f1 +: fs2*)
        case (f1, f2)               =>
          And(f1, f2)

    /** EXERCISE 2
      *
      * Add an "or" operator that models matching an email if either the first or the second email
      * filter match the email.
      */
    def ||(that: EmailFilter): EmailFilter =
      (this, that) match
        case (Or(fs1*), Or(fs2*)) =>
          Or(fs1 ++ fs2*)
        case (Or(fs1*), f2)       =>
          Or(fs1 :+ f2*)
        case (f1, Or(fs2*))       =>
          Or(f1 +: fs2*)
        case (f1, f2)             =>
          Or(f1, f2)

    /** EXERCISE 3
      *
      * Add a "negate" operator that models matching an email if this email filter does NOT match an
      * email.
      */
    def negate: EmailFilter =
      this match
        case Not(f) => f
        case f      => Not(f)

  end EmailFilter
  object EmailFilter:

    /** EXERCISE 4
      *
      * Add a constructor for `EmailFilter` that models looking to see if the subject of an email
      * contains the specified word.
      */
    def subjectContains(string: String): EmailFilter =
      FieldPredicate(FieldGetter("subject", _.subject), Predicate.Contains(string))

    /** EXERCISE 5
      *
      * Add a constructor for `EmailFilter` that models looking to see if the body of an email
      * contains the specified word.
      */
    def bodyContains(string: String): EmailFilter =
      FieldPredicate(FieldGetter("body", _.body), Predicate.Contains(string))

    /** EXERCISE 6
      *
      * Add a constructor for `EmailFilter` that models looking to see if the sender of an email is
      * in the specified set of senders.
      */
    def senderIn(senders: Set[Address]): EmailFilter =
      FieldPredicate(FieldGetter("sender", _.sender), Predicate.OneOf(senders))

    /** EXERCISE 7
      *
      * Add a constructor for `EmailFilter` that models looking to see if the recipient of an email
      * is in the specified set of recipients.
      */
    def recipientIn(recipients: Set[Address]): EmailFilter =
      FieldPredicate(FieldGetter("to", _.to), Predicate.Intersects(recipients))

  end EmailFilter

  /** EXERCISE 8
    *
    * Implement an interpreter for the `EmailFilter` model that translates it into into tests on the
    * specified email.
    */
  def matches(filter: EmailFilter, email: Email): Boolean =
    filter match
      case EmailFilter.And(filters*)                                  =>
        filters.foldLeft(true)((accum, f) => accum && matches(f, email))
      case EmailFilter.Or(filters*)                                   =>
        filters.foldLeft(false)((accum, f) => accum || matches(f, email))
      case EmailFilter.Not(filter)                                    =>
        !matches(filter, email)
      case EmailFilter.FieldPredicate(FieldGetter(_, get), predicate) =>
        evalPredicate(predicate)(get(email))

  def evalPredicate[T](predicate: Predicate[T]): T => Boolean =
    predicate match
      case Predicate.Contains(value: String)               =>
        (t: T) => t.asInstanceOf[String].contains(value)
      case Predicate.Contains(value)               =>
        throw IllegalArgumentException("Cannot evaluate Predicate.Contains on other types apart from String")
      case Predicate.OneOf(values: Set[T])                 =>
        values.contains
      case Predicate.Intersects(values: Set[ElementOf[T]]) =>
        type V = ElementOf[T]
        (t: T) => t.asInstanceOf[scala.collection.Iterable[V]].toSet.intersect(values).nonEmpty

  /** EXERCISE 9
    *
    * Implement a function to make an English-readable description of an `EmailFilter`.
    */
  def describe(filter: EmailFilter): String =
    filter match
      case EmailFilter.And(filters*)                                    =>
        filters.map(describe).reduce(_ + " and " + _)
      case EmailFilter.Or(filters*)                                     =>
        filters.map(describe).reduce(_ + " or " + _)
      case EmailFilter.Not(filter)                                      =>
        "not " + describe(filter)
      case EmailFilter.FieldPredicate(FieldGetter(field, _), predicate) =>
        predicate match
          case Predicate.Contains(value)    =>
            s"field $field contains '$value'"
          case Predicate.OneOf(values)      =>
            s"field $field contains one of the values ${values.map("'" + _ + "'").mkString(", ")}"
          case Predicate.Intersects(values) =>
            s"field $field has non empty intersection with one of the values ${values.map("'" + _ + "'").mkString(", ")}"

end email_filter2

/** SPREADSHEET - EXERCISE SET 4
  *
  * Consider a spreadsheet application with a bunch of cells, containing either static data or
  * formula computed from other cells.
  */
object spreadsheet2:
  trait Spreadsheet:
    def cols: Int
    def rows: Int

    def valueAt(col: Int, row: Int): CalculatedValue

    final def scan(range: Range): LazyList[Cell] =
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

  final case class Cell(col: Int, row: Int, contents: CalculatedValue)

  enum Value:
    case Error(message: String)
    case Str(value: String)
    case Dbl(value: Double)

  extension (v: Value)
    def +(other: Value): Value =
      (v, other) match
        case (e1 @ Value.Error(_), _)       => e1
        case (_, e2 @ Value.Error(_))       => e2
        case (Value.Str(v1), Value.Str(v2)) => Value.Str(v1 + v2)
        case (Value.Dbl(v1), Value.Dbl(v2)) => Value.Dbl(v1 + v2)
        case (_, _)                         => Value.Error("Incompatible types of Values")

  enum CalculatedValue:
    case Negate(calculatedValue: CalculatedValue)
    case Sum(calculatedValues: CalculatedValue*)
    case Const(value: Value)
    case At(col: Int, row: Int)
    def self = this

    /** EXERCISE 1
      *
      * Add some operators to transform one `CalculatedValue` into another `CalculatedValue`. For
      * example, one operator could "negate" a double CalculatedValue.
      */
    def negate: CalculatedValue = Negate(self)

    /** EXERCISE 2
      *
      * Add some operators to combine `CalculatedValue`. For example, one operator could sum two
      * double CalculatedValueessions.
      */
    def sum(that: CalculatedValue): CalculatedValue =
      (this, that) match
        case (Sum(vs1*), Sum(vs2*)) => Sum(vs1 ++ vs2*)
        case (Sum(vs1*), v2)        => Sum(vs1 :+ v2*)
        case (v1, Sum(vs2*))        => Sum(v1 +: vs2*)
        case (v1, v2)               => Sum(v1, v2)
  end CalculatedValue

  object CalculatedValue:

    /** EXERCISE 3
      *
      * Add a constructor that makes an CalculatedValue from a Value.
      */
    def const(contents: Value): CalculatedValue =
      CalculatedValue.Const(contents)

    /** EXERCISE 4
      *
      * Add a constructor that provides access to the value of the specified cell, identified by
      * col/row.
      */
    def at(col: Int, row: Int): CalculatedValue =
      CalculatedValue.At(col, row)

  /** EXERCISE 5
    *
    * Implement an interpreter for the `Value.CalculatedValue` model that translates it into static
    * cell contents by evaluating the CalculatedValueession.
    */
  def evaluate(spreadsheet: Spreadsheet, cell: Cell): Value =
    evaluateCalculatedValue(spreadsheet)(cell.contents)

  def evaluateCalculatedValue(spreadsheet: Spreadsheet)(calculatedValue: CalculatedValue): Value =
    calculatedValue match
      case CalculatedValue.Negate(calculatedValue) =>
        val value = evaluateCalculatedValue(spreadsheet)(calculatedValue)
        value match
          case Value.Error(_)   => value
          case Value.Str(value) => Value.Error("cannot negate string value")
          case Value.Dbl(value) => Value.Dbl(-value)
      case CalculatedValue.Sum(calculatedValues*)  =>
        calculatedValues
          .map(evaluateCalculatedValue(spreadsheet))
          .foldLeft(Value.Dbl(0.0))(_ + _)
      case CalculatedValue.Const(value)            => value
      case CalculatedValue.At(col, row)            =>
        evaluateCalculatedValue(spreadsheet)(spreadsheet.valueAt(col, row))

end spreadsheet2

/** E-COMMERCE MARKETING - GRADUATION PROJECT
  *
  * Consider an e-commerce marketing platform where emails are sent to users whose history matches
  * specific patterns (for example, an event of adding a product to a shopping card, followed by an
  * abandonment of the web session).
  */
object ecommerce_marketing:
  type Event = Map[Attribute, Value]

  enum Attribute:
    case EventType
    case UserName
    case ShoppingCartId
    case Email
    case WebSession
    case DateTime

  enum Value:
    case Str(value: String)
    case Id(value: String)
    case Email(value: String)
    case DateTime(value: java.time.OffsetDateTime)

  object abstract_encoding:
    enum HistoryPattern:
      case Matches
      case EventP(eventPattern: EventPattern)
      case Sequence(first: HistoryPattern, second: HistoryPattern)
      case Repeat(pattern: HistoryPattern, min: Option[Int], max: Option[Int])

      def self = this

      def *>(that: HistoryPattern): HistoryPattern = HistoryPattern.Sequence(self, that)

      def atLeast(n: Int): HistoryPattern = repeat(Some(n), None)

      def atMost(n: Int): HistoryPattern = repeat(None, Some(n))

      def between(min: Int, max: Int): HistoryPattern = repeat(Some(min), Some(max))

      def repeat(min: Option[Int], max: Option[Int]): HistoryPattern =
        HistoryPattern.Repeat(self, min, max)
    object HistoryPattern:

      val matches: HistoryPattern = Matches

      def event(eventPattern: EventPattern): HistoryPattern = EventP(eventPattern)

      def eventType(eventType: String): HistoryPattern =
        event(EventPattern.HasValue(Attribute.EventType, Value.Str(eventType)))
    enum EventPattern:
      case Matches
      case HasValue(attr: Attribute, value: Value)

      def self = this

      import EventPattern.*

      def matches(event: Event): Boolean =
        self match
          case Matches               => true
          case HasValue(attr, value) => event.get(attr) == Some(value)

    import HistoryPattern.*
    import Attribute.EventType

    val example = eventType("add-item") *> eventType("abandon-cart")

    def matches(history: List[Event], pattern: HistoryPattern): Boolean =
      def loop(history: List[Event], pattern: HistoryPattern): (List[Event], Boolean) =
        (pattern, history.headOption) match
          case (EventP(eventPattern), Some(event)) => (history.tail, eventPattern.matches(event))
          case (EventP(_), None)                   => (history.tail, false)
          case (Sequence(first, second), _)        =>
            val (leftHistory, leftMatch) = loop(history, first)

            if leftMatch then loop(leftHistory, second) else (leftHistory, leftMatch)
          case (Repeat(pattern, min0, max0), _)    =>
            val min = min0.getOrElse(0)
            val max = max0.getOrElse(Int.MaxValue)

            val baseline = (0 to min).foldLeft((history, true)):
              case ((history, false), _) => (history, false)
              case ((history, true), _)  => loop(history, pattern)

            if !baseline._2 then baseline
            else
              val after = (0 to (max - min)).foldLeft(baseline):
                case ((history, false), _) => (history, false)
                case ((history, true), _)  => loop(history, pattern)

              (after._1, true)
          case _                                   => (history, false)
      loop(history, pattern)._2
    end matches
  end abstract_encoding

  /** EXERCISE 1
    *
    * Develop an executable encoding of the pattern matcher. Instead of having an ADT to represent a
    * pattern, and then interpreting that on a user history to see if there is a match, you will
    * represent a pattern as a function or an interface that is capable of testing the user history
    * for a match.
    */
  object executable_encoding:
    type HistoryPatternResult = (Boolean, List[Event])

    /**
      * Шаблон истории проверяет какое-то условие на части истории и возвращает продолжение
      */
    type HistoryPattern = (history: List[Event]) => HistoryPatternResult
    type EventPattern = (Event) => Boolean
    object EventPattern:
      def matches: EventPattern = (e) => true
      def hasValue(attr: Attribute, value: Value): EventPattern = 
        (e: Event) => e.get(attr) == Some(value)

    object HistoryPattern:
      // Всегда сопоставляется и не захватывает event'ы
      val matches: HistoryPattern = (true, _)

      def bind(historyPattern: HistoryPattern): HistoryPatternResult => HistoryPatternResult = historyPatternResult =>
        val (matched, history) = historyPatternResult
        if matched then
          historyPattern(history)
        else
          (false, history)

      def eventP(eventPattern: EventPattern): HistoryPattern = (history: List[Event]) =>
        history match
          case Nil => (false, history)
          case head :: tail => (eventPattern(head), tail)
        
      def sequence(first: HistoryPattern, second: HistoryPattern): HistoryPattern = 
        (history: List[Event]) =>
          bind(second)(first(history))

      def repeat(pattern: HistoryPattern, min0: Option[Int], max0: Option[Int]): HistoryPattern = 
        history =>
          val min = min0.getOrElse(0)
          val max = max0.getOrElse(Int.MaxValue)

          val baseline = (0 to min).foldLeft((true, history))((r, _) => bind(pattern)(r))

          if !baseline._1 then // если не сопоставилось min, то возвращаем изначальную историю.
            (false, history)
          else
            val after = (0 to (max - min)).foldLeft(baseline)((r, _) => bind(pattern)(r))

            (true, after._2) // в любом случае возвращаем ту историю которая осталась после удачных сопоставлений.

      def event(eventPattern: EventPattern): HistoryPattern = eventP(eventPattern)

      def eventType(eventType: String): HistoryPattern =
        eventP(EventPattern.hasValue(Attribute.EventType, Value.Str(eventType)))

    extension (self: HistoryPattern)  
      def *>(that: HistoryPattern): HistoryPattern = HistoryPattern.sequence(self, that)

      def atLeast(n: Int): HistoryPattern = repeat(Some(n), None)

      def atMost(n: Int): HistoryPattern = repeat(None, Some(n))

      def between(min: Int, max: Int): HistoryPattern = repeat(Some(min), Some(max))

      def repeat(min: Option[Int], max: Option[Int]): HistoryPattern =
        HistoryPattern.repeat(self, min, max)

end ecommerce_marketing
