package net.degoes

import java.net.URL
import java.time.LocalDate
import java.time.LocalDateTime
import net.degoes.etl.Pipeline.extract

/*
 * INTRODUCTION
 *
 * In the last section, you explored operators. In this section, you will have
 * a chance to flesh out the design of full functional domains, which include
 * not only operators, but also models and constructors.
 */

/** SPREADSHEET - EXERCISE SET 1
  *
  * Consider a spreadsheet application with a bunch of cells, containing either static data or
  * formula computed from other cells.
  */
object spreadsheet:
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

  /** EXERCISE 1
    *
    * Design a data type called `CalculatedValue`, which represents a `Value` that is dynamically
    * computed from a `Spreadsheet`.
    */
  final case class CalculatedValue( eval: Spreadsheet => Value ):
    self =>

    /** EXERCISE 2
      *
      * Add an operator that returns a new `CalculatedValue` that is the negated version of this
      * one.
      */
    def unary_- : CalculatedValue = CalculatedValue((s: Spreadsheet) => eval(s) match
      case Value.Dbl(d) => Value.Dbl(-d)
      case Value.Str(value) => Value.Error("Cannot negate string")
      case e => e
    )

    /** EXERCISE 3
      *
      * Add a binary operator `+` that returns a new `CalculatedValue` that is the sum of the two
      * calculated values.
      */
    def +(that: CalculatedValue): CalculatedValue = CalculatedValue((s: Spreadsheet) => 
      (eval(s), that.eval(s)) match
        case (Value.Dbl(d1), Value.Dbl(d2)) => Value.Dbl(d1 + d2)
        case (Value.Str(value1), Value.Str(value2)) => Value.Str(value = value1 + value2)
        case (e@Value.Error(_), _) => e
        case (_, e@Value.Error(_)) => e
        case (v1, v2) => Value.Error(s"Incompatible values: $v1, $v2")
    )

    /** EXERCISE 4
      *
      * Add a binary operator `-` that returns a new `CalculatedValue` that is the difference of the
      * two calculated values.
      */
    def -(that: CalculatedValue): CalculatedValue = CalculatedValue((s: Spreadsheet) => 
      (eval(s), that.eval(s)) match
        case (Value.Dbl(d1), Value.Dbl(d2)) => Value.Dbl(d1 - d2)
        //case (Value.Str(value1), Value.Str(value2)) => Value.Str(value = value1 + "-" + value2)
        case (e@Value.Error(_), _) => e
        case (_, e@Value.Error(_)) => e
        case (v1, v2) => Value.Error(s"Incompatible values: $v1, $v2")
    )

    protected def binaryOp(that: CalculatedValue)(error: String)(
      f: PartialFunction[(Value, Value), Value]
    ): CalculatedValue = CalculatedValue((s: Spreadsheet) => 
      f.applyOrElse((eval(s), that.eval(s)), _ => Value.Error(error))
    )
  end CalculatedValue
  object CalculatedValue:

    /** EXERCISE 5
      *
      * Add a constructor that makes an `CalculatedValue` from a `Value`.
      */
    def const(contents: Value): CalculatedValue = CalculatedValue(_ => contents)

    /** EXERCISE 6
      *
      * Add a constructor that provides access to the value of the specified cell, identified by
      * col/row.
      */
    def at(col: Int, row: Int): CalculatedValue = CalculatedValue(s => s.valueAt(col, row).eval(s))

  /** EXERCISE 7
    *
    * Describe a cell whose contents are the sum of the cells at (0, 0) and (1, 0).
    */
  lazy val cell1: Cell = Cell(2, 0, CalculatedValue.at(0, 0) + CalculatedValue.at(1, 0))
end spreadsheet

/** ETL - EXERCISE SET 2
  *
  * Consider an application designed to extract, transform, and load data.
  */
object etl:
  import scala.util.*

  /** Represents a row of data.
    */
  final case class DataRow(row: Map[String, DataValue]):
    self =>
    def delete(name: String): DataRow = DataRow(row - name)

    def map(name: String)(f: PartialFunction[DataValue, DataValue]): DataRow =
      row.get(name).fold(self)(v => f.lift(v).fold(self)(v => DataRow(row.updated(name, v))))

    def rename(oldName: String, newName: String): DataRow =
      DataRow(row.get(oldName).fold(row)(value => (row - oldName).updated(newName, value)))

    def coerce(name: String, dtype: DataType): DataRow =
      row.get(name).fold(self)(v => v.coerce(dtype).fold(self)(v => DataRow(row + (name -> v))))

  /** Represents a stream of data.
    */
  final case class DataStream(foreach: (Try[DataRow] => Unit) => Unit):
    self =>
    def coerce(name: String, dtype: DataType): DataStream = self.map(_.coerce(name, dtype))

    def delete(name: String): DataStream = self.map(_.delete(name))

    def orElse(that: => DataStream): DataStream =
      DataStream { callback =>
        self.foreach:
          case Failure(exception) => that.foreach(callback)
          case x                  => callback(x)
      }

    def map(f: DataRow => DataRow): DataStream =
      DataStream(callback => self.foreach(a => callback(a.map(f))))

    def mapColumn(name: String)(f: PartialFunction[DataValue, DataValue]): DataStream =
      self.map(_.map(name)(f))

    def merge(that: => DataStream): DataStream =
      DataStream { callback =>
        self.foreach(callback)
        that.foreach(callback)
      }

    def rename(oldName: String, newName: String): DataStream =
      self.map(_.rename(oldName, newName))
  end DataStream

  /** EXERCISE 1
    *
    * Design a data type that models sources and sinks in an ETL pipeline. Assume your business
    * requires you to extract data from (and load data to) FTP sites, URLs, AWS S3 buckets, and
    * databases described by JDBC connection strings.
    *
    * Also mock out, but do not implement, a method on each repository type called `load`, which
    * returns a `DataStream`.
    */
  trait DataRepo:
    def load: DataStream = ???

  class Ftp(val host: String) extends DataRepo
  class Http(val url: URL) extends DataRepo
  class S3bucket(val bucket: String) extends DataRepo
  class DbTable(val connectionString: String, val table: String) extends DataRepo

  enum FileFormat:
    case Json
    case Csv
    case Xml

  /** EXERCISE 2
    *
    * Design a data type that models the type of primitives the ETL pipeline has access to. This
    * will include string, numeric, and date/time data.
    */
  enum DataType:
    case Null
    case Str
    case Num
    case Int
    case Datetime

  /** EXERCISE 3
    *
    * Design a data type that models a value. Every value should have a `DataType` that identifies
    * its type (string, numeric, or data/time), and a `coerce` method to coerce the value into
    * another type.
    *
    * Be sure to model null, string, and integer, at the very least!
    */
  enum DataValue:
    case Null
    case Str(value: String)
    case Num(value: BigDecimal)
    case Int(value: scala.Int)
    case Datetime(value: LocalDateTime)

    def self = this

    def dataType: DataType =
      self match
        case Null => DataType.Null
        case DataValue.Str(_) => DataType.Str
        case DataValue.Num(_) => DataType.Num
        case DataValue.Int(_) => DataType.Int
        case DataValue.Datetime(_) => DataType.Datetime

    def coerce(otherType: DataType): Option[DataValue] =
      self match
        case Null =>
          otherType match
            case DataType.Null => Some(self)
            case DataType.Str => None
            case DataType.Num => None
            case DataType.Int => None
            case DataType.Datetime => None
        case DataValue.Str(_) =>
          otherType match
            case DataType.Null => None
            case DataType.Str => Some(self)
            case DataType.Num => None
            case DataType.Int => None
            case DataType.Datetime => None
        case DataValue.Num(value) =>
          otherType match
            case DataType.Null => None
            case DataType.Str => None
            case DataType.Num => Some(self)
            case DataType.Int => Some(DataValue.Int(value.toInt))
            case DataType.Datetime => None
        case DataValue.Int(value) =>
          otherType match
            case DataType.Null => None
            case DataType.Str => None
            case DataType.Num => Some(DataValue.Num(BigDecimal.valueOf(value)))
            case DataType.Int => Some(self)
            case DataType.Datetime => None
        case DataValue.Datetime(_) =>
          otherType match
            case DataType.Null => None
            case DataType.Str => None
            case DataType.Num => None
            case DataType.Int => None
            case DataType.Datetime => Some(self)
  end DataValue

  /** EXERCISE 4
  *
    * `Pipeline` is a data type that models a transformation from an input data set into an output
    * data step, as a series of one or more individual operations.
    *
    * Create a model of a pipeline, using `DataStream`.
    */
  final case class Pipeline(f: DataStream => DataStream):
    self =>

    /** EXERCISE 5
      *
      * Add a `merge` operator that models the merge of the output of this pipeline with the output
      * of the specified pipeline.
      *
      * {{{
      * Merge Associativity:  (p1 merge p2) merge p3 == p1 merge (p2 merge p3)
      * Merge Identity:       p merge Pipeline.empty == Pipeline.empty merge p == p
      * Merge Commutativity:  p1 merge p2 == p2 merge p1
      * Merge Duplication:    ???
      * }}}
      */
    def merge(that: Pipeline): Pipeline = Pipeline{input => 
      f(input).merge(that.f(input))
    }

    /** EXERCISE 6
      *
      * Add an `orElse` operator that models applying this pipeline, but if it fails, switching over
      * and trying another pipeline.
      */
    def orElse(that: Pipeline): Pipeline = Pipeline(
      input =>
        Try(f(input)) match
          case Success(value) => value.orElse(that.f(input))
          case Failure(_) => that.f(input)        
    )

    /** EXERCISE 7
      *
      * Add an operator to rename a column in a pipeline.
      */
    def rename(oldName: String, newName: String): Pipeline = Pipeline(
      input => input.rename(oldName, newName)
    )

    /** EXERCISE 8
      *
      * Add an operator to coerce a column into a specific type in a pipeline.
      */
    def coerce(column: String, newType: DataType): Pipeline = Pipeline(
      input => DataStream(
        foreach = outputCallback =>
          input.foreach(dataRow => 
            outputCallback(dataRow.map(_.coerce(column, newType)))
          )
      )
    )

    /** EXERCISE 9
      *
      * Add an operator to delete a column in a pipeline.
      */
    def delete(column: String): Pipeline = Pipeline(
      input => input.delete(column)
    )

    /** EXERCISE 10
      *
      * To replace nulls in the specified column with a specified value.
      */
    def replaceNulls(column: String, defaultValue: DataValue): Pipeline = Pipeline(
      input => DataStream(f =>
        input.foreach(r => 
          r match
            case failure@Failure(exception) => f(failure)
            case Success(dataRow) => 
              val newValue = 
                dataRow.row.getOrElse(column, defaultValue) match
                  case net.degoes.etl.DataValue.Null => defaultValue
                  case v => v
              f(Success(DataRow(dataRow.row.updated(column, newValue))))
        )
      )
    )
  end Pipeline
  object Pipeline:

    /** EXERCISE 11
      *
      * Add a constructor for `Pipeline` that models extraction of data from the specified data
      * repository.
      */
    def extract(repo: DataRepo): Pipeline =
      Pipeline(input => repo.load)

  /** EXERCISE 12
    *
    * Create a pipeline that models extracts data from a URL, replacing all null "age" columns with
    * "0" as the default age, which renames a column "fname" into a column "first_name", and which
    * coerces the "age" column into an integer type.
    */
  lazy val pipeline: Pipeline = 
    extract(Http(URL.apply("http://example.com/data.csv")))
      .replaceNulls("age", DataValue.Str("0"))
      .rename("fname", "first_name")
      .coerce("age", DataType.Int)

end etl

/** REAL ESTATE APP - GRADUATION PROJECT
  *
  * Consider a real estate app that must regularly fetch third-party pricing data according to
  * specified schedules. These schedules can be quite complicated, although they possess regular
  * structure (e.g. every fifth Tuesday, and hourly on Wednesdays). The business considers it
  * acceptable to create the schedules in code (rather than reading them from a database).
  */
object pricing_fetcher:
  def fetch(directory: java.io.File, url: java.net.URL, schedule: Schedule): Unit = ???

  enum DayOfWeek:
    case Sunday
    case Monday
    case Tuesday
    case Wednesday
    case Thursday
    case Friday
    case Saturday

  final case class Time(
    minuteOfHour: Int,
    hourOfDay: Int,
    dayOfWeek: DayOfWeek,
    weekOfMonth: Int,
    monthOfYear: Int
  )

  /** EXERCISE 1
    *
    * `Schedule` is a data type that models a schedule, which has the ability to indicate whether at
    * any given `java.time.Instant`, it is time to fetch the pricing data set.
    */
  final case class Schedule( predicate: Time => Boolean ):
    self =>
    /*
     * EXERCISE 2
     *
     * Create an operator for schedule that allows composing two schedules to
     * yield the union of those schedules. That is, the fetch will occur
     * only when either of the schedules would have performed a fetch.
     */
    def union(that: Schedule): Schedule = 
      Schedule(i => predicate(i) || that.predicate(i))

    /** EXERCISE 3
      *
      * Create an operator for schedule that allows composing two schedules to yield the
      * intersection of those schedules. That is, the fetch will occur only when both of the
      * schedules would have performed a fetch.
      */
    def intersection(that: Schedule): Schedule = 
      Schedule(i => predicate(i) && that.predicate(i))

    /** EXERCISE 4
      *
      * Create a unary operator that returns a schedule that will never fetch when the original
      * schedule would fetch, and will always fetch when the original schedule would not fetch.
      */
    def negate: Schedule = 
      Schedule(predicate andThen (!_))

  end Schedule
  object Schedule:

    /** EXERCISE 5
      *
      * Create a constructor for Schedule that models fetching on specific weeks of the month.
      */
    def weeks(weeks: Int*): Schedule = Schedule(t => 
      weeks.contains(t.weekOfMonth)
    )

    /** EXERCISE 6
      *
      * Create a constructor for Schedule that models fetching on specific days of the week.
      */
    def daysOfTheWeek(daysOfTheWeek: DayOfWeek*): Schedule = Schedule(t => 
      daysOfTheWeek.contains(t.dayOfWeek)
    )

    /** EXERCISE 7
      *
      * Create a constructor for Schedule that models fetching on specific hours of the day.
      */
    def hoursOfTheDay(hours: Int*): Schedule = Schedule(t => 
      hours.contains(t.hourOfDay)
    )

    /** EXERCISE 8
      *
      * Create a constructor for Schedule that models fetching on specific minutes of the hour.
      */
    def minutesOfTheHour(minutes: Int*): Schedule = Schedule(t => 
      minutes.contains(t.minuteOfHour)
    )
  end Schedule

  import Schedule.*
  /** EXERCISE 9
    *
    * Create a schedule that repeats every Wednesday, at 6:00 AM and 12:00 PM, and at 5:30, 6:30,
    * and 7:30 every Thursday.
    */
  lazy val schedule: Schedule = 
    daysOfTheWeek(DayOfWeek.Wednesday)
      .intersection(hoursOfTheDay(0,6).intersection(minutesOfTheHour(0)))
      .union(
        daysOfTheWeek(DayOfWeek.Thursday)
          .intersection(
            hoursOfTheDay(5,6,7).intersection(minutesOfTheHour(30))
          )
      )
      
end pricing_fetcher
