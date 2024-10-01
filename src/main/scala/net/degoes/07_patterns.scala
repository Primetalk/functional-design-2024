package net.degoes

import scala.util.matching.Regex

/*
 * INTRODUCTION
 *
 * In Functional Design, operators that transform and compose values in a
 * domain often fall into pre-existing patterns.
 *
 * In this section, you'll learn to identify these patterns, first in
 * untyped functional domains, and then in typed functional domains.
 *
 */

/** UNTYPED FUNCTIONAL DOMAINS - EXERCISE SET 2
  */
object untyped:
  lazy val nameValidation =
    JsonValidation.start.field("name").string("""\w+(\s+(\w|\s)+)+""".r)

  // ValidateString(DescendField(Start, "name"), "...")

  enum JsonValidation:
    case Start
    case DescendField(parent: JsonValidation, name: String)
    case DescendElement(parent: JsonValidation, index: Int)
    case DescendElements(parent: JsonValidation)
    case ValidateNumber(
      parent: JsonValidation,
      min: Option[BigDecimal],
      max: Option[BigDecimal]
    ) extends JsonValidation
    case ValidateString(parent: JsonValidation, pattern: Regex)
    case AndThen(parent: JsonValidation, child: JsonValidation)
    case Both(left: JsonValidation, right: JsonValidation)
    case OrElse(validation: JsonValidation, fallback: JsonValidation)

    def self = this

    def element(index: Int): JsonValidation = JsonValidation.DescendElement(self, index)

    def elements: JsonValidation = JsonValidation.DescendElements(self)

    def field(name: String): JsonValidation = JsonValidation.DescendField(self, name)

    def number: JsonValidation = JsonValidation.ValidateNumber(self, None, None)

    def numberBetween(min: BigDecimal, max: BigDecimal): JsonValidation =
      JsonValidation.ValidateNumber(self, Some(min), Some(max))

    def string(regex: Regex): JsonValidation = JsonValidation.ValidateString(self, regex)

    /** EXERCISE
      *
      * Design a binary operator with the meaning of sequential composition. For example,
      * `JsonValidation.start.field("address") ++ JsonValidation.start.field("street")` would first
      * validate that a field called `address` exists, and then would descend into that field value
      * to validate that a field called `street` exists within it.
      */
    def ++(that: JsonValidation): JsonValidation = JsonValidation.AndThen(self, that)

    /** EXERCISE
      *
      * Design a binary operator with the meaning of parallel composition. The meaning of `a && b`
      * should be that `a` is validated, and also `b` is validated (starting from the root of the
      * JSON object).
      */
    def &&(that: JsonValidation): JsonValidation = JsonValidation.Both(self, that)

    /** EXERCISE
      *
      * Design a binary operator with the meaning of fallback. The meaning of `a || b` should be
      * that `a` is validated, but if the validation fails, then `b` is validated (starting from the
      * root of the JSON object).
      */
    def ||(that: JsonValidation): JsonValidation = JsonValidation.OrElse(self, that)
  end JsonValidation

  object JsonValidation:
    def start: JsonValidation = Start

  enum Json:
    case Object(fields: Map[String, Json])
    case Array(elements: List[Json])
    case JsString(value: String)
    case Number(value: BigDecimal)
    case Boolean(value: Boolean)
    case Null

  final case class JsonValidator(validation: JsonValidation):

    private def error(msg: String, json: Json): Either[String, Nothing] =
      Left(s"$msg: $json")

    private def validateJsons(jsons: List[Json])(f: Json => Either[String, List[Json]]): Either[String, List[Json]] =
      jsons.foldLeft[Either[String, List[Json]]](Right(List.empty[Json])):
        case (acc @ Left(_), _) => acc
        case (Right(xs), json) =>
          f(json) match
            case Left(s) => Left(s)
            case Right(ys) => Right(xs ++ ys)
          
    private def run(v: JsonValidation, jsons: List[Json]): Either[String, List[Json]] =
      v match
        case JsonValidation.Start =>
          Right(jsons)
        case JsonValidation.DescendField(parent, name) =>
          run(parent, jsons).flatMap(js=>
            validateJsons(js):
              case j @ Json.Object(fields) =>
                fields.get(name).fold(error(s"no field $name", j))(x => Right(List(x)))
              case json => error("not an object", json)
          )
        case JsonValidation.DescendElement(parent, index) =>
          run(parent, jsons).flatMap(js =>
            validateJsons(js):
              case Json.Array(elements) if elements.length > index =>
                Right(List(elements(index)))
              case xs @ Json.Array(_) => error(s"array is less than $index", xs)
              case x => error("not an array", x)
          )
        case JsonValidation.DescendElements(parent) =>
          run(parent, jsons).flatMap(js =>
            validateJsons(js):
              case Json.Array(elements) => Right(elements)
              case x => error("not an array", x)
          )
        case JsonValidation.ValidateNumber(parent, min, max) =>
          run(parent, jsons).flatMap(js =>
            validateJsons(js):
              case Json.Number(value) if min.forall(_ < value) && max.forall(_ > value) =>
                Right(List())
              case x @ Json.Number(value) => error(s"value is out of bounds [$min, $max]", x)
              case x => error("is not a number", x)
          )
        case JsonValidation.ValidateString(parent, pattern) =>
          run(parent, jsons).flatMap(js =>
            validateJsons(js):
              case Json.JsString(value) if pattern.matches(value) =>
                Right(List())
              case x @ Json.JsString(value) => error(s"value does not match $pattern", x)
              case x => error("is not a string", x)
          )
        case JsonValidation.AndThen(parent, child) =>
          run(parent, jsons).flatMap(run(child, _))
        case JsonValidation.Both(left, right) =>
          run(left, jsons).flatMap(js => run(right, jsons).map(js ++ _))
        case JsonValidation.OrElse(validation, fallback) =>
          run(validation, jsons).orElse(run(fallback, jsons))

    /** EXERCISE
      *
      * Implement the following executor which validates JSON.
      */
    def validateWith(json: Json): Either[String, Unit] = run(validation, List(json)).map(_ => ())
      
end untyped

/** TYPED FUNCTIONAL DOMAINS - EXERCISE SET 2
  */
object typed:
  enum Baked[+A]:
    case Burnt(value: A)
    case CookedPerfect(value: A)
    case Undercooked(value: A)

  enum Ingredient:
    case Eggs(number: Int)
    case Sugar(amount: Double)
    case Flour(amount: Double)
    case Cinnamon(amount: Double)

  enum Recipe[+A]:
    case Disaster                                      extends Recipe[Nothing]
    case AddIngredient(ingredient: Ingredient)         extends Recipe[Unit]
    case Bake(recipe: Recipe[A], temp: Int, time: Int) extends Recipe[Baked[A]]

    def self = this

    /** Uses all the ingredients in a recipe by baking them to produce a baked result.
      */
    def bake(temp: Int, time: Int): Recipe[Baked[A]] = Recipe.Bake(self, temp, time)

    /** EXERCISE 1
      *
      * Implement a `both` operation that allows combining two recipes into one, producing both
      * items in a tuple.
      *
      * NOTE: Be sure to update the `bake` method below so that you can make recipes that use your
      * new operation.
      */
    def both[B](that: Recipe[B]): Recipe[(A, B)] = ???

    /** EXERCISE 2
      *
      * Implement a `either` operation that allows trying a backup recipe, in case this recipe ends
      * in disaster.
      *
      * NOTE: Be sure to update the `bake` method below so that you can make recipes that use your
      * new operation.
      */
    def either[B](that: Recipe[B]): Recipe[Either[A, B]] = ???

    /** EXERCISE 3
      *
      * Implement a `map` operation that allows changing what a recipe produces.
      *
      * NOTE: Be sure to update the `bake` method below so that you can make recipes that use your
      * new operation.
      */
    def map[B](f: A => B): Recipe[B] = ???

    /** EXERCISE 4
      *
      * Implement a `flatMap` operation that allows deciding which recipe to make after this recipe
      * has produced its item.
      *
      * NOTE: Be sure to update the `bake` method below so that you can make recipes that use your
      * new operation.
      */
    def flatMap[B](f: A => Recipe[B]): Recipe[B] = ???
  end Recipe
  object Recipe:
    def addIngredient(ingredient: Ingredient): Recipe[Unit] = AddIngredient(ingredient)

    def disaster: Recipe[Nothing] = Disaster
  import Recipe.*

  def bake[Out](recipe: Recipe[Out]): Out =
    def loop[A](ingredients: Vector[Ingredient], recipe: Recipe[A]): (Vector[Ingredient], A) =
      recipe match
        case Disaster                        => throw new Exception("Uh no, utter disaster!")
        case AddIngredient(ingredient)       => (ingredients :+ ingredient, ())
        case bake @ Bake(recipe, temp, time) =>
          val (leftover, a) = loop(ingredients, recipe)

          println(s"Baking ${a} for ${time} minutes at ${temp} temperature")
          println("Ingredients: ")
          println(ingredients.mkString("\n"))

          if time * temp < 1000 then (Vector(), Baked.Undercooked(a))
          else if time * temp > 6000 then (Vector(), Baked.Burnt(a))
          else (Vector(), Baked.CookedPerfect(a))

    val (leftover, a) = loop(Vector(), recipe)

    println(s"Leftover ingredients: ${leftover}")

    a
  end bake
  final case class Cake(ingredients: List[Ingredient])

  /** EXERCISE 5
    *
    * Make a recipe that will produced a baked cake or other food of your choice!
    */
  lazy val recipe: Recipe[Baked[Cake]] = ???
end typed

object stack:
  enum StackVM[T <: Tuple]:
    case Empty                                           extends StackVM[EmptyTuple]
    case Push[A, T <: Tuple](value: A, prev: StackVM[T]) extends StackVM[A *: T]
    case Add[T <: Tuple, T2 <: Tuple](prev: StackVM[T])(using T <:< Int *: Int *: T2)
        extends StackVM[Int *: T2]

    def self = this

    def push[A](a: A): StackVM[A *: T] = StackVM.Push(a, self)

    def add[T2 <: Tuple](using T <:< Int *: Int *: T2): StackVM[Int *: T2] = Add(self)

    def run: T =
      def loop(op: StackVM[_], stack: Tuple): Tuple =
        op match
          case Empty             => stack
          case Push(value, prev) => loop(prev, value *: stack)
          case Add(prev)         =>
            loop(prev, stack) match
              case x1 *: x2 *: xs =>
                (x1.asInstanceOf[Int] + x2.asInstanceOf[Int]) *: xs

              case xs => throw new IllegalStateException(s"Uh oh: ${xs}")

      loop(self, EmptyTuple).asInstanceOf[T]
  end StackVM

  def empty: StackVM[EmptyTuple] = StackVM.Empty

  @main
  def example =
    println(empty.push(1).push(2).add.run)

end stack
