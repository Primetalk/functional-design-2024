package net.degoes

import scala.util.matching.Regex
import scala.concurrent.duration.Duration
import javax.sound.midi.Sequence

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
    JsonNavigation.start.field("name").string("""\w+(\s+(\w|\s)+)+""".r)

  enum JsonValidation:
    case ValidateNumber(
      parent: JsonNavigation,
      min: Option[BigDecimal],
      max: Option[BigDecimal]
    ) extends JsonValidation
    case ValidateString(parent: JsonNavigation, pattern: Regex)
    case And(validations: JsonValidation*)
    case Or(validations: JsonValidation*)

    /** EXERCISE
      *
      * Design a binary operator with the meaning of parallel composition. The meaning of `a && b`
      * should be that `a` is validated, and also `b` is validated (starting from the root of the
      * JSON object).
      */
    def &&(that: JsonValidation): JsonValidation = JsonValidation.And(this, that)

    /** EXERCISE
      *
      * Design a binary operator with the meaning of fallback. The meaning of `a || b` should be
      * that `a` is validated, but if the validation fails, then `b` is validated (starting from the
      * root of the JSON object).
      */
    def ||(that: JsonValidation): JsonValidation = JsonValidation.Or(this, that)
  end JsonValidation
  enum JsonNavigation:
    case Start
    case DescendField(parent: JsonNavigation, name: String)
    case DescendElement(parent: JsonNavigation, index: Int)
    case DescendElements(parent: JsonNavigation)
    case Sequential(parent: JsonNavigation, downwards: JsonNavigation)

    def self = this

    def element(index: Int): JsonNavigation = JsonNavigation.DescendElement(self, index)

    def elements: JsonNavigation = JsonNavigation.DescendElements(self)

    def field(name: String): JsonNavigation = JsonNavigation.DescendField(self, name)

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
    def ++(that: JsonNavigation): JsonNavigation =
      JsonNavigation.Sequential(this, that)

  end JsonNavigation
  object JsonNavigation:
    def start: JsonNavigation = Start

  enum Json:
    case Object(fields: Map[java.lang.String, Json])
    case Array(elements: List[Json])
    case String(value: java.lang.String)
    case Number(value: BigDecimal)
    case Boolean(value: Boolean)
    case Null
  type ValidationResult = Either[String, Unit]
  val Validated: ValidationResult = Right((): Unit)
  final case class JsonValidator(validation: JsonValidation):

    def navigate(jsons: List[Json], jsonNavigation: JsonNavigation): List[Json] =
      jsonNavigation match
        case JsonNavigation.Start                         => jsons
        case JsonNavigation.DescendField(parent, name)    =>
          val jsons2 = navigate(jsons, parent)
          jsons2.flatMap:
            case Json.Object(fields) =>
              fields.get(name).toList
            case _                   => Nil
        case JsonNavigation.DescendElement(parent, index) =>
          val jsons2 = navigate(jsons, parent)
          jsons2.flatMap:
            case Json.Array(elements) =>
              elements.drop(index - 1).take(1)
            case _                    => Nil
        case JsonNavigation.DescendElements(parent)       =>
          val jsons2 = navigate(jsons, parent)
          jsons2.flatMap:
            case Json.Array(elements) =>
              elements
            case _                    => Nil
        case JsonNavigation.Sequential(parent, downwards) =>
          val jsons2 = navigate(jsons, parent)
          navigate(jsons2, downwards)

    /** EXERCISE
      *
      * Implement the following executor which validates JSON.
      */
    def validateWith(json: Json): Either[String, Unit] =
      validation match
        case JsonValidation.ValidateNumber(parent, min, max) =>
          val jsons2 = navigate(List(json), parent)
          jsons2
            .find:
              case Json.Number(value) if min.forall(value >= _) && max.forall(value <= _) => false
              case _                                                                      => true
            .map(j => Left(s"value $j is not a number within [$min, $max]"))
            .getOrElse(Validated)
        case JsonValidation.ValidateString(parent, pattern)  =>
          val jsons2 = navigate(List(json), parent)
          jsons2
            .find:
              case Json.String(value) if pattern.matches(value) => false
              case _                                            => true
            .map(j => Left(s"value $j is not a string that matches r'$pattern'"))
            .getOrElse(Validated)
        case JsonValidation.And(validations*)                =>
          validations
            .map(JsonValidator(_).validateWith(json))
            .find(_.isLeft)
            .getOrElse(Validated)
        case JsonValidation.Or(validations*)                 =>
          validations
            .map(JsonValidator(_).validateWith(json))
            .find(_.isRight)
            .getOrElse(Left(s"None of the validations matched"))
  end JsonValidator

end untyped

/** TYPED FUNCTIONAL DOMAINS - EXERCISE SET 2
  */
object typed:
  enum Baked[+A]:
    case Burnt(value: A)
    case CookedPerfect(value: A)
    case Undercooked(value: A)

  sealed trait Matter
  enum Ingredient extends Matter:
    case Eggs
    case Yolk
    case EggWhite
    case Sugar
    case Flour
    case Cinnamon
    case Curd
    case VanillaSugar
    case Salt
    case RefinedVegetableOil

  enum MatterAttribute:
    case Dry
    case Wet
    case Soft

  case class SpecificMatter(matter: Matter, attributes: MatterAttribute*) extends Matter

  /** разные названия промежуточных продуктов - тесто, масса, смесь... */
  case class IntermediateProduct(name: String) extends Matter

  case class BakedProduct(name: String) extends Matter

  /** Отходы (скорлупа яиц, пар, ...)
    */
  case object Waste extends Matter

  enum Ingredient2:
    case Eggs(number: Int)
    case Sugar(amount: Double)
    case Flour(amount: Double)
    case Cinnamon(amount: Double)

  enum Manipulation:
    case Bake
    case Fry
    case Mix
    case PressSlightly
    case Split(n: Int)
    case Whisk
    case Shake
    case SeparateWhitesYolk

  case class MatterAmount(matter: Matter, amountGramm: Double)

  enum TemperatureSetting:
    case RoomTemp
    case Oven(value: Int)

  enum ManipulationDuration:
    case UntilProperlyMixed
    case UntilProperlyBaked
    case FixedDuration(duration: Duration)

  case class OvenSettings(temperatureC: Double, fan: Boolean, grille: Boolean, powerW: Double)

  def eggAmount(number: Int): Double = number * 65.0

  val pinch = 5

  enum Recipe:
    case SingleWholeManipulation(
      manipulation: Manipulation,
      duration: ManipulationDuration,
      temperature: TemperatureSetting,
      inputs: List[MatterAmount],
      outputs: List[MatterAmount]
    )
    case SplitOperation(
      input: MatterAmount,
      partCount: Int,
      partOutput: MatterAmount
    )
    case Parallel(recipes: List[Recipe])
    case Sequence(recipes: List[Recipe])

  /** Рецепты сгруппированы по тому продукту, который получается в результате применения рецепта.
    */
  type RecipeBook = Map[Matter, Recipe]
  enum Recipe2[+A]:
    case Mix(ingredients: List[MatterAmount])
    case Bake(recipe: Recipe[A], temp: Int, time: Int) extends Recipe[Baked[A]]

    case Disaster                                       extends Recipe[Nothing]
    case AddIngredient(ingredient: Ingredient)          extends Recipe[Unit]
    case Both[A, B](a: Recipe[A], b: Recipe[B])         extends Recipe[(A, B)]
    case OneOf[A, B](a: Recipe[A], b: Recipe[B])        extends Recipe[Either[A, B]]
    case Map[A, B](a: Recipe[A], f: A => B)             extends Recipe[B]
    case FlatMap[A, B](a: Recipe[A], f: A => Recipe[B]) extends Recipe[B]
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
    def both[B](that: Recipe[B]): Recipe[(A, B)] = Both(this, that)

    /** EXERCISE 2
      *
      * Implement a `either` operation that allows trying a backup recipe, in case this recipe ends
      * in disaster.
      *
      * NOTE: Be sure to update the `bake` method below so that you can make recipes that use your
      * new operation.
      */
    def either[B](that: Recipe[B]): Recipe[Either[A, B]] = OneOf(this, that)

    /** EXERCISE 3
      *
      * Implement a `map` operation that allows changing what a recipe produces.
      *
      * NOTE: Be sure to update the `bake` method below so that you can make recipes that use your
      * new operation.
      */
    def map[B](f: A => B): Recipe[B] = Map(this, f)

    /** EXERCISE 4
      *
      * Implement a `flatMap` operation that allows deciding which recipe to make after this recipe
      * has produced its item.
      *
      * NOTE: Be sure to update the `bake` method below so that you can make recipes that use your
      * new operation.
      */
    def flatMap[B](f: A => Recipe[B]): Recipe[B] = FlatMap(this, f)
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
        case Recipe.Both(a, b)               =>
          val (loa, aa) = loop(ingredients, a)
          val (lob, bb) = loop(ingredients, b)
          (loa ++ lob, (aa, bb))
        case Recipe.OneOf(a, b)              =>
          val (loa, aa) = loop(ingredients, a)
          if aa == Baked.Burnt || aa == Baked.Undercooked then
            val (lob, bb) = loop(ingredients, b)
            (lob, Right(bb))
          else (loa, Left(aa))
        case Recipe.Map(a, f)                =>
          val (loa, aa) = loop(ingredients, a)
          (loa, f(aa))
        case Recipe.FlatMap(a, f)            =>
          val (loa, aa) = loop(ingredients, a)
          loop(loa, f(aa))

    val (leftover, a) = loop(Vector(), recipe)

    println(s"Leftover ingredients: ${leftover}")

    a
  end bake
  final case class Cake(ingredients: List[Ingredient])

  /** EXERCISE 5
    *
    * Make a recipe that will produced a baked cake or other food of your choice! Сырники \=======
    *
    * Чтобы сырники получились мягкими и нежными, но при этом хорошо держали форму, большое значение
    * имеет качество творога.
    *
    * Продукты
    *
    * Творог жирностью 9% - 400 г Яйца - 1-2 шт. (в зависимости от размера) Сахар - 60 г (3 ст.
    * ложки), по вкусу Мука - 70 г (2 ст. ложки с большой горкой) + для панировки Ванильный сахар -
    * 10 г Соль - 1 щепотка Масло растительное рафинированное - для жарки
    *
    * Сырники из творога
    *
    * Фото приготовления рецепта: Сырники из творога - шаг №1
    *
    * Соединяем в миске творог (натуральный, не мокрый), яйцо, сахар, ванильный сахар и соль.
    *
    * Фото приготовления рецепта: Сырники из творога - шаг №2
    *
    * Всё хорошо перемешиваем. Можно использовать блендер, чтобы творожная масса получилась
    * однородной.
    *
    * Фото приготовления рецепта: Сырники из творога - шаг №3
    *
    * Затем добавляем муку и тщательно перемешиваем творожное тесто.
    *
    * Фото приготовления рецепта: Сырники из творога - шаг №4
    *
    * Формируем небольшие шарики из творожного теста, слегка их придавливаем.
    *
    * Фото приготовления рецепта: Сырники из творога - шаг №5
    *
    * На разогретую сковороду наливаем растительное масло.
    *
    * Обваливаем сырники в муке и выкладываем на сковороду.
    *
    * Жарим сырники на небольшом огне до золотистой корочки с двух сторон. Можно накрыть сковороду
    * крышкой, перевернув сырники на другую сторону.
    *
    * Фото приготовления рецепта: Сырники из творога - шаг №6
    *
    * Приятного аппетита!
    */
  lazy val recipe: Recipe[Baked[Cake]] =
    val curdMass = IntermediateProduct("творожная масса")
    val dough    = IntermediateProduct("тесто")
    val curdBall = IntermediateProduct("творожный шарик")
    val curdFlat = IntermediateProduct("творожная лепёшка")
    Recipe
      .Sequence(
        List(
          Recipe.SingleWholeManipulation(
            Manipulation.Mix,
            ???,
            ???,
            List(
              MatterAmount(SpecificMatter(Ingredient.Curd, MatterAttribute.Dry), 400),
              MatterAmount(Ingredient.Eggs, eggAmount(1)),
              MatterAmount(Ingredient.Sugar, 60),
              MatterAmount(Ingredient.VanillaSugar, 10),
              MatterAmount(Ingredient.Salt, pinch)
            ),
            List(
              MatterAmount(curdMass, 540)
            )
          ),
          Recipe.SingleWholeManipulation(
            Manipulation.Mix,
            ???,
            ???,
            List(
              MatterAmount(curdMass, 540),
              MatterAmount(Ingredient.Flour, 70)
            ),
            List(
              MatterAmount(dough, 610)
            )
          ),
          Recipe.SplitOperation(
            MatterAmount(dough, 610),
            partCount = 12,
            partOutput = curdBall
          ),
          Recipe.SingleWholeManipulation(
            Manipulation.PressSlightly,
            ???,
            ???,
            inputs = List(curdBall),
            outputs = List(curdFlat)
          ),
          Recipe.SingleWholeManipulation(
            Manipulation.Bake,
            ManipulationDuration.UntilProperlyBaked,
            TemperatureSetting.Oven(200),
            inputs = List(curdBall),
            outputs = List(curdFlat)
          )
        )
      )
      .addIngredient(Ingredient.Flour)         // (500.0))
      .both(
        Recipe.addIngredient(Ingredient.Sugar) // (150.0))
      )
      .both(
        Recipe.addIngredient(Ingredient.Eggs)  // (2))
      )
      .map(_ => Cake(Nil))
      .bake(200, 30)
  end recipe

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
