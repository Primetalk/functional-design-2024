package net.degoes

/*
 * INTRODUCTION
 *
 * In Functional Design, composable operators allow building infinitely many
 * solutions from a few operators and domain constructors.
 *
 * Operators and constructors are either primitive, meaning they cannot be
 * expressed in terms of others, or they are derived, meaning they can be
 * expressed in terms of other operators or constructors.
 *
 * The choice of primitives determine how powerful and expressive a domain
 * model is. Some choices lead to weaker models, and others, to more powerful
 * models. Power is not always a good thing: constraining the power of a model
 * allows more efficient and more feature-full execution.
 *
 * Derived operators and constructors bridge the gap from the domain, to common
 * problems that a user of the domain has to solve, improving productivity.
 *
 * In many domains, there exist many potential choices for the set of primitive
 * operators and constructors. But not all choices are equally good.
 *
 * The best primitives are:
 *
 * * Composable, to permit a lot of power in a small, reasonable package
 * * Expressive, to solve the full range of problems in the domain
 * * Orthogonal, such that no primitive provides the capabilities of any other
 *
 * Orthogonality also implies minimalism, which means the primitives are the
 * smallest set of orthogonal primitives that exist.
 *
 */

/** ORTHOGONALITY - EXERCISE SET 1
  */
object email_filter3:
  final case class Address(emailAddress: String)
  final case class Email(sender: Address, to: List[Address], subject: String, body: String)

  enum FieldType:
    case String
    case Address
    case ListOfAddress
  final case class Field[V](name: String, getter: Email => List[V], fieldType: FieldType)

  val senderField = Field[Address]("sender", e => List(e.sender), FieldType.Address)
  val toField = Field[Address]("to", _.to, FieldType.ListOfAddress)
  val subjectField = Field[String]("subject", e => List(e.subject), FieldType.String)
  val bodyField = Field[String]("body", e => List(e.body), FieldType.String)

  /** EXERCISE 1
    *
    * In the following model, which describes an email filter, there are many primitives with
    * overlapping responsibilities. Find the smallest possible set of primitive operators and
    * constructors, without deleting any constructors or operators (you may implement them in terms
    * of primitives).
    *
    * NOTE: You may *not* use a final encoding, which would allow you to collapse everything down to
    * one primitive.``
    */
  enum EmailFilter:
    case Always
    // case Never = not always
    case Not(filter: EmailFilter)
    case And(left: EmailFilter, right: EmailFilter)
    case InclusiveOr(left: EmailFilter, right: EmailFilter)
    // while we could implement it via InclusiveOr + Not + And,
    // it is still valuable to have it as a primitive.
    case ExclusiveOr(left: EmailFilter, right: EmailFilter)
    // case SenderEquals(target: Address)
    // case RecipientEquals(target: Address)
    case FieldPredicate[V](field: Field[V], check: List[V] => Boolean)
    // case SenderIn(targets: Set[Address])
    // case RecipientIn(targets: Set[Address])
    // case BodyContains(phrase: String)
    // // case BodyNotContains(phrase: String)
    // case SubjectContains(phrase: String)
    // // case SubjectNotContains(phrase: String)

    def self = this

    def &&(that: EmailFilter): EmailFilter = EmailFilter.And(self, that)

    def ||(that: EmailFilter): EmailFilter = EmailFilter.InclusiveOr(self, that)

    def ^^(that: EmailFilter): EmailFilter = EmailFilter.ExclusiveOr(self, that)
  end EmailFilter
  object EmailFilter:
    val always: EmailFilter = Always

    val never: EmailFilter = Not(Always)

    def senderIs(sender: Address): EmailFilter = FieldPredicate(senderField, _ == List(sender))

    def senderIsNot(sender: Address): EmailFilter = Not(senderIs(sender))

    def recipientIs(recipient: Address): EmailFilter = FieldPredicate(toField, _ == List(recipient))

    def recipientIsNot(recipient: Address): EmailFilter = Not(recipientIs(recipient))

    def senderIn(senders: Set[Address]): EmailFilter = FieldPredicate(senderField, _.toSet == senders)

    def recipientIn(recipients: Set[Address]): EmailFilter = FieldPredicate(toField, _.toSet == recipients)

    def bodyContains(phrase: String): EmailFilter = FieldPredicate(bodyField, _.forall(_.contains(phrase)))

    def bodyDoesNotContain(phrase: String): EmailFilter = Not(bodyContains(phrase))

    def subjectContains(phrase: String): EmailFilter = FieldPredicate(subjectField, _.head.contains(phrase))

    def subjectDoesNotContain(phrase: String): EmailFilter = Not(subjectContains(phrase))
  end EmailFilter
end email_filter3

/** COMPOSABILITY - EXERCISE SET 2
  */
object ui_components:

  // Один из возможных способов интерпретации состояния черепахи - 
  // хранить только текущее положение черепахи.
  // Можно также хранить список нарисованных линий.
  final case class TurtleState(position: (Double, Double), direction: Int)
  object executable:
    /** EXERCISE 1
      *
      * The following API is not composable—there is no domain. Introduce a domain with elements,
      * constructors, and composable operators. Use an executable model.
      */
    type Turtle = TurtleState => TurtleState
    object Turtle:

      def turnLeft(degrees: Int): Turtle = 
        s => s.copy(direction = (s.direction + 360 - degrees) % 360)

      def turnRight(degrees: Int): Turtle =
        s => s.copy(direction = (s.direction + degrees) % 360)

      def goForward(): Turtle = 
        s => s.copy(position = 
          (
            s.position._1 + math.cos(math.toRadians(s.direction)),
            s.position._2 + math.sin(math.toRadians(s.direction))
          ))

      def goBackward(): Turtle =
        s => s.copy(position = 
          (
            s.position._1 - math.cos(math.toRadians(s.direction)),
            s.position._2 - math.sin(math.toRadians(s.direction))
          ))

      def draw(init: TurtleState, commands: List[Turtle]): TurtleState = 
        commands.foldLeft(init)((s, cmd) => cmd(s))

  object declarative:
    /** EXERCISE 2
      *
      * The following API is not composable—there is no domain. Introduce a domain with elements,
      * constructors, and composable operators. Use a declarative model.
      */
    enum TurtleCommand:
      case TurnLeft(degrees: Int)
      case TurnRight(degrees: Int)
      case GoForward(steps: Double)
      case GoBackward(steps: Double)

    def draw(commands: List[TurtleCommand]): (TurtleState) => TurtleState =
      commands.foldLeft(_)((s, cmd) => cmd match
        case TurtleCommand.TurnLeft(degrees: Int) => s.copy(direction = (s.direction + 360 - degrees) % 360)
        case TurtleCommand.TurnRight(degrees: Int) => s.copy(direction = (s.direction + degrees) % 360)
        case TurtleCommand.GoForward(steps: Double) => s.copy(position = 
          (
            s.position._1 + math.cos(math.toRadians(s.direction)),
            s.position._2 + math.sin(math.toRadians(s.direction))
          ))
        case TurtleCommand.GoBackward(steps: Double) => s.copy(position = 
          (
            s.position._1 - math.cos(math.toRadians(s.direction)),
            s.position._2 - math.sin(math.toRadians(s.direction))
          ))
      )
end ui_components
