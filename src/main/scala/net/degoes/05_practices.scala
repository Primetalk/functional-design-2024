package net.degoes

import net.degoes.ui_components.declarative.Action.Draw

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

  /** EXERCISE 1
    *
    * In the following model, which describes an email filter, there are many primitives with
    * overlapping responsibilities. Find the smallest possible set of primitive operators and
    * constructors, without deleting any constructors or operators (you may implement them in terms
    * of primitives).
    *
    * NOTE: You may *not* use a final encoding, which would allow you to collapse everything down to
    * one primitive.
    */
  enum EmailFilter:
    case Always
    case Never
    case And(left: EmailFilter, right: EmailFilter)
    case InclusiveOr(left: EmailFilter, right: EmailFilter)

    // additional cases that I added
    case Not(filter: EmailFilter)
    case StringContains(phrase: String, f: Email => String)
    case AddressesIntersect(addresses: Set[Address], f: Email => Set[Address])


    // now every case below can be expressed by a combination of cases above
//    case ExclusiveOr(left: EmailFilter, right: EmailFilter) // now can be expressed as And(Or(a, b), Not(And(a, b)))
//    case SenderEquals(target: Address)
//    case SenderNotEquals(target: Address)
//    case RecipientEquals(target: Address)
//    case RecipientNotEquals(target: Address)
//    case SenderIn(targets: Set[Address])
//    case RecipientIn(targets: Set[Address])
//    case BodyContains(phrase: String)
//    case BodyNotContains(phrase: String)
//    case SubjectContains(phrase: String)
//    case SubjectNotContains(phrase: String)

    def self = this

    def &&(that: EmailFilter): EmailFilter = EmailFilter.And(self, that)

    def ||(that: EmailFilter): EmailFilter = EmailFilter.InclusiveOr(self, that)

    def ^^(that: EmailFilter): EmailFilter = (self || that) && Not(self && that)
  end EmailFilter
  object EmailFilter:
    val always: EmailFilter = Always

    val never: EmailFilter = Never

    def senderIs(sender: Address): EmailFilter = AddressesIntersect(Set(sender), email => Set(email.sender))

    def senderIsNot(sender: Address): EmailFilter = Not(senderIs(sender))

    def recipientIs(recipient: Address): EmailFilter = AddressesIntersect(Set(recipient), _.to.toSet)

    def recipientIsNot(recipient: Address): EmailFilter = Not(recipientIs(recipient))

    def senderIn(senders: Set[Address]): EmailFilter = AddressesIntersect(senders, email => Set(email.sender))

    def recipientIn(recipients: Set[Address]): EmailFilter = AddressesIntersect(recipients, _.to.toSet)

    def bodyContains(phrase: String): EmailFilter = StringContains(phrase, _.body)

    def bodyDoesNotContain(phrase: String): EmailFilter = Not(bodyContains(phrase))

    def subjectContains(phrase: String): EmailFilter = StringContains(phrase, _.subject)

    def subjectDoesNotContain(phrase: String): EmailFilter = Not(subjectContains(phrase))

  end EmailFilter
end email_filter3

/** COMPOSABILITY - EXERCISE SET 2
  */
object ui_components:

  object executable:
    /** EXERCISE 1
      *
      * The following API is not composable—there is no domain. Introduce a domain with elements,
      * constructors, and composable operators. Use an executable model.
      */
    trait Turtle:
      self =>
      def turnLeft(degrees: Int): Unit

      def turnRight(degrees: Int): Unit

      def goForward(): Unit

      def goBackward(): Unit

      def draw(): Unit

  object declarative:
    /** EXERCISE 2
      *
      * The following API is not composable—there is no domain. Introduce a domain with elements,
      * constructors, and composable operators. Use a declarative model.
      */

    enum Action:
      case TurnLeft(degrees: Int)
      case TurnRight(degrees: Int)
      case GoForward
      case GoBackward
      case Draw

      // similar to `And` or `Concatenate`. It should be used to make an ordered sequence of actions.
      case Then(action: Action, nextAction: Action)

    trait Turtle:
      self =>
      def turnLeft(degrees: Int): Action =
        Action.TurnLeft(degrees)

      def turnRight(degrees: Int): Action =
        Action.TurnRight(degrees)

      def goForward(): Action =
        Action.GoForward

      def goBackward(): Action =
        Action.GoBackward

      def draw(): Action =
        Draw

end ui_components
