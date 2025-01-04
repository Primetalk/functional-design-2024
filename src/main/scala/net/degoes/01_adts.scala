package net.degoes

import java.time.{Instant, LocalDate}
import java.util.Currency

/*
 * INTRODUCTION
 *
 * Functional Design depends heavily on functional data modeling. Functional
 * data modeling is the task of creating precise, type-safe models of a given
 * domain using algebraic data types and generalized algebraic data types.
 *
 * In this section, you'll review basic functional domain modeling.
 */

/** E-COMMERCE - EXERCISE SET 1
  *
  * Consider an e-commerce application that allows users to purchase products.
  */
object credit_card:

  /** EXERCISE 1
    *
    * Using only enums and case classes, create an immutable data model of a credit card, which must
    * have:
    *
    * * Number * Name * Expiration date * Security code
    */
//  type CreditCard
  case class CreditCard(number: String, name: String, expirationDate: LocalDate, securityCode: String)

  /** EXERCISE 2
    *
    * Using only enums and case classes, create an immutable data model of a product, which could be
    * a physical product, such as a gallon of milk, or a digital product, such as a book or movie,
    * or access to an event, such as a music concert or film showing.
    */
//  type Product
  sealed trait Product

  enum PhysicalProduct extends Product:
    case GallonOfMilk

  enum DigitalProduct extends Product:
    case Book
    case Movie

  enum EventAccess extends Product:
    case MusicConcert
    case FilmShowing

  /** EXERCISE 3
    *
    * Using only enums and case classes, create an immutable data model of a product price, which
    * could be one-time purchase fee, or a recurring fee on some regular interval.
    */
//  type PricingScheme
  enum PricingScheme:
    case OneTimeFee(fee: BigDecimal)
    case RecurringFee(fee: BigDecimal, interval: Instant)

end credit_card

/** EVENT PROCESSING - EXERCISE SET 3
  *
  * Consider an event processing application, which processes events from both devices, as well as
  * users.
  */
object events:

  /** EXERCISE
    *
    * Refactor the object-oriented data model in this section to a more functional one, which uses
    * only enums and case classes.
    */

  sealed trait Event:
    def id: Int

  enum UserEvent(id: Int, time: Instant, userName: String) extends Event:
    case UserPurchase(id: Int, item: String, price: Double, time: Instant, userName: String) extends UserEvent(id, time, userName)
    case UserAccountCreated(id: Int, userName: String, time: Instant) extends UserEvent(id, time, userName)

  enum DeviceEvent(id: Int, time: Instant, deviceId: Int) extends Event:
    case SensorUpdated(id: Int, deviceId: Int, time: Instant, reading: Option[Double]) extends DeviceEvent(id, time, deviceId)
    case DeviceActivated(id: Int, deviceId: Int, time: Instant) extends DeviceEvent(id, time, deviceId)

  // Events are either UserEvent (produced by a user) or DeviceEvent (produced by a device),
  // please don't extend both it will break code!!!
//  trait UserEvent extends Event:
//    def userName: String

  // Events are either UserEvent (produced by a user) or DeviceEvent (produced by a device),
  // please don't extend both it will break code!!!
//  trait DeviceEvent extends Event:
//    def deviceId: Int

//  class SensorUpdated(id: Int, val deviceId: Int, val time: Instant, val reading: Option[Double])
//      extends Event(id)
//      with DeviceEvent
//
//  class DeviceActivated(id: Int, val deviceId: Int, val time: Instant)
//      extends Event(id)
//      with DeviceEvent
//
//  class UserPurchase(
//    id: Int,
//    val item: String,
//    val price: Double,
//    val time: Instant,
//    val userName: String
//  ) extends Event(id)
//      with UserEvent
//
//  class UserAccountCreated(id: Int, val userName: String, val time: Instant)
//      extends Event(id)
//      with UserEvent
end events

/** DOCUMENT EDITING - EXERCISE SET 4
  *
  * Consider a web application that allows users to edit and store documents of some type (which is
  * not relevant for these exercises).
  */
object documents:
  final case class UserId(identifier: String)
  final case class DocId(identifier: String)
  final case class DocContent(body: String)

  /** EXERCISE 1
    *
    * Using only enums and case classes, create a simplified but somewhat realistic model of a
    * Document.
    */
//  type Document
  case class Document(id: DocId, content: DocContent)

  /** EXERCISE 2
    *
    * Using only enums and case classes, create a model of the access type that a given user might
    * have with respect to a document. For example, some users might have read-only permission on a
    * document.
    */
//  type AccessType
  enum AccessType:
    case ReadOnly
    case WriteOnly
    case FullAccess

  /** EXERCISE 3
    *
    * Using only enums and case classes, create a model of the permissions that a user has on a set
    * of documents they have access to. Do not store the document contents themselves in this model.
    */
//  type DocPermissions
  case class DocPermissions(userId: UserId, docId: DocId, accessType: AccessType)

end documents

/** BANKING - EXERCISE SET 5
  *
  * Consider a banking application that allows users to hold and transfer money.
  */
object bank:

  /** EXERCISE 1
    *
    * Using only enums and case classes, develop a model of a customer at a bank.
    */
//  type Customer
  case class Customer(id: Int, name: String)

  /** EXERCISE 2
    *
    * Using only enums and case classes, develop a model of an account type. For example, one
    * account type allows the user to write checks against a given currency. Another account type
    * allows the user to earn interest at a given rate for the holdings in a given currency.
    */
//  type AccountType
  // questionable practice (?). This enum can grow indefinitely as more cases are added
  enum AccountType:
    case WriteCheck(currency: Currency)
    case EarnInterest(rate: BigDecimal)

  /** EXERCISE 3
    *
    * Using only enums and case classes, develop a model of a bank account, including details on the
    * type of bank account, holdings, customer who owns the bank account, and customers who have
    * access to the bank account.
    */
//  type Account
  case class Account(accountType: AccountType, holdings: BigDecimal, owners: Set[Customer], customerWithAccess: Set[Customer])
end bank

/** STOCK PORTFOLIO - GRADUATION PROJECT
  *
  * Consider a web application that allows users to manage their portfolio of investments.
  */
object portfolio:

  /** EXERCISE 1
    *
    * Using only enums and case classes, develop a model of a stock exchange. Ensure there exist
    * values for NASDAQ and NYSE.
    */
  type Exchange

  /** EXERCISE 2
    *
    * Using only enums and case classes, develop a model of a currency type.
    */
  type CurrencyType

  /** EXERCISE 3
    *
    * Using only enums and case classes, develop a model of a stock symbol. Ensure there exists a
    * value for Apple's stock (APPL).
    */
  type StockSymbol

  /** EXERCISE 4
    *
    * Using only enums and case classes, develop a model of a portfolio held by a user of the web
    * application.
    */
  type Portfolio

  /** EXERCISE 5
    *
    * Using only enums and case classes, develop a model of a user of the web application.
    */
  type User

  /** EXERCISE 6
    *
    * Using only enums and case classes, develop a model of a trade type. Example trade types might
    * include Buy and Sell.
    */
  type TradeType

  /** EXERCISE 7
    *
    * Using only enums and case classes, develop a model of a trade, which involves a particular
    * trade type of a specific stock symbol at specific prices.
    */
  type Trade
end portfolio
