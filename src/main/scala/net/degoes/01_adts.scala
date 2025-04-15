package net.degoes

import java.time.Instant
import java.time.{ LocalDate => Date }
import zio.Duration
import javax.print.Doc

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
  final case class CreditCard(
    number: String,
    name: String,
    expirationDate: Date,
    securityCode: String
  )

  /** EXERCISE 2
    *
    * Using only enums and case classes, create an immutable data model of a product, which could be
    * a physical product, such as a gallon of milk, or a digital product, such as a book or movie,
    * or access to an event, such as a music concert or film showing.
    */
  enum Product:
    case PhysicalProduct(kind: PhysicalProductKind, amount: PhysicalProductAmount)
    case DigitalProduct(kind: DigitalProductKind)
    case AccessToAnEvent(eventKind: EventKind)

  enum PhysicalProductKind:
    case Milk
  enum PhysicalProductAmount:
    case Gallon
  enum DigitalProductKind:
    case Book, Movie
  enum EventKind:
    case MusicConcert, FilmShowing

  /** EXERCISE 3
    *
    * Using only enums and case classes, create an immutable data model of a product price, which
    * could be one-time purchase fee, or a recurring fee on some regular interval.
    */
  enum PricingScheme:
    case OneTimePurchaseFee(amount: BigDecimal)
    case RecurringFee(fee: BigDecimal, interval: Duration)
end credit_card

/** EVENT PROCESSING - EXERCISE SET 3
  *
  * Consider an event processing application, which processes events from both devices, as well as
  * users.
  */
object events:

  // Events are either UserEvent (produced by a user) or DeviceEvent (produced by a device),
  enum EventSource:
    case User(userName: String)
    case Device(deviceId: Int)

  /** EXERCISE
    *
    * Refactor the object-oriented data model in this section to a more functional one, which uses
    * only enums and case classes.
    */
  final case class EventId(id: Int, time: Instant)

  sealed trait Event:
    def eventId: EventId
    def eventSource: EventSource

  sealed trait DeviceEvent extends Event:
    def eventSource: EventSource.Device

  sealed trait UserEvent extends Event:
    def eventSource: EventSource.User

  final case class SensorUpdated(
    eventId: EventId,
    eventSource: EventSource.Device,
    reading: Option[Double]
  ) extends DeviceEvent

  final case class DeviceActivated(eventId: EventId, eventSource: EventSource.Device)
      extends DeviceEvent

  final case class UserPurchase(
    eventId: EventId,
    eventSource: EventSource.User,
    item: String,
    price: Double
  ) extends UserEvent

  final case class UserAccountCreated(eventId: EventId, eventSource: EventSource.User)
      extends UserEvent
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
  final case class Document(id: DocId, content: DocContent, author: UserId)

  /** EXERCISE 2
    *
    * Using only enums and case classes, create a model of the access type that a given user might
    * have with respect to a document. For example, some users might have read-only permission on a
    * document.
    */
  enum AccessType:
    case NoAccess, ReadOnly, ReadWrite

  /** EXERCISE 3
    *
    * Using only enums and case classes, create a model of the permissions that a user has on a set
    * of documents they have access to. Do not store the document contents themselves in this model.
    */
  final case class DocPermissions(permissions: List[DocPermission])
  final case class DocPermission(subj: UserId, obj: DocId, access: AccessType)
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
  final case class CustomerId(identifier: String)
  final case class Customer(id: CustomerId, name: String)

  enum CurrencyType:
    case RUR, USD, CNY

  /** EXERCISE 2
    *
    * Using only enums and case classes, develop a model of an account type. For example, one
    * account type allows the user to write checks against a given currency. Another account type
    * allows the user to earn interest at a given rate for the holdings in a given currency.
    */
  enum AccountType:
    case CheckingAccount(currency: CurrencyType)
    case Deposit(interestRate: BigDecimal, currency: CurrencyType)

  /** EXERCISE 3
    *
    * Using only enums and case classes, develop a model of a bank account, including details on the
    * type of bank account, holdings, customer who owns the bank account, and customers who have
    * access to the bank account.
    */
  final case class AccountId(identifier: String)
  final case class Account(
    number: AccountId,
    owner: CustomerId,
    holdings: BigDecimal,
    accountType: AccountType
  )
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
  enum Exchange:
    case NASDAQ, NYSE, MOEX, SPBE, SSE

  /** EXERCISE 2
    *
    * Using only enums and case classes, develop a model of a currency type.
    */
  enum CurrencyType:
    case RUR, USD, CNY

  /** EXERCISE 3
    *
    * Using only enums and case classes, develop a model of a stock symbol. Ensure there exists a
    * value for Apple's stock (APPL).
    */
  enum StockSymbol:
    case APPL, NVDA, GOOGL, MSFT, TCSG, YDEX, MGNT

  /** EXERCISE 4
    *
    * Using only enums and case classes, develop a model of a portfolio held by a user of the web
    * application.
    */
  final case class Stock(amount: BigDecimal, symbol: StockSymbol)
  final case class Portfolio(stocks: List[Stock])

  /** EXERCISE 5
    *
    * Using only enums and case classes, develop a model of a user of the web application.
    */
  final case class User(identifier: String)

  /** EXERCISE 6
    *
    * Using only enums and case classes, develop a model of a trade type. Example trade types might
    * include Buy and Sell.
    */
  enum TradeType:
    case Buy, Sell

  /** EXERCISE 7
    *
    * Using only enums and case classes, develop a model of a trade, which involves a particular
    * trade type of a specific stock symbol at specific prices.
    */
  final case class Trade(src: User, dst: User, symbol: StockSymbol, amount: BigDecimal)
end portfolio
