package net.degoes

import java.time.Instant

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
  final case class CreditCard(number: String, name: String, expirationDate: Instant, securityCode: String)


  /** EXERCISE 2
   *
   * Using only enums and case classes, create an immutable data model of a product, which could be
   * a physical product, such as a gallon of milk, or a digital product, such as a book or movie,
   * or access to an event, such as a music concert or film showing.
   */
  sealed trait Product
  final case class PhysicalProduct(name: String) extends Product
  final case class DigitalProduct(name: String) extends Product
  final case class EventAccess(eventName: String, date: Instant) extends Product
  //type Product

  /** EXERCISE 3
   *
   * Using only enums and case classes, create an immutable data model of a product price, which
   * could be one-time purchase fee, or a recurring fee on some regular interval.
   */
  sealed trait PricingScheme
  final case class OneTimePrice(price: Double) extends PricingScheme
  final case class RecurringPrice(price: Double, intervalDays: Int) extends PricingScheme
  //type PricingScheme

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
    def time: Instant

  trait UserEvent extends Event:
    def userName: String

  trait DeviceEvent extends Event:
    def deviceId: Int

  final case class SensorUpdated(id: Int,
                                 time: Instant,
                                 deviceId: Int,
                                 reading: Option[Double]) extends Event with DeviceEvent
  final case class DeviceActivated(id: Int,
                                   time: Instant,
                                   deviceId: Int) extends Event with DeviceEvent
  final case class UserPurchase(id: Int,
                                time: Instant,
                                item: String,
                                price: Double,
                                userName: String ) extends Event with UserEvent
  final case class UserAccountCreated(id: Int,
                                      time: Instant,
                                      userName: String ) extends Event with UserEvent
  /*
    abstract class Event(val id: Int):

      def time: Instant

    // Events are either UserEvent (produced by a user) or DeviceEvent (produced by a device),
    // please don't extend both it will break code!!!
    trait UserEvent extends Event:
      def userName: String

    // Events are either UserEvent (produced by a user) or DeviceEvent (produced by a device),
    // please don't extend both it will break code!!!
    trait DeviceEvent extends Event:
      def deviceId: Int

    class SensorUpdated(id: Int, val deviceId: Int, val time: Instant, val reading: Option[Double])
        extends Event(id)
        with DeviceEvent

    class DeviceActivated(id: Int, val deviceId: Int, val time: Instant)
        extends Event(id)
        with DeviceEvent

    class UserPurchase(
      id: Int,
      val item: String,
      val price: Double,
      val time: Instant,
      val userName: String
    ) extends Event(id)
        with UserEvent

    class UserAccountCreated(id: Int, val userName: String, val time: Instant)
        extends Event(id)
        with UserEvent
  */
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
  final case class Document( id: DocId,
                             content: DocContent,
                             author: UserId,
                             createdDate: Instant)
  //type Document

  /** EXERCISE 2
   *
   * Using only enums and case classes, create a model of the access type that a given user might
   * have with respect to a document. For example, some users might have read-only permission on a
   * document.
   */
  sealed trait AccessType
  final case object NoAccess extends AccessType
  final case object OnlyRead extends AccessType
  final case object ReadWrite extends AccessType
  //type AccessType

  /** EXERCISE 3
   *
   * Using only enums and case classes, create a model of the permissions that a user has on a set
   * of documents they have access to. Do not store the document contents themselves in this model.
   */
  final case class DocPermissions( documentId: DocId,
                                   accessType: AccessType)
  //type DocPermissions
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
  final case class Customer(
                             id: Int,
                             name: String
                           )
  //type Customer

  /** EXERCISE 2
   *
   * Using only enums and case classes, develop a model of an account type. For example, one
   * account type allows the user to write checks against a given currency. Another account type
   * allows the user to earn interest at a given rate for the holdings in a given currency.
   */
  sealed trait AccountType
  final case class CheckingAccount(currency: String) extends AccountType
  final case class SavingsAccount(currency: String, interestRate: Double) extends AccountType
  //type AccountType

  /** EXERCISE 3
   *
   * Using only enums and case classes, develop a model of a bank account, including details on the
   * type of bank account, holdings, customer who owns the bank account, and customers who have
   * access to the bank account.
   */
  final case class Account(
                            id: Int,
                            accountType: AccountType,
                            balance: Double,
                            owner: Customer,
                            authorizedUsers: List[Customer]
                          )
  //type Account
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
  sealed trait Exchange
  final case object NASDAQ extends Exchange
  final case object NYSE extends Exchange
  //type Exchange

  /** EXERCISE 2
   *
   * Using only enums and case classes, develop a model of a currency type.
   */
  sealed trait CurrencyType
  final case object RUB extends CurrencyType
  final case object EUR extends CurrencyType
  final case object USD extends CurrencyType
  //type CurrencyType

  /** EXERCISE 3
   *
   * Using only enums and case classes, develop a model of a stock symbol. Ensure there exists a
   * value for Apple's stock (APPL).
   */
  sealed trait StockSymbol

  final case object APPL extends StockSymbol
  final case object GOOG extends StockSymbol
  //type StockSymbol

  /** EXERCISE 4
   *
   * Using only enums and case classes, develop a model of a portfolio held by a user of the web
   * application.
   */
  final case class Portfolio(
                              owner: User,
                              investments: Map[StockSymbol, Double]
                            )
  //type Portfolio

  /** EXERCISE 5
   *
   * Using only enums and case classes, develop a model of a user of the web application.
   */
  final case class User(
                         id: Int,
                         name: String,
                         email: String
                       )
  //type User

  /** EXERCISE 6
   *
   * Using only enums and case classes, develop a model of a trade type. Example trade types might
   * include Buy and Sell.
   */
  sealed trait TradeType
  final case object Buy extends TradeType
  final case object Sell extends TradeType

  //type TradeType

  /** EXERCISE 7
   *
   * Using only enums and case classes, develop a model of a trade, which involves a particular
   * trade type of a specific stock symbol at specific prices.
   */
  final case class Trade(
                          tradeType: TradeType,
                          stockSymbol: StockSymbol,
                          price: Double,
                          amount: Double
                        )
  //type Trade
end portfolio
