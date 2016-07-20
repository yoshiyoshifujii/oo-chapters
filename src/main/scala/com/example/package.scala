package com

package object example {

  val 顧客 = Customer
  val 取引 = Transaction
  val お預かり品 = KeepItem
  val 品目 = Item
  val 料金ルール = FeeRule
  val サービス = Service
  val タグ = Tag

  type Address = Option[String]
  type PhoneNumber = Option[String]
  type ReferenceNumber = String
  type DateTime = String
  type Money = Int
  type Kind = String
  type ItemName = String
  type ServiceName = String
  type TagNo = String
  type Services = Seq[Service]

  case class Customer(name: String,
                      address: Address = None,
                      phoneNumber: PhoneNumber = None)

  case class Transaction(referenceNumber: ReferenceNumber,
                         dateTime: DateTime,
                         customer: Customer,
                         keepItems: Seq[KeepItem]) {
    def totalFee: Money = keepItems.foldLeft(0){(a,b) => a + b.fee}
  }

  case class KeepItem(kind: Kind,
                      moveDateTime: DateTime,
                      item: Item,
                      service: Service) {
    def fee: Money = {
      (item, service) match {
        case (_, Service("防虫加工")) => FeeRules.values.get("防虫加工").map(_.money).getOrElse(0)
        case (Item(i), Service(s)) => FeeRules.values.get(i + s).map(_.money).getOrElse(0)
        case _ => 0
      }
    }
  }

  case class Item(name: ItemName) {
    def とサービスの(name: ServiceName) = ItemServiceTemp(this, Service(name))
  }

  case class FeeRule(item: Option[Item], service: Service, money: Money) {
    def 円で追加する = this
  }

  case class Service(name: ServiceName) {
    def を一律(money: Money) = FeeRule(None, this, money)
  }

  case class Tag(number: TagNo)

  object FeeRules {
    lazy val values = Map(
      "背広ドライクリーニング" -> FeeRule(Option(Item("背広")), Service("ドライクリーニング"), 800),
      "ワイシャツドライクリーニング" -> FeeRule(Option(Item("ワイシャツ")), Service("ドライクリーニング"), 200),
      "ワイシャツ水洗い" -> FeeRule(Option(Item("ワイシャツ")), Service("水洗い"), 150),
      "防虫加工" -> FeeRule(None, Service("防虫加工"), 500))
  }

  object Customer {
    def の(name: String) = Customer(name)
  }

  object Item {
    def の(name: ItemName) = Item(name)
  }

  object Service {
    def の(name: ServiceName) = Service(name)
  }

  case class ItemServiceTemp(item: Item, service: Service) {
    def を料金(money: Money) = FeeRule(Option(item), service, money)
  }

  object FeeRule {
    def に品目の(name: ItemName) = Item(name)
    def にサービスの(name: ServiceName) = Service(name)
  }

}

