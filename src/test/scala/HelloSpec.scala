import org.scalatest._

import com.example._
import scala.collection.immutable.{Nil => 円}

class HelloSpec extends FlatSpec with Matchers {

  "Hello" should """背広のドライクリーニング代金は800円、ワイシャツのドライは200円、水洗いは150円、防虫加工は品目にかかわらず一律500円である。""" in {

    val res1 = 料金ルール に品目の "背広" とサービスの "ドライクリーニング" を料金 800 円で追加する

    val res2 = 料金ルール に品目の "ワイシャツ" とサービスの "ドライクリーニング" を料金 200 円で追加する

    val res3 = 料金ルール に品目の "ワイシャツ" とサービスの "水洗い" を料金 150 円で追加する

    val res4 = 料金ルール にサービスの "防虫加工" を一律 500 円で追加する

    res1 === FeeRule(Option(Item("背広")), Service("ドライクリーニング"), 800)
    res2 === FeeRule(Option(Item("ワイシャツ")), Service("ドライクリーニング"), 200)
    res3 === FeeRule(Option(Item("ワイシャツ")), Service("水洗い"), 150)
    res4 === FeeRule(None, Service("防虫加工"), 500)

  }

  "Hello" should """徳川さんから4月18日に、背広のドライクリーニングと防虫加工、ワイシャツの水洗いと防虫加工でお預かりした。""" in {

    val 徳川さん        = 顧客 の "徳川"

    val 品目背広        = 品目 の "背広"
    val 品目ワイシャツ  = 品目 の "ワイシャツ"

    val ドライ    = サービス の "ドライクリーニング"
    val 水洗い    = サービス の "水洗い"
    val 防虫加工  = サービス の "防虫加工"

    val res1 = 取引(
      "1",
      "2016-04-18",
      徳川さん,
      Seq(
        KeepItem("kind1", "2016-04-18", 品目背広, ドライ),
        KeepItem("kind2", "2016-04-18", 品目背広, 防虫加工),
        KeepItem("kind3", "2016-04-18", 品目ワイシャツ, 水洗い),
        KeepItem("kind4", "2016-04-18", 品目ワイシャツ, 防虫加工)
      )
    )

  res1.totalFee === 1650
  }

  "Hello" should """徳川さんが4月21日に取りに見えたので、すでにできていたワイシャツだけをお返しした。""" in {
    true should === (true)
  }

}
