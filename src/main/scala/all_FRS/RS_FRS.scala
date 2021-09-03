package all_FRS

import Fuzzy_Distance_And_Relation._

// 李钰雯博士的代表性样本模糊粗糙集
// 先识别每个label下的代表性样本RS，然后到RS越近相对于该label隶属度越高
class RS_FRS(dataSet:Seq[Seq[Double]], allLabels:Seq[Int]) {
  def getLowerApproximateForEveryLabel(retainAttributes:Seq[Boolean]):Seq[Double] = {
    val dimension = retainAttributes.length
    val num_of_each_label = new Array[Int](allLabels.length)
    var dis_to_same_label = new Array[(Int, Double, Int)](dataSet.length) // (label, distance, index)
    for (i <- 0 to dis_to_same_label.length-1) {
      dis_to_same_label(i) = (0,0.0,0)
    }
    val rs_of_each_label = new Array[Seq[Double]](allLabels.length)
    val dis_to_each_rs = new Array[Seq[Double]](dataSet.length)

    // 先找每个label的RS，可以先排序再找会快一些，但此处未排序
    for (i <- 0 to dataSet.length-1) {
      val i_label = dataSet(i)(dimension).toInt
      num_of_each_label(i_label) = num_of_each_label(i_label) + 1
      for (j <- i+1 to dataSet.length-1) {
        val j_label = dataSet(j)(dimension).toInt
        if (i_label == j_label) {
          val temp_dis = getDistance(dataSet(i), dataSet(j), retainAttributes)
          dis_to_same_label(i) = (i_label, dis_to_same_label(i)._2 + temp_dis, i)
          dis_to_same_label(j) = (j_label, dis_to_same_label(j)._2 + temp_dis, j)
        }
      }
    }

    // 标签聚合后距离按升序排序，找到RS
    dis_to_same_label = dis_to_same_label.sortWith{
      case (o1, o2) => {
        if (o1._1 == o2._1)
          o1._2 < o2._2
        else
          o1._1 < o2._1
      }
    }

    for (i <- 0 to rs_of_each_label.length-1) {
      var start = 0
      for (j <- 0 to i-1)
        start += num_of_each_label(j)

      val temp_rs_index = dis_to_same_label(start)._3
      rs_of_each_label(i) = dataSet(temp_rs_index)
    }

    // 算出每个样本到每个RS的距离
    for(i <- 0 to dataSet.length-1) {
      var tempSeq = Seq[Double]()
      for (j <- 0 to rs_of_each_label.length-1) {
        val temp_dis = getDistance(dataSet(i), rs_of_each_label(j), retainAttributes)
        tempSeq = tempSeq :+ temp_dis
      }
      dis_to_each_rs(i) = tempSeq
    }

    // 基于RS算下近似
    val result = new Array[Double](allLabels.length) // 记录了每个标签的下近似取值
    for (i <- 0 to result.length-1)
      result(i) = Double.MaxValue

    for (i <- 0 to dataSet.length-1) {
      for (j <- 0 to dataSet.length-1) {
        if (i != j) {
          val tempRelation = getFuzzyRelation(dataSet(i), dataSet(j), retainAttributes)
          // 对于每一个标签，算出一个新的下近似值，如果更小则取代老值，样本本身的label不再关心
          for (cur_label_index <- 0 to allLabels.length-1) {
            var sum_of_dis_to_rs = 0.0
            for (dis_to_rs <- dis_to_each_rs(j))
              sum_of_dis_to_rs += dis_to_rs
            val tempMemberShip = 1 - dis_to_each_rs(j)(cur_label_index) / sum_of_dis_to_rs
            var tempLApr = 1 - tempRelation
            if (tempMemberShip > tempLApr)
              tempLApr = tempMemberShip
            if (tempLApr < result(cur_label_index))
              result(cur_label_index) = tempLApr
          }
        }
      }
    }

    result.toSeq
  }
}
