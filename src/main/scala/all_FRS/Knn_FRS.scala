package all_FRS

import Fuzzy_Distance_And_Relation._

class Knn_FRS(dataSet:Seq[Seq[Double]], allLabels:Seq[Int], k:Int, model:String) {
  def getLowerApproximateForEveryLabel(retainAttributes:Seq[Boolean]):Seq[Double] = {
    val dimension = retainAttributes.length
    var all_sample_min_apr = new Array[(Int, Double)](dataSet.length) // （label,lower_apr）
    val num_of_each_label = new Array[Int](allLabels.length)

    for (i <- 0 to all_sample_min_apr.length-1)
      all_sample_min_apr(i) = (0, Double.MaxValue)

    // 算出所有样本最小下近似
    for (i <- 0 to dataSet.length-1) {
      val i_label = dataSet(i)(dimension).toInt
      num_of_each_label(i_label) = num_of_each_label(i_label) + 1
      for (j <- i+1 to dataSet.length-1) {
        val j_label = dataSet(j)(dimension).toInt
        if (i_label != j_label) {
          val tempRelation = getFuzzyRelation(dataSet(i), dataSet(j), retainAttributes)
          if (1-tempRelation < all_sample_min_apr(i)._2) {
            all_sample_min_apr(i) = (i_label, 1-tempRelation)
          }

          if (1-tempRelation < all_sample_min_apr(j)._2) {
            all_sample_min_apr(j) = (j_label, 1-tempRelation)
          }
        }
      }
    }

    // 为算出的每个样本的最小下近似排序，把相同标签的排一起并且按下近似升序排序
    all_sample_min_apr = all_sample_min_apr.sortWith{
      case (o1, o2) => {
        if (o1._1 == o2._1)
          o1._2 < o2._2
        else
          o1._1 < o2._1
      }
    }

    // 获取每个标签最小距离的索引
    val eachLabelStart = new Array[Int](allLabels.length)
    for (i <- 0 to eachLabelStart.length-1) {
      var start = 0
      for (j <- 0 to i-1) {
        start += num_of_each_label(j)
      }
      eachLabelStart(i) = start
    }

    // 根据model：trimmed、mean、median决定下近似的计算方法
    val result = new Array[Double](allLabels.length)
    if (model == "trimmed") {
      for (i <- 0 to result.length-1) {
        val kth_min_apr = all_sample_min_apr(eachLabelStart(i) + k -1)._2
        result(i) = kth_min_apr
      }
    } else if (model == "mean") {
      for (i <- 0 to result.length-1) {
        var k_apr = 0.0
        for (j <- eachLabelStart(i) to eachLabelStart(i) + k - 1)
          k_apr += all_sample_min_apr(j)._2
        result(i) = k_apr / k
      }

    } else if (model == "median") {
      for (i <- 0 to result.length-1) {
        if (k % 2 == 0) {
          val k_median1 = all_sample_min_apr(eachLabelStart(i) + k/2 -1)._2
          val k_median2 = all_sample_min_apr(eachLabelStart(i) + k/2)._2
          result(i) = (k_median1 + k_median2) / 2
        } else
          result(i) = all_sample_min_apr(eachLabelStart(i) + k/2)._2
      }
    } else {
      println("model error, please input trimmed, mean or median")
    }

    result.toSeq
  }
}
