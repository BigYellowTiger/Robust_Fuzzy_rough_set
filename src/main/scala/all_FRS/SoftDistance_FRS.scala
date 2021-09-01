package all_FRS

import all_FRS.Fuzzy_Distance_And_Relation._

// 计算每个样本与异类样本的软距离，排序后取最小软距离用于下近似计算，punishArgue为惩罚系数
class SoftDistance_FRS(dataSet:Seq[Seq[Double]], allLabels:Seq[Int], punishArgue:Double) {
  def getLowerApproximateForEveryLabel(retainAttributes:Seq[Boolean]):Seq[Double] = {
    val dimension = retainAttributes.length
    var all_sample_min_hdis = new Array[(Int, Double)](dataSet.length) // 硬距离（label,lower_apr）
    var all_sample_min_sdis = new Array[(Int, Double)](dataSet.length) // 软距离（label,lower_apr）
    val num_of_each_label = new Array[Int](allLabels.length)

    for (i <- 0 to all_sample_min_hdis.length-1)
      all_sample_min_hdis(i) = (0, Double.MaxValue)

    // 计算硬距离
    for (i <- 0 to dataSet.length-1) {
      val i_label = dataSet(i)(dimension).toInt
      num_of_each_label(i_label) = num_of_each_label(i_label) + 1
      for (j <- i+1 to dataSet.length-1) {
        val j_label = dataSet(j)(dimension).toInt
        if (i_label != j_label) {
          val temp_hdis = getDistance(dataSet(i), dataSet(j), retainAttributes)

          if (temp_hdis < all_sample_min_hdis(i)._2)
            all_sample_min_hdis(i) = (i_label, temp_hdis)
          if (temp_hdis < all_sample_min_hdis(j)._2)
            all_sample_min_hdis(j) = (j_label, temp_hdis)
        }
      }
    }

    // 硬距离按标签升序排序
    all_sample_min_hdis = all_sample_min_hdis.sortWith{
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
    for (i <- 0 to all_sample_min_hdis.length-1) {
      val i_label = all_sample_min_hdis(i)._1
      all_sample_min_sdis(i) = (i_label
        , all_sample_min_hdis(i)._2 - punishArgue * (i - eachLabelStart(i_label)))
    }

    // 软距离按标签升序排序
    all_sample_min_sdis = all_sample_min_sdis.sortWith{
      case (o1, o2) => {
        if (o1._1 == o2._1)
          o1._2 < o2._2
        else
          o1._1 < o2._1
      }
    }

    val result = new Array[Double](allLabels.length)
    for (i <- 0 to eachLabelStart.length-1) {
      val ith_label_sd = all_sample_min_sdis(eachLabelStart(i))._2
      result(i) = getFuzzyRelation(ith_label_sd)
    }

    result.toSeq
  }
}
