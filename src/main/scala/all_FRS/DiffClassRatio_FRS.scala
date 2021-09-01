package all_FRS

import all_FRS.Fuzzy_Distance_And_Relation.{getFuzzyRelation, getDistance}

// 仅能处理有标签数据集
class DiffClassRatio_FRS(dataSet:Seq[Seq[Double]], allLabels:Seq[Int], sphereRadius:Double, diffClassThreshold:Double) {

  // 下近似计算方法为Di中每个样本构造的超球面内异类样本占比，而且根据阈值对样本的隶属度划分为0或1
  // 仅有异类率满足要求的样本会参与下近似计算
  def getLowerApproximateForEveryLabel(retainAttributes:Seq[Boolean]):Seq[Double] = {
    val dimension = retainAttributes.length
    val all_dc_ratio = new Array[Double](dataSet.length)
    val result = new Array[Double](allLabels.length) // 记录了每个标签的下近似取值
    for (i <- 0 to result.length-1)
      result(i) = Int.MaxValue

    // 算出每个样本的异类率
    for (i <- 0 to dataSet.length-1) {
      val i_label = dataSet(i)(dimension).toInt
      var diffClassNum = 0.0
      var totalNum = 0.0
      for (j <- 0 to dataSet.length - 1) {
        val j_label = dataSet(j)(dimension).toInt
        if (getDistance(dataSet(i), dataSet(j), retainAttributes) <= sphereRadius * sphereRadius) {
          totalNum += 1
          if (j_label != i_label)
            diffClassNum += 1
        }
      }
      val dc_ratio = diffClassNum/totalNum
      all_dc_ratio(i) = dc_ratio
    }

    // 仅有异类率满足要求的样本参与模糊关系的运算
    for (i <- 0 to dataSet.length-2) {
      val i_label = dataSet(i)(dimension).toInt
      for (j <- i+1 to dataSet.length-1) {
        val j_label = dataSet(j)(dimension).toInt
        if (i_label != j_label && all_dc_ratio(i) <= diffClassThreshold && all_dc_ratio(j) <= diffClassThreshold) {
          val tempRelation = getFuzzyRelation(dataSet(i), dataSet(j), retainAttributes)
          if (1-tempRelation < result(i_label))
            result(i_label) = 1-tempRelation
          if (1-tempRelation < result(j_label))
            result(j_label) = 1-tempRelation
        }
      }
    }

    result.toSeq
  }
}
