package all_FRS

import all_FRS.Fuzzy_Distance_And_Relation.getFuzzyRelation

// 模糊变精度粗糙集，对于按决策值清晰划分的crisp set来说，就是把S算子第二个参数由0或1变为了a，a属于[0,1)
class Fvp_RS(dataSet:Seq[Seq[Double]], allLabels:Seq[Int], a:Double) {
  def getLowerApproximateForEveryLabel(retainAttributes:Seq[Boolean]):Seq[Double]={
    val dimension = retainAttributes.length
    val result = new Array[Double](allLabels.length) // 记录了每个标签的下近似取值
    for (i <- 0 to result.length-1)
      result(i) = Double.MaxValue

    for (i <- 0 to dataSet.length-2) {
      val i_label = dataSet(i)(dimension).toInt
      for (j <- i+1 to dataSet.length-1) {
        val j_label = dataSet(j)(dimension).toInt
        if (i_label != j_label) {
          val tempRelation = getFuzzyRelation(dataSet(i), dataSet(j), retainAttributes)
          if (1-tempRelation < result(i_label) && 1-tempRelation >= a)
            result(i_label) = 1-tempRelation
          if (1-tempRelation < result(j_label) && 1-tempRelation >= a)
            result(j_label) = 1-tempRelation
        }
      }
    }

    // 没有满足要求的模糊关系的下近似值用a代替
    for (i <- 0 to result.length-1) {
      if (result(i) > 1)
        result(i) = a
    }

    result.toSeq
  }
}
