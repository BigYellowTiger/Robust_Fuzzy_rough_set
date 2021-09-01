package all_FRS

import all_FRS.Fuzzy_Distance_And_Relation.getFuzzyRelation

// 通过beta参数控制舍弃样本个数占比
class Beta_PFRS(dataSet:Seq[Seq[Double]], allLabels:Seq[Int], beta:Double) {

  // 算出所有label的1-R后排序舍弃最小的几个，用数组记录所有label的1-R后，排序算出n值再挑选最小的
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

    // 为算出的每个样本的最小下近似排序，把相同标签的排一起并且按下近似降序排序
    all_sample_min_apr = all_sample_min_apr.sortWith{
      case (o1, o2) => {
        if (o1._1 == o2._1)
          o1._2 > o2._2
        else
          o1._1 < o2._1
      }
    }

    // 算出每个label的n值
    val n_for_each_label = new Array[Double](allLabels.length)
    for (obj <- all_sample_min_apr)
      n_for_each_label(obj._1) = n_for_each_label(obj._1) + obj._2
    for (i <- 0 to n_for_each_label.length-1)
      n_for_each_label(i) = n_for_each_label(i)*(1-beta)

    // 每个标签取出第n个下近似值作为结果
    val result = new Array[Double](allLabels.length)
    for (i <- 0 to result.length-1) {
      val cur_i_n = n_for_each_label(i).toInt
      var start = 0
      for (i <- 0 to i-1) {
        start += num_of_each_label(i)
      }
      result(i) = all_sample_min_apr(start + cur_i_n-1)._2
    }
    result.toSeq
  }

  def getFuzzySimilarityClass(retainAttributes:Seq[Boolean]):Seq[Seq[Int]] = {
    Seq[Seq[Int]]()
  }

  def removeRepeatSimilarClass(allFuzzySimilarClass:Seq[Seq[Int]]):Seq[Seq[Int]] = {
    allFuzzySimilarClass.distinct
  }

  def getLowerApproximateOnFuzzySimilarity(deleteArray:Seq[Boolean]):(Seq[Int], Seq[Seq[Int]])={
    (Seq[Int](), Seq[Seq[Int]]())
  }
}
