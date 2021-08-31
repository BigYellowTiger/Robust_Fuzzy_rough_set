package all_FRS
import Fuzzy_Distance_And_Relation.getFuzzyRelation

class Classic_FRS(dataSet:Seq[Seq[Double]], allLabels:Seq[Int], fuzzySimilarityThreshold:Double) {
  // 标签值总在最后一个
  def getLowerApproximateForEveryLabel(retainAttributes:Seq[Boolean]):Seq[Double]={
    val dimension = retainAttributes.length
    val result = new Array[Double](allLabels.length) // 记录了每个标签的下近似取值
    for (i <- 0 to result.length-1)
      result(i) = Int.MaxValue

    for (i <- 0 to dataSet.length-2) {
      val i_label = dataSet(i)(dimension).toInt
      for (j <- i+1 to dataSet.length-1) {
        val j_label = dataSet(j)(dimension).toInt
        if (i_label != j_label) {
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

  // 返回值即为第i个样本的相似类
  def getFuzzySimilarityClass(retainAttributes:Seq[Boolean]):Seq[Seq[Int]] = {
    val class_of_each_sample = new Array[Seq[Int]](dataSet.length)

    for (i <- 0 to dataSet.length-1) {
      var temp_class = Seq[Int]()
      for (j <- 0 to dataSet.length-1) {
        val temp_relation = getFuzzyRelation(dataSet(i), dataSet(j), retainAttributes)
        if (temp_relation < fuzzySimilarityThreshold)
          temp_class = temp_class :+ j
      }
      class_of_each_sample(i) = temp_class
    }
    class_of_each_sample.toSeq
  }

  // 对模糊近似类去重
  def removeRepeatSimilarClass(allFuzzySimilarClass:Seq[Seq[Int]]):Seq[Seq[Int]] = {
    allFuzzySimilarClass.distinct
  }

  // 仅针对当前fuzzySimilarityThreshold下模糊相似类的正域
  // 第一个seq为每个模糊相似类的下近似值，第二个为每个模糊相似类所包含的元素索引
  def getLowerApproximateOnFuzzySimilarity(deleteArray:Seq[Boolean]):(Seq[Int], Seq[Seq[Int]])={
    (Seq[Int](), Seq[Seq[Int]]())
  }
}
