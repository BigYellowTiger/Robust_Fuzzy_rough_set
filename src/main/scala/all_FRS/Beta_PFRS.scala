package all_FRS

class Beta_PFRS(dataSet:Seq[Seq[Double]], allLabels:Seq[Int], fuzzySimilarityThreshold:Double) {

  def getLowerApproximateForEveryLabel(retainAttributes:Seq[Boolean]):Seq[Double] = {
    Seq[Double]()
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
