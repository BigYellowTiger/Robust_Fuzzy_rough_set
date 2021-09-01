import scala.io.Source
import all_FRS._

object Main {
  def main(args: Array[String]): Unit = {
    val allLabels = Seq(0,1,2)
    val retainAttributes = Seq(true, true)
    var dataSet = Seq[Seq[Double]]()
    val dataFile = Source.fromFile("D:\\all_Project\\classic_robust_fuzzy_rough_sets_classifier_and_attribute_reduction\\src\\main\\scala\\test_data\\two_dimension_label_data.csv")
    for (line <- dataFile.getLines()) {
      val tempArr = line.split(",")
      var tempSeq = Seq[Double]()
      for (obj <- tempArr)
        tempSeq = tempSeq :+ obj.toDouble
      dataSet = dataSet :+ tempSeq
    }

    val classic_FRS = new Classic_FRS(dataSet, allLabels, 0.1)
    val diffClassRatio_FRS = new DiffClassRatio_FRS(dataSet, allLabels, 12, 0.5)
    val beta_PFRS = new Beta_PFRS(dataSet, allLabels, 0)
    val softDistance_FRS = new SoftDistance_FRS(dataSet, allLabels, 0.8)
    val knn_FRS = new Knn_FRS(dataSet, allLabels, k=3, "median")
    val lower_approximate = knn_FRS.getLowerApproximateForEveryLabel(retainAttributes)
    for (obj <- lower_approximate)
      println(obj)
  }
}
