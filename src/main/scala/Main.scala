import scala.io.Source
import all_FRS.{Classic_FRS, DiffClassRatio_FRS}

object Main {
  def main(args: Array[String]): Unit = {
    println(9.0/15.0)
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
    val lower_approximate = diffClassRatio_FRS.getLowerApproximateForEveryLabel(retainAttributes)
    for (obj <- lower_approximate)
      println(obj)
  }
}
