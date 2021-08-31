package all_FRS
import scala.math.pow

object Fuzzy_Distance_And_Relation {
  // 默认采用欧式距离的平方，以后有需要再添加别的距离
  def getDistance(x:Seq[Double], y:Seq[Double], retainAttributes:Seq[Boolean]):Double = {
    var dis:Double = 0.0

    for (i <-0 to retainAttributes.length-1) {
      if (retainAttributes(i)) {
        dis += (x(i)-y(i)) * (x(i)-y(i))
      }
    }
    dis
  }

  // 默认采用高斯核函数，以后有需要再添加别的相似度计算方法
  def getFuzzyRelation(x:Seq[Double], y:Seq[Double], retainAttributes:Seq[Boolean]):Double = {
//    val para = 2 // 高斯核函数参数
//    pow(2.7183, -getDistance(x, y, deleteArray) / para)
    1-getDistance(x, y, retainAttributes)
  }
}
