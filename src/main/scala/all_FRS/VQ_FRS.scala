package all_FRS

// 是VPRS的变种，即为加了上下边界u、l的VPRS，其从粗糙集转换到模糊集的逻辑为：
// 直接使用模糊集定义下的包含（min）、等价类（样本模糊关系）、模糊基数（隶属度求和）
// 其可以返回每个样本相对于di标签的隶属度
class VQ_FRS(dataSet:Seq[Seq[Double]], allLabels:Seq[Int], l:Double, u:Double) {
  // 返回值为每个样本相对于每个标签的隶属度
}
