package de.tu_dresden.epistemic_rewriter

import de.tu_dresden.epistemic_rewriter.datatypes.Answer


case class QueryStatistics(gt: Set[Answer], a: Set[Answer]) {
  val precision: Double = truePositives /(truePositives + falsePositives)
  val recall: Double = truePositives / (truePositives + falseNegatives)
  val truePositives: Double = gt.intersect(a).size
  val falsePositives: Double = a.diff(gt).size
  val trueNegatives: Double = -1
  val falseNegatives: Double = gt.diff(a).size
  val f1measure: Double = precision * recall / (precision + recall)
}


