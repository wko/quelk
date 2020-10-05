package de.tu_dresden.epistemic_rewriter.datatypes

import scala.collection.immutable.Set
import scala.xml.{Node, NodeSeq}

/** TEMPORAL DATATYPES **/

sealed abstract class TemporalFOQuery extends Query with XMLPrintable {

  override def toXML(subxml: NodeSeq): Node = <query content={pShow}>
    {subxml}
  </query>

  def simplify(): TemporalFOQuery = {
    // TODO: Implement...
    this
  }


  def applyToAtoms(f: QueryPlusFO => QueryPlusFO): TemporalFOQuery
}

case class TemporalAtom(q: QueryPlusFO) extends TemporalFOQuery {

  override def getAnswerVars(): Set[Variable] = q.getAnswerVars() //.+(Variable("t"))

  override def pShow(): String = q.pShow()

  override def applyToAtoms(f: QueryPlusFO => QueryPlusFO): TemporalFOQuery = TemporalAtom(f(q))
}

case class TemporalConj(fs: List[TemporalFOQuery]) extends TemporalFOQuery {
  override def getAnswerVars(): Set[Variable] = fs.map(_.getAnswerVars()).flatten.toSet

  override def pShow: String = fs.size match {
    case 0 => ""
    case 1 => fs.map(_.pShow).mkString(" ∧ ")
    case _ => fs.map(_.pShow).mkString("(", " ∧ ", ")")
  }

  override def applyToAtoms(f: QueryPlusFO => QueryPlusFO): TemporalFOQuery = TemporalConj(fs.map(_.applyToAtoms(f)))
}

case class TemporalDisj(fs: List[TemporalFOQuery]) extends TemporalFOQuery {
  override def getAnswerVars(): Set[Variable] = fs.map(_.getAnswerVars()).flatten.toSet

  override def pShow: String = fs.size match {
    case 0 => ""
    case 1 => fs.map(_.pShow).mkString(" ∨ ")
    case _ => fs.map(_.pShow).mkString("(", " ∨ ", ")")
  }

  override def applyToAtoms(f: QueryPlusFO => QueryPlusFO): TemporalFOQuery = TemporalDisj(fs.map(_.applyToAtoms(f)))
}


case class RelInterval(begin: ExtPeriod, end: ExtPeriod)
case class AbsInterval(begin: ExtTimePoint, end: ExtTimePoint)

case class TemporalNeg(q: TemporalFOQuery) extends TemporalFOQuery {
  override def getAnswerVars(): Set[Variable] = q.getAnswerVars()

  override def pShow: String = "¬(" + q.pShow + ")"

  override def applyToAtoms(f: QueryPlusFO => QueryPlusFO): TemporalFOQuery = TemporalNeg(q.applyToAtoms(f))
}

case class TemporalImpl(left: TemporalFOQuery, right: TemporalFOQuery) extends TemporalFOQuery {
  override def getAnswerVars(): Set[Variable] = left.getAnswerVars().++(right.getAnswerVars())

  override def pShow: String = "[" + left.pShow + " → " + right.pShow + "]"

  override def applyToAtoms(f: QueryPlusFO => QueryPlusFO): TemporalFOQuery = TemporalImpl(left.applyToAtoms(f), right.applyToAtoms(f))
}

case class TemporalEventually(i: RelInterval, q: TemporalFOQuery) extends TemporalFOQuery {
  override def getAnswerVars(): Set[Variable] = q.getAnswerVars()

  override def pShow: String = s"࿇[${i.toString}] ( ${q.pShow()} )"

  override def applyToAtoms(f: QueryPlusFO => QueryPlusFO): TemporalFOQuery = TemporalEventually(i, q.applyToAtoms(f))
}

case class TemporalAlways(i: RelInterval, q: TemporalFOQuery) extends TemporalFOQuery {
  override def getAnswerVars(): Set[Variable] = q.getAnswerVars()

  override def pShow: String = s"΋[${i.toString}] ( ${q.pShow()} )"

  override def applyToAtoms(f: QueryPlusFO => QueryPlusFO): TemporalFOQuery = TemporalAlways(i, q.applyToAtoms(f))
}

case class TemporalConvex(n: ExtPeriod, q: TemporalFOQuery) extends TemporalFOQuery {
  override def getAnswerVars(): Set[Variable] = q.getAnswerVars()

  override def pShow: String = s"C[${n.toString}] ( ${q.pShow()} )"

  override def applyToAtoms(f: QueryPlusFO => QueryPlusFO): TemporalFOQuery = TemporalConvex(n, q.applyToAtoms(f))
}
