package de.tu_dresden.epistemic_rewriter

import de.tu_dresden.epistemic_rewriter.datatypes._


case class EpistemicConjunctiveQuery(foq: FOQuery) extends Query {
  override def pShow(): String = foq.pShow()
  def +(l: FOLiteral): EpistemicConjunctiveQuery = ++(List(l))

  def ++(ls: Traversable[FOLiteral]): EpistemicConjunctiveQuery = {
    val foqNew = foq match {
      case FOEx(ts, FOConj(qs)) => FOEx(ts, FOConj(qs.++:(ls)))
      case FOEx(ts, FOTrue()) => FOEx(ts, FOConj(ls.toList))
      case FOEx(ts, c) if (c.isInstanceOf[FOLiteral]) => FOEx(ts, FOConj(List(c.asInstanceOf[FOLiteral]).++:(ls)))
    }
    EpistemicConjunctiveQuery(foqNew)
  }

  def removeQuantifiedVariable(v: Variable): EpistemicConjunctiveQuery = {
    val foqNew = foq match {
      case FOEx(ts, f) => FOEx(ts.filterNot(_ == v), f)
      case _ => throw new Exception(s"Can not remove variable $v from query $this")
    }
    EpistemicConjunctiveQuery(foqNew)
  }

  override def getAnswerVars(): Set[Variable] = foq.getAnswerVars()
}








