package de.tu_dresden.epistemic_rewriter.datatypes

import de.tu_dresden.epistemic_rewriter._
import de.tu_dresden.epistemic_rewriter.datatypes.Polarity.Polarity
import de.tu_dresden.epistemic_rewriter.datatypes.ResultListType.ResultList
import org.phenoscape.scowl._
import org.semanticweb.owlapi.model._
import uk.ac.manchester.cs.owl.owlapi.{OWLClassImpl, OWLObjectPropertyImpl}

import scala.collection.immutable.{ListMap, Map, Set}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.xml.{Node, NodeSeq}


trait Query extends PrettyPrintable {
  def getAnswerVars(): Set[Variable]
}

trait XMLPrintable {
  def toXML(): Node = toXML(NodeSeq.Empty)

  def toXML(subxml: NodeSeq): Node
}

trait PrettyPrintable {
  def pShow(): String

  def pPrint() = println(pShow())
}


sealed abstract class Term(name: String) extends PrettyPrintable {

  def toVarList(): List[Variable]
}

case class Variable(name: String) extends Term(name) {
  override def toVarList(): List[Variable] = List(this)

  override def pShow(): String = name
}

case class Constant(name: String) extends Term(name) {
  override def toVarList(): List[Variable] = List.empty

  override def pShow(): String = IRI.create(name).getShortForm
}

sealed abstract class Atom extends PrettyPrintable {
  def getVars: Traversable[Variable]

  def getTerms: Traversable[Term]

  def map(f: Term => Term): Atom
}

case class ClassAtom(cls: OWLClass, t: Term) extends Atom {
  def this(s: String, t: Term) = {
    this(Class(s), t)
  }

  override def pShow: String = cls.getIRI.getShortForm + "(" + t.pShow + ")"

  override def getVars: List[Variable] = t.toVarList()

  override def getTerms: Traversable[Term] = List(t)

  override def map(f: Term => Term): Atom = this.copy(t = f(t))

}

case class RoleAtom(role: OWLObjectProperty, t1: Term, t2: Term) extends Atom {
  def this(s: String, t1: Term, t2: Term) = {
    this(ObjectProperty(s), t1, t2)
  }

  override def pShow(): String = role.getIRI.getShortForm + "(" + t1.pShow + "," + t2.pShow + ")"

  override def getVars: List[Variable] = t1.toVarList().++(t2.toVarList())

  override def getTerms: Traversable[Term] = List(t1, t2)

  override def map(f: Term => Term): Atom = this.copy(t1 = f(t1), t2 = f(t2))
}


/** **** FO QUERY *******/
object Polarity extends Enumeration {
  type Polarity = Value
  val Pos, Neg = Value
}

sealed abstract class FOQuery extends Query with XMLPrintable {

  def getQuantifiedVars(): Set[Variable]

  def map(f: FOLiteral => FOLiteral): FOQuery

  def traverse[B](f: FOLiteral => B): Traversable[B] = traverseWhile(f, _ => true)

  def traverseWhile[B](f: FOLiteral => B, cond: FOQuery => Boolean): Traversable[B]

  def toQueryPlusFO: QueryPlusFO = QueryPlusFO(this, List.empty)

  def simplify(): FOQuery = this match {
    case FOConj(fs) => {
      val fs1 = fs
        .map { case x: FOQuery => x.simplify() }
        .filter {
          case FOTrue() => false
          case _ => true
        }
        .distinct

      fs1.size match {
        case 0 => FOTrue()
        case 1 => fs1.head
        case _ => FOConj(fs1)
      }
    }
    case FODisj(fs) => {
      val fs1 = fs
        .map { case x: FOQuery => x.simplify() }
        .filter {
          case FOFalse() => false
          case _ => true
        }
        .distinct

      fs1.size match {
        case 0 => FOFalse()
        case 1 => fs1.head
        case _ => FODisj(fs1)
      }
    }

    case FOEx(Nil, f) => f.simplify()
    case FOEx(ts1, FOEx(ts2, f)) => FOEx(ts1.++(ts2), f).simplify()
    case FOEx(ts, f) => FOEx(ts, f.simplify)
    case FOAll(Nil, f) => f.simplify
    case FOAll(t, f) => FOAll(t, f.simplify)
    case FOImpl(_, FOTrue()) => FOTrue()
    case FOImpl(f1, f2) => {
      val f1s = f1.simplify()
      val f2s = f2.simplify()
      if (f1s == f2s) FOTrue()
      else FOImpl(f1s, f2s)
    }
    case default => default
  }


  def applyToAtoms(f: Atom => Atom): FOQuery = {
    def f1(l: FOLiteral): FOLiteral = l match {
      case PosLiteral(atom) => PosLiteral(f(atom))
      case NegLiteral(atom) => NegLiteral(f(atom))
    }

    map(f1)
  }

  def getLiterals(): Traversable[FOLiteral] = traverse(identity(_))

  def getLiteralsContaining(term: Term): Traversable[FOLiteral] = getLiterals().filter(_.atom.getTerms.toSet.contains(term))

  def rename(vars: List[Term], new_t: Term): FOQuery = {
    def f(t: Term): Term = if (vars.contains(t)) new_t else t

    map(_.map(f))
  }

  def getTerms(): Set[Term] = traverse(_.atom.getTerms).flatten.toSet

  def getVars(): Set[Variable] = getTerms().filter(_.isInstanceOf[Variable]).map(_.asInstanceOf[Variable])

  def getRootTerms(): Set[Term] = getTerms().filter(getPredecessors(_).isEmpty)

  def getLeafTerms(): Set[Term] = getTerms().filter(getSuccessors(_).isEmpty)

  def getNonAnswerLeafVariables(): Set[Variable] = {
    val ansVars = getAnswerVars()
    getLeafTerms().filter(_.isInstanceOf[Variable]).map(_.asInstanceOf[Variable]).filter(!ansVars.contains(_))
  }

  def getConstants(): Set[Constant] = getTerms().filter(_.isInstanceOf[Constant]).map(_.asInstanceOf[Constant])

  def getClassAtoms(term: Term): Set[(ClassAtom, Polarity)] = {
    getLiteralsContaining(term).map(_.toAtom()).filter(_._1.isInstanceOf[ClassAtom]).map(x => (x._1.asInstanceOf[ClassAtom], x._2)).toSet
  }

  def getClassAtoms(term: Term, polarity: Polarity): Set[ClassAtom] = collectByPolarity(polarity)(getClassAtoms(term))

  def getRoleAtoms(): Set[(RoleAtom, Polarity)] = {
    getLiterals()
      .map(_.toAtom())
      .filter(_._1.isInstanceOf[RoleAtom])
      .map(x => (x._1.asInstanceOf[RoleAtom], x._2))
      .toSet
  }

  def getRoleAtoms(p: Polarity): Set[RoleAtom] = collectByPolarity(p)(getRoleAtoms())

  def getOutgoingRoleAtoms(term: Term): Set[(RoleAtom, Polarity)] = getRoleAtoms().filter(_._1.t1 == term)

  def getOutgoingRoleAtoms(term: Term, p: Polarity): Set[RoleAtom] = collectByPolarity(p)(getOutgoingRoleAtoms(term))

  def getIncomingRoleAtoms(term: Term): Set[(RoleAtom, Polarity)] = getRoleAtoms().filter(_._1.t2 == term)

  def getIncomingRoleAtoms(term: Term, p: Polarity): Set[RoleAtom] = collectByPolarity(p)(getIncomingRoleAtoms(term))

  def getPredecessors(term: Term): Set[Term] = getIncomingRoleAtoms(term).map(_._1.t1)

  def getSuccessors(term: Term): Set[Term] = getOutgoingRoleAtoms(term).map(_._1.t2)

  def collectByPolarity[B](p: Polarity)(l: Set[(B, Polarity)]): Set[B] = {
    l.filter(_._2 == p).map(_._1)
  }

  def dropWhere(f: FOLiteral => Boolean): FOQuery

  override def toXML(subxml: NodeSeq): Node = <query content={pShow}>
    {subxml}
  </query>


}

abstract class FOLiteral extends FOQuery {
  def atom: Atom

  override def getQuantifiedVars(): Set[Variable] = atom.getVars.toSet

  override def getAnswerVars(): Set[Variable] = atom.getVars.toSet

  override def map(f: FOLiteral => FOLiteral): FOQuery = f(this)

  def traverseWhile[B](f: FOLiteral => B, cond: FOQuery => Boolean): Traversable[B] = {
    if (cond(this)) List(f(this)) else List.empty
  }

  def toAtom(): (Atom, Polarity)

  def map(f: Term => Term): FOLiteral


}

case class PosLiteral(atom: Atom) extends FOLiteral {
  override def pShow: String = atom.pShow()

  override def toAtom(): (Atom, Polarity) = (atom, Polarity.Pos)

  override def dropWhere(f: FOLiteral => Boolean): FOQuery = if (f(this)) FOTrue() else this

  override def map(f: Term => Term): FOLiteral = this.copy(atom = this.atom.map(f))
}

case class NegLiteral(atom: Atom) extends FOLiteral {
  override def pShow: String = "¬" + atom.pShow()

  override def toAtom(): (Atom, Polarity) = (atom, Polarity.Neg)

  override def dropWhere(f: FOLiteral => Boolean): FOQuery = if (f(this)) FOTrue() else this

  override def map(f: Term => Term): FOLiteral = this.copy(atom = this.atom.map(f))
}

case class FOTrue() extends FOQuery {
  override def pShow: String = "TRUE"

  override def getQuantifiedVars(): Set[Variable] = Set()

  override def getAnswerVars(): Set[Variable] = Set()

  override def map(f: FOLiteral => FOLiteral): FOQuery = this

  def traverseWhile[B](f: FOLiteral => B, cond: FOQuery => Boolean): Traversable[B] = List.empty

  override def dropWhere(f: FOLiteral => Boolean): FOQuery = this
}

case class FOFalse() extends FOQuery {
  override def pShow: String = "FALSE"

  override def getQuantifiedVars(): Set[Variable] = Set()

  override def getAnswerVars(): Set[Variable] = Set()

  override def map(f: FOLiteral => FOLiteral): FOQuery = this

  def traverseWhile[B](f: FOLiteral => B, cond: FOQuery => Boolean): Traversable[B] = List.empty

  override def dropWhere(f: FOLiteral => Boolean): FOQuery = this
}


case class FOConj(fs: List[FOQuery]) extends FOQuery {
  override def map(f: FOLiteral => FOLiteral): FOQuery = FOConj(fs.map(_.map(f)))

  override def canEqual(that: Any): Boolean = that.isInstanceOf[FOConj]

  override def equals(that: Any): Boolean = {
    if (canEqual(that)) {
      val c1 = that.asInstanceOf[FOConj]
      fs.toSet == c1.fs.toSet
    } else {
      return false
    }
  }

  def traverseWhile[B](f: FOLiteral => B, cond: FOQuery => Boolean): Traversable[B] = {
    if (cond(this)) fs.map(_.traverse(f)).flatten
    else List.empty
  }

  override def pShow: String = fs.size match {
    case 0 => ""
    case 1 => fs.map(_.pShow).mkString(" ∧ ")
    case _ => fs.map(_.pShow).mkString("(", " ∧ ", ")")
  }

  override def getAnswerVars(): Set[Variable] = fs.map(_.getAnswerVars()).flatten.toSet

  override def getQuantifiedVars(): Set[Variable] = fs.map(_.getAnswerVars()).flatten.toSet

  override def dropWhere(f: FOLiteral => Boolean): FOQuery = FOConj(fs.map(_.dropWhere(f)))
}

case class FODisj(fs: List[FOQuery]) extends FOQuery {
  override def map(f: FOLiteral => FOLiteral): FOQuery = FODisj(fs.map(_.map(f)))

  override def canEqual(that: Any): Boolean = that.isInstanceOf[FOConj]

  override def equals(that: Any): Boolean = {
    if (canEqual(that)) {
      val c1 = that.asInstanceOf[FOConj]
      fs.toSet == c1.fs.toSet
    } else {
      return false
    }
  }

  def traverseWhile[B](f: FOLiteral => B, cond: FOQuery => Boolean): Traversable[B] = {
    if (cond(this)) fs.map(_.traverse(f)).flatten
    else List.empty
  }

  override def pShow: String = fs.size match {
    case 0 => ""
    case 1 => fs.map(_.pShow).mkString(" ∨ ")
    case _ => fs.map(_.pShow).mkString("(", " ∨ ", ")")
  }

  override def getAnswerVars(): Set[Variable] = fs.map(_.getAnswerVars()).flatten.toSet

  override def getQuantifiedVars(): Set[Variable] = fs.map(_.getAnswerVars()).flatten.toSet

  override def dropWhere(f: FOLiteral => Boolean): FOQuery = FOConj(fs.map(_.dropWhere(f)))
}

case class FOEx(ts: List[Variable], q: FOQuery) extends FOQuery {

  def this(t: Variable, q: FOQuery) = {
    this(List(t), q)
  }

  override def map(f: FOLiteral => FOLiteral): FOQuery = FOEx(ts, q.map(f))

  def traverseWhile[B](f: FOLiteral => B, cond: FOQuery => Boolean): Traversable[B] = {
    if (cond(this)) q.traverse(f)
    else List.empty
  }

  override def pShow: String = {
    ts.size match {
      case 0 => q.pShow()
      //case 1 => "∃" + ts.map(_.pShow).mkString(",") + "." + q.pShow
      case _ => "∃(" + ts.map(_.pShow).mkString(",") + ")." + q.pShow
    }
  }

  override def getAnswerVars(): Set[Variable] = q.getAnswerVars().filterNot(ts.contains(_))

  override def getQuantifiedVars(): Set[Variable] = q.getQuantifiedVars().filter(ts.contains(_))

  override def dropWhere(f: FOLiteral => Boolean): FOQuery = this.copy(q = q.dropWhere(f))
}

case class FOAll(ts: List[Variable], q: FOQuery) extends FOQuery {


  def this(t: Variable, f: FOQuery) = {
    this(List(t), f)
  }

  override def map(f: FOLiteral => FOLiteral): FOQuery = FOAll(ts, q.map(f))

  def traverseWhile[B](f: FOLiteral => B, cond: FOQuery => Boolean): Traversable[B] = {
    if (cond(this)) q.traverse(f)
    else List.empty
  }

  override def pShow: String = ts.size match {
    case 0 => q.pShow()
    case 1 => "∀" + ts.map(_.pShow).mkString(",") + "." + q.pShow
    case _ => "∀(" + ts.map(_.pShow).mkString(",") + ")." + q.pShow
  }

  override def getAnswerVars(): Set[Variable] = q.getAnswerVars().filterNot(ts.contains(_))

  override def getQuantifiedVars(): Set[Variable] = q.getQuantifiedVars().filter(ts.contains(_))

  override def dropWhere(f: FOLiteral => Boolean): FOQuery = this.copy(q = q.dropWhere(f))
}

case class FONeg(q: FOQuery) extends FOQuery {
  override def pShow: String = "¬(" + q.pShow + ")"

  override def map(f: FOLiteral => FOLiteral): FOQuery = FONeg(q.map(f))

  def traverseWhile[B](f: FOLiteral => B, cond: FOQuery => Boolean): Traversable[B] = {
    if (cond(this)) q.traverse(f)
    else List.empty
  }

  override def getAnswerVars(): Set[Variable] = q.getAnswerVars()

  override def getQuantifiedVars(): Set[Variable] = q.getQuantifiedVars()

  override def dropWhere(f: FOLiteral => Boolean): FOQuery = this.copy(q = q.dropWhere(f))
}

case class FOImpl(f1: FOQuery, f2: FOQuery) extends FOQuery {
  override def pShow: String = "[" + f1.pShow + " → " + f2.pShow + "]"

  override def map(f: FOLiteral => FOLiteral): FOQuery = FOImpl(f1.map(f), f2.map(f))

  def traverseWhile[B](f: FOLiteral => B, cond: FOQuery => Boolean): Traversable[B] = {
    if (cond(this)) f1.traverse(f).++(f2.traverse(f))
    else List.empty
  }

  override def getAnswerVars(): Set[Variable] = f1.getAnswerVars().++(f2.getAnswerVars())

  override def getQuantifiedVars(): Set[Variable] = f1.getQuantifiedVars().++(f2.getQuantifiedVars())

  override def dropWhere(f: FOLiteral => Boolean): FOQuery = FOImpl(f1.dropWhere(f), f2.dropWhere(f))
}


/** ******************* EPISTEMIC CONJUNCTIVE QUERY *************************/


case class QueryPlusFO(ecq: FOQuery, foFilters: List[FOQuery]) extends Query {
  var labelsMap: Map[String, String] = Map()

  def simplify: QueryPlusFO = {
    val newF = foFilters.map(_.simplify()).filterNot(_.isInstanceOf[FOTrue])
    this.copy(ecq = ecq.simplify(), foFilters = newF)
  }

  def applyToAtoms(f: Atom => Atom): QueryPlusFO = {
    QueryPlusFO(this.ecq.applyToAtoms(f), this.foFilters.map(_.applyToAtoms(f)))
  }

  def toFirstOrderQuery(): FOQuery = {
    ecq match {
      case FOEx(ts, f1) => FOEx(ts, FOConj(foFilters.:+(f1).reverse))
      case default => FOConj(foFilters.:+(ecq).reverse)
    }

  }

  override def getAnswerVars(): Set[Variable] = ecq.getAnswerVars()

  override def pShow: String = {
    toFirstOrderQuery().simplify().pShow()
  }
}

object ComplexQuery {
  def fromXML(n: Node)(implicit manager: DatabaseManager): ComplexQuery = {

    def f(node: Node)(implicit manager: DatabaseManager): ComplexQueryExpression = {
      node \@ "type" match {
        case "AND" => NOutOf((node \ "part").map(f(_)), (node \ "part").size)
        case "OR" => NOutOf((node \ "part").map(f(_)), 1)
        case "GEN" => NOutOf((node \ "part").map(f(_)), (node \@ "n").toInt)
        case "NCQ" => NCQ(node \@ "label", FOQueryParser.parse(node \@ "content", manager).ecq)
        case "TNCQ" => {
          if ((node \@ "timepoint").isEmpty) {
            throw new IllegalArgumentException("Each TNCQ needs to be provided with a timepoint. Please fix this.")
          }
          TNCQ(node \@ "label", TemporalFOQueryParser.parse(node \@ "content", manager), TimePointParam.parse(node \@ "timepoint"))
        }
        case "REF" => Ref(node \@ "label")
        case "DIFF" => {
          val children = node \ "part"
          Diff(f(children.head), children.tail.map(f(_)))
        }
        case _ => throw new Error(node \@ "type")
      }
    }

    ComplexQuery(n \@ "label", f((n \ "part").head))
  }
}

object ResultListType {
  type ResultList = ListMap[String, (ComplexQuery, ExtendedAnswers)]
}
// test
case class ComplexQuery(label: String, q: ComplexQueryExpression) extends Query {


  override def getAnswerVars(): Set[Variable] = q.getAnswerVars()

  override def pShow(): String = "\"" + label + "\": " + q.pShow()

  def computeAnswersMapping(withRewriting: Boolean)(implicit helper: OntologyHelper, model: Modellike, results: ResultList): Future[(Set[Answer], ExtendedAnswers)] =
    q.computeAnswers(withRewriting).map { extAns => (extAns.answers, extAns) }


  def toXML(subxml: NodeSeq, f: ComplexQueryExpression => NodeSeq): Node =
    <query label={label}>
      {subxml}{q.toXML(f)}
    </query>
}

case class ExtendedAnswers(answers: Set[Answer], aMap: Map[ComplexQueryExpression, Set[Answer]])

//case class ExtendedAnswersWithJustifications(answers: Set[Answer], aMap: Map[ComplexQueryExpression, Set[(Answer, Set[OWLConcept])]])

trait ComplexQueryExpression extends Query {
  def computeAnswers(withRewriting: Boolean)(implicit helper: OntologyHelper, model: Modellike, results: ResultList): Future[ExtendedAnswers]

  override def pShow(): String = pShow(0)

  def pShow(indent: Int): String

  def toXML(f: ComplexQueryExpression => NodeSeq): Node

  def isTemporal():Boolean
}

case class Ref(label: String) extends ComplexQueryExpression {
  override def computeAnswers(withRewriting: Boolean)(implicit helper: OntologyHelper, model: Modellike, results: ResultList): Future[ExtendedAnswers] = {
    results.get(label) match {
      case Some((q, a)) => Future {
        ExtendedAnswers(a.answers, Map((this, a.answers)))
      }
      case None => throw new Error("For Ref are only forward definitions allowed.")
    }
  }

  override def pShow(indent: Int): String = " " * indent + "\"" + label + "\""

  override def getAnswerVars(): Set[Variable] = throw new NotImplementedError("Answer Vars for Ref not available.")

  override def toXML(f: ComplexQueryExpression => NodeSeq): Node = <ref label={label}>
    {f(this)}
  </ref>

  override def isTemporal(): Boolean = false
}

case class NCQ(label: String, ncq: FOQuery) extends ComplexQueryExpression {
  // Compute the answers using a rewriting approach.
  override def computeAnswers(withRewriting: Boolean)(implicit helper: OntologyHelper, model: Modellike, results: ResultList): Future[ExtendedAnswers] = {
    val rewritings: Future[List[QueryPlusFO]] = Future {
      if (withRewriting) EpistemicConjunctiveQueryRewriter.transform(ncq.toQueryPlusFO, helper).map(_.simplify)
      else List(ncq.toQueryPlusFO)
    }
    val a = rewritings.map(model.getAnswers(_))
    a.map(x => ExtendedAnswers(x, Map((this, x))))
  }

  override def getAnswerVars(): Set[Variable] = ncq.getAnswerVars()

  override def toXML(f: ComplexQueryExpression => NodeSeq): Node = <part type="NCQ" label={label} content={ncq.pShow}>
    {f(this)}
  </part>

  override def pShow(indent: Int): String = " " * indent + ncq.pShow()
  override def isTemporal(): Boolean = false
}

object TimePointParam {
  def parse(s: String):TimePointParam = s match  {
    case "latest" => LATEST_TP()
    case "earliest" => EARLIEST_TP()
    case _ => CONCRETE_TP(TimePoint.parse(s))
  }
}

abstract class TimePointParam() {
  def getPoint(model: TemporalDatabaseModel):TimePoint

}
case class LATEST_TP() extends TimePointParam {
  override def getPoint(model: TemporalDatabaseModel): TimePoint = {
    model.getRealTimePoints().toList.sorted.last
  }
}
case class EARLIEST_TP() extends TimePointParam {
  override def getPoint(model: TemporalDatabaseModel): TimePoint = {
    model.getRealTimePoints().toList.sorted.head
  }
}

case class CONCRETE_TP(tp: TimePoint) extends TimePointParam {
  override def getPoint(model: TemporalDatabaseModel): TimePoint = tp
}

case class TNCQ(label: String, q: TemporalFOQuery, tp_param: TimePointParam) extends ComplexQueryExpression {
  // Compute the answers using a rewriting approach.
  override def computeAnswers(withRewriting: Boolean)(implicit helper: OntologyHelper, model: Modellike, results: ResultList): Future[ExtendedAnswers] = {
    if (model.isInstanceOf[TemporalDatabaseModel]) {
      val m = model.asInstanceOf[TemporalDatabaseModel]
      val a = Future { m.getAnswers(q,tp_param.getPoint(m)) }
      a.map( x => ExtendedAnswers(x, Map((this, x))))
    }
    else throw new Exception("TNCQs can only be answered over temporal models.")

  }

  override def getAnswerVars(): Set[Variable] = q.getAnswerVars()

  override def toXML(f: ComplexQueryExpression => NodeSeq): Node = <part type="TNCQ" label={label} content={q.pShow}>
    {f(this)}
  </part>

  override def pShow(indent: Int): String = " " * indent + q.pShow()
  override def isTemporal(): Boolean = true
}

case class NOutOf(subqueries: Seq[ComplexQueryExpression], n: Int) extends ComplexQueryExpression {
  override def computeAnswers(withRewriting: Boolean)(implicit helper: OntologyHelper, model: Modellike, results: ResultList): Future[ExtendedAnswers] = {
    val extAnswers = Future.sequence(subqueries.map(_.computeAnswers(withRewriting)))

    extAnswers.map { x =>
      val a = x.map(_.answers).flatten.groupBy(identity(_)).mapValues(_.size).filter { case (a, c) => c >= n }.keys.toSet
      val newAMap = x.map(_.aMap).flatten.toMap.+((this, a))
      ExtendedAnswers(a, newAMap)
    }

  }

  override def getAnswerVars(): Set[Variable] = subqueries.head.getAnswerVars()

  def getType = {
    val m = subqueries.size
    if (n == 1) "OR"
    else if (n == m) "AND"
    else "GEN"
  }

  override def toXML(f: ComplexQueryExpression => NodeSeq): Node = <part type={getType} n={n.toString}>
    {f(this)}{NodeSeq fromSeq subqueries.map(_.toXML(f))}
  </part>

  override def pShow(indent: Int): String = {
    " " * indent + getType + ":\n" + subqueries.map(_.pShow(indent + 2)).mkString(",\n")
  }

  override def isTemporal(): Boolean = {
    val ts = subqueries.map(_.isTemporal())
    if (ts.forall(_ == true)) true
    else if (ts.forall(_ == false)) false
    else throw new Error("A query can not be temporal and a temporal at the same time..")
  }
}

case class Diff(subtrahend: ComplexQueryExpression, minuends: Seq[ComplexQueryExpression]) extends ComplexQueryExpression {
  def allSeq = Seq(subtrahend).++(minuends)

  override def computeAnswers(withRewriting: Boolean)(implicit helper: OntologyHelper, model: Modellike, results: ResultList): Future[ExtendedAnswers] = {
    val extAns = Future.sequence(allSeq.map(_.computeAnswers(withRewriting)))
    extAns.map { x =>
      val a = x.map(_.answers).reduceLeft(_.diff(_))
      val newAMap = x.map(_.aMap).flatten.toMap.+((this, a))
      ExtendedAnswers(a, newAMap)
    }
  }

  override def getAnswerVars(): Set[Variable] = subtrahend.getAnswerVars()

  override def toXML(f: ComplexQueryExpression => NodeSeq): Node =
    <part type="DIFF">
      {f(this)}{NodeSeq fromSeq allSeq.map(_.toXML(f))}
    </part>

  override def pShow(indent: Int): String = {
    " " * indent + "DIFF" + ":\n" + allSeq.map(_.pShow(indent + 2)).mkString(",\n")
  }

  override def isTemporal(): Boolean = {
    val qs = minuends.:+(subtrahend)
    val ts = qs.map(_.isTemporal())
    if (ts.forall(_ == true)) true
    else if (ts.forall(_ == false)) false
    else throw new Error("A query can not be temporal and a temporal at the same time..")
  }
}


/** ******************* HELPER CLASSES *************************/

case class OWLTopClass(iri: IRI = IRI.create("http://www.w3.org/2002/07/owl#Thing")) extends OWLClassImpl(iri) {
  override def isOWLThing: Boolean = true
}

case class OWLBottomClass(iri: IRI = IRI.create("")) extends OWLClassImpl(iri) {
  override def isOWLNothing: Boolean = true
}

case class OWLTopObjectProperty(iri: IRI = IRI.create("")) extends OWLObjectPropertyImpl(iri) {
  override def isOWLTopObjectProperty: Boolean = true
}

case class OWLBottomObjectProperty(iri: IRI = IRI.create("")) extends OWLObjectPropertyImpl(iri) {
  override def isOWLBottomObjectProperty: Boolean = true
}


case class Answer(answer: Map[Term, OWLIndividual]) extends PrettyPrintable with XMLPrintable {
  def +(term: Term, ind: OWLIndividual): Answer = {
    copy(answer = this.answer.+((term, ind)))
  }

  def named(): Map[Term, OWLNamedIndividual] = {
    answer.filter(_._2.isNamed).mapValues(_.asOWLNamedIndividual())
  }

  def +(term: Term, ind: IRI): Answer = {
    this.+(term, NamedIndividual(ind))
  }

  private def printInd(ind: OWLIndividual): String = {
    if (ind.isAnonymous) {
      "anonymous"
    }
    else {
      ind.asOWLNamedIndividual().getIRI.toString
    }
  }

  override def toString: String = pShow

  override def pShow: String = {

    if (answer.isEmpty) {
      "true"
    }
    else answer.map { case (term, ind) => term.pShow + " ↦ " + printInd(ind) }.mkString("\n")
  }


  override def toXML(subxml: NodeSeq): Node = {
    val nodes: NodeSeq = NodeSeq fromSeq answer.map { case (term, ind) => <var name={term.pShow()} ind={printInd(ind)}/> }.toSeq
    <answer>
      {nodes}
    </answer>
  }
}

case class AnswerWithJustifications(answer: Map[Term, (OWLNamedIndividual, Set[OWLClassAssertionAxiom])]) extends PrettyPrintable {
  override def pShow: String = {
    if (answer.isEmpty) {
      ""
    }
    else answer.map { case (term, (ind, justifications)) => term.pShow + " ↦ " + ind.getIRI.getShortForm + justifications.map(_.getClassExpression.asOWLClass().getIRI.getShortForm).mkString(" J: (", ",", ") ") }.mkString("\n")
  }
}

case class AnswersPack(query: EpistemicConjunctiveQuery, answers: Map[QueryPlusFO, Set[Answer]]) extends XMLPrintable {
  override def toXML(subxml: NodeSeq): Node = {
    //val q = query.asXML()
    val rewritingsXML: Seq[Node] = (answers.map { case (q, awrs) => {
      val aXML = <answers>
        {NodeSeq fromSeq awrs.map(_.toXML()).toSeq}
      </answers>
      q.toFirstOrderQuery().toXML(aXML)
    }
    }).toSeq

    val rewritingsNode = <rewritings>
      {NodeSeq fromSeq rewritingsXML}
    </rewritings>
    val answersNode = <answers>
      {NodeSeq fromSeq answers.values.flatten.map(_.toXML()).toSeq.distinct}
    </answers>
    val sxml = NodeSeq.fromSeq(Seq(answersNode, rewritingsNode))
    query.foq.toXML(sxml)
  }
}
