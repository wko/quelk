package de.tu_dresden.epistemic_rewriter.datatypes

import de.tu_dresden.epistemic_rewriter._
import de.tu_dresden.epistemic_rewriter.datatypes.Polarity.Polarity
import de.tu_dresden.epistemic_rewriter.datatypes.SQLStatement._
import org.semanticweb.owlapi.model.IRI

import scala.collection.immutable.{ListSet, Map}

/**
  * Represents something that can be directly mapped to a SELECT SQL QUERY
  */
trait View {
  def defaultName: String = iri2sql(hashCode().abs.toString)
  def toSQL(): String

  def toTemporalSQL(): String
}

/**
  * Represents a view of an existing relation of the schema.
  * @param vars
  * @param relation
  * @param conditions
  */
case class StructuredView(vars: List[String], relation: String, conditions: List[String]) extends View {
  override def toSQL() =
    s"""
       |  SELECT ${vars.mkString(",")} FROM $relation
       |  ${if (conditions.nonEmpty) s"WHERE ${conditions.mkString("\nAND ")}" else ""}
     """.stripMargin

  //${ConfigValues.INT_BEGIN}, ${INT_END}
  override def toTemporalSQL() = {
    s"""
       |  SELECT ${vars.mkString(",")}, $relation.timepoint_id AS timepoint_id
       |  FROM $relation
       |  ${if (conditions.nonEmpty) s"WHERE ${conditions.mkString(" AND ")}" else ""}
     """.stripMargin
  }
}

case class TemporalView() extends View {
  override def toSQL(): String = ???

  override def toTemporalSQL(): String = ???
}

case class PlainView(definition: String) extends View {
  override def toSQL() = definition
  override def toTemporalSQL() = definition
}

case class EventuallyView(vars: ListSet[String], relation: TemporalRelation, interval: RelInterval) extends View {
  override def toSQL(): String = ???
  override def toTemporalSQL(): String = {
    val i_begin = interval.end match {
      case FinitePeriod(end) =>  s"dt_from - INTERVAL '${end.toStandardMinutes.getMinutes} minutes'"
      case PosInfinitePeriod() => "'-infinity'::timestamp"
      case NegInfinitePeriod() => "'infinity'::timestamp"
    }
    val i_end = interval.begin match {
      case FinitePeriod(begin) => s"dt_to - INTERVAL '${begin.toStandardMinutes.getMinutes} minutes'"
      case PosInfinitePeriod() => "'-infinity'::timestamp"
      case NegInfinitePeriod() => "'infinity'::timestamp"
    }

    s"""
       |  SELECT ${vars.mkString(",")}, $i_begin AS dt_from, $i_end AS dt_to
       |  FROM ${relation.name}
     """.stripMargin
  }
}

case class AlwaysView(vars: ListSet[String], relation: TemporalRelation, interval: RelInterval) extends View {
  override def toSQL(): String = ???
  override def toTemporalSQL(): String = {
    val i_begin = interval.begin match {
      case FinitePeriod(begin) =>  s"dt_from - INTERVAL '${begin.toStandardMinutes.getMinutes} minutes'"
      case PosInfinitePeriod() => "'infinity'::timestamp"
      case NegInfinitePeriod() => "'-infinity'::timestamp"
    }
    val i_end = interval.end match {
      case FinitePeriod(end) => s"dt_to - INTERVAL '${end.toStandardMinutes.getMinutes} minutes'"
      case PosInfinitePeriod() => "'infinity'::timestamp"
      case NegInfinitePeriod() => "'-infinity'::timestamp"
    }

    s"""
       |  SELECT ${vars.mkString(",")}, $i_begin AS dt_from, $i_end AS dt_to
       |  FROM ${relation.name}
       |  WHERE $i_begin <= $i_end
     """.stripMargin
  }
}

trait Constraint {

  def toTemporalSQL(): String
}


trait NonTemporalConstraint extends Constraint {
  def toSQL(): String
}

case class PosConstraint(vars: List[Var], content: String, tVar: TVar = TVar("t")) extends NonTemporalConstraint{
  override def toSQL(): String = {
    vars.map(_.toField()).mkString("ROW(", ",", ")") + s" $connector ( SELECT * FROM $content )"
  }

  override def toTemporalSQL(): String = {
    val vars1 = vars.:+(tVar)
    vars1.map(_.toField()).mkString("ROW(", ",", ")") + s" $connector ( SELECT * FROM $content )"
  }

  def connector: String = "IN"
}

case class NegConstraint(vars: List[Var], content: String, tVar: TVar = TVar("t")) extends NonTemporalConstraint {
  override def toSQL(): String = {
    vars.map(_.toField()).mkString("ROW(", ",", ")") + s" $connector ( SELECT * FROM $content )"
  }

  override def toTemporalSQL(): String = {
    val vars1 = vars.:+(tVar)
    vars1.map(_.toField()).mkString("ROW(", ",", ")") + s" $connector ( SELECT * FROM $content )"
  }

  def connector: String = "NOT IN"
}



trait TemporalConstraint extends Constraint {

}

case class PlainConstraint(definition: String) extends NonTemporalConstraint with TemporalConstraint {
  override def toTemporalSQL(): String = definition
  override def toSQL(): String = ???
}

case class PlainTemporalConstraint(content: PartialFunction[TVar, String], tVar: TVar = TVar("t")) extends TemporalConstraint {
  override def toTemporalSQL(): String = content(tVar)
}


object ConstraintCreator {
  def createTemporalIntersections(relations: ListSet[TemporalRelation]): ListSet[TemporalConstraint] = {
    // Assuming guarded negation find a positive relation:
    val r = relations.find(_.polarity == Polarity.Pos).head
    val tail:ListSet[TemporalRelation] = relations.-(r)
    tail.map{ s => s.polarity match {
      case Polarity.Pos => TemporalIntersection(r,s)
      case Polarity.Neg => TemporalDisjointness(r,s)
    }}
  }
  def createTemporalIntersections(relations: ListSet[TemporalRelation], interval: AbsInterval): ListSet[TemporalConstraint] = {
    // Assuming guarded negation find a positive relation:
    val r = relations.find(_.polarity == Polarity.Pos).head
    val tpC = PlainConstraint(s"NOT (${r}.dt_from > '${interval.end}' OR '${interval.begin}' > ${r}.dt_to)")
    createTemporalIntersections(relations).+(tpC)
  }

  def createObjectIntersections(relations: ListSet[TemporalRelation]): ListSet[TemporalConstraint] = {
    val r = relations.head
    relations.tail.map(ObjectIntersection(r, _))
  }
}
/**
  * The intervals of all the given relations have to intersect.
  * @param relations
  */
case class TemporalIntersection(r1: TemporalRelation, r2: TemporalRelation) extends  TemporalConstraint {
  override def toTemporalSQL(): String = s"NOT (${r1}.dt_from > ${r2}.dt_to OR ${r2}.dt_from > ${r1}.dt_to)"
}

case class TemporalDisjointness(r1: TemporalRelation, r2: TemporalRelation) extends  TemporalConstraint {
  override def toTemporalSQL(): String = s"(${r1}.dt_from > ${r2}.dt_to OR ${r2}.dt_from > ${r1}.dt_to)"
}


case class ObjectIntersection(r1: TemporalRelation, r2: TemporalRelation) extends  TemporalConstraint {
  override def toTemporalSQL(): String = {
      val commonVars = r1.fields.intersect(r2.fields)
      commonVars.map( v => s"$r1.${v.toString} = $r2.${v.toString}").mkString("\nAND")
  }

}




abstract class Var(name: String) {
  def toSelect(): String

  def toField(): String

  def toRelation(): String

  override def toString: String = name
}

/**
  * Represents a variable for an individual
  *
  * @param name
  */
case class IVar(name: String) extends Var(name) {
  override def toSelect(): String = s"$name.iri AS $name"

  override def toField(): String = s"$name.iri"

  override def toRelation(): String = s"individuals $name"
}

/**
  * Represents a variable for a time point
  *
  * @param name
  */
case class TVar(name: String) extends Var(name) {
  override def toSelect(): String = ???

  override def toField: String = s"$name.id"

  override def toRelation(): String = ??? // s"$TSRelation $name"
}


object SQLStatement {
  def empty: SQLStatement = new SQLStatement(Map(), ListSet(), ListSet(), ListSet())

  def iri2sql(iRI: IRI): String = "s" + iRI.toString.filterNot(":/.-_,;<>#".contains(_))

  def iri2sql(iRI: String): String = "s" + iRI

  def toVar(term: Term): Var = term match {
    case Constant(n) => IVar("x" + n.hashCode().abs)
    case Variable(n) => IVar(n)
  }



  def relevantTPs(): String = s"${ConfigValues.TSRelation}"

  def relevantTPs(tp: ExtTimePoint): String = s"${ConfigValues.TSRelevantFunction}('${tp.toString}', '${tp.toString}')"

  def relevantTPs(from: ExtTimePoint, to: ExtTimePoint): String = s"${ConfigValues.TSRelevantFunction}('${from.toString}', '${to.toString}')"

  //val TSSELECT = "SELECT timepoint FROM timepoints"
}


/**
  * Represents an SQL Statement that can then be compiled to either a temporal representation or an atemporal one.
  *
  * @param views
  * @param constraints
  * @param vars
  */
case class SQLStatement(views: Map[String, View], constraints: ListSet[NonTemporalConstraint], vars: ListSet[Var], ansVars: ListSet[Var]) extends View {

  def toSQL(): String = {
    val header = {
      if (views.isEmpty) ""
      else views.map { case (name, v) => s"$name AS (${v.toSQL()})" }.mkString("WITH RECURSIVE\n", ",\n", "\n")
    }

    val select = "SELECT DISTINCT " + ansVars.map(_.toSelect).mkString(",") + "\n" +
      "FROM " + vars.map(_.toRelation()).mkString(",")
    val filters =
      if (constraints.isEmpty) ""
      else constraints.map(_.toSQL()).mkString("\nWHERE ", "\nAND ", "")

    return header + select + filters
  }

  def toTemporalSQL(timepoint: TimePoint): String = {
    val tVar = TVar("t")

    val header = {
      if (views.isEmpty) ""
      else views.map { case (name, v) => s"$name AS (${v.toTemporalSQL()})" }.mkString("WITH RECURSIVE\n", ",\n", "\n")
    }
    val select = "SELECT DISTINCT " + ansVars.map(_.toSelect()).mkString(",") + "\n" +
      "FROM " + vars.map(_.toRelation()).mkString(",") + s",${relevantTPs(timepoint)} $tVar"
    val filters =
      if (constraints.isEmpty) s""
      else constraints.map(_.toTemporalSQL()).mkString("\nWHERE ", "\nAND ", "")


    return header + select + filters
  }

  override def toTemporalSQL(): String = {

    val header = {
      if (views.isEmpty) ""
      else views.map { case (name, v) => s"$name AS (${v.toTemporalSQL()})" }.mkString("WITH RECURSIVE\n", ",\n", "\n")
    }
    var tVar = TVar("t")
    val select =
      s"""
         |  SELECT DISTINCT ${ansVars.map(_.toSelect()).mkString(",")}, $tVar.id AS timepoint_id
         |  FROM ${vars.map(_.toRelation()).mkString(",")}, timepoints $tVar
         |  ${if (constraints.nonEmpty) constraints.map(_.toTemporalSQL()).mkString("WHERE ", "\n  AND ", "")}
       """.stripMargin


    return header + select
  }

  /*def abstractAsView(tVars: List[TVar]): SQLStatement = {
    val name: String = iri2sql(hashCode().abs.toString)
    val stmt = SQLStatement(views.+((name, removeViews())), ListSet(), ansVars, ansVars)
    stmt.copy(constraints = stmt.constraints.++(tVars.map(PosConstraint(ansVars.toList, name, _))))
  }*/

  def abstractAsView(name: String = iri2sql(hashCode().abs.toString) ): (String, SQLStatement) = {
    (name, SQLStatement(views.+((name, removeViews())), ListSet(), ansVars, ansVars))
  }

  /*def abstractAsNegView(name: String = iri2sql(hashCode().abs.toString)): (String, SQLStatement) = {
    (name, SQLStatement(views.+((name, removeViews())), ListSet(), ansVars, ansVars).addConstraint(NegConstraint(ansVars.toList, name)))
  }*/

  def addView(n: String, v: View): SQLStatement = v match {
    case v: SQLStatement => {
      // Merge the views
      val stmt = v.removeViews()
      copy(views = views.+((n, stmt)).++(v.views))

    }
    case _ => copy(views = views.+((n, v)))
  }


  def addConstraint(constraint: NonTemporalConstraint): SQLStatement = {
    copy(constraints = constraints.+(constraint))
  }

  def addVar(v: Var*): SQLStatement = {
    this.copy(vars = vars.++(v))
  }

  def addAnsVar(v: Var*): SQLStatement = {
    this.copy(ansVars = ansVars.++(v), vars = vars.++(v))
  }

  def setAnsVars(v: Var*): SQLStatement = {
    this.copy(ansVars = ListSet(v: _*), vars = vars.++(v))
  }

  def merge(stmt: SQLStatement): SQLStatement = {
    this.copy(views = views.++(stmt.views), constraints = constraints.++(stmt.constraints), vars = vars.++(stmt.vars), ansVars = ansVars.++(stmt.ansVars))
  }

  def merge(stmts: SQLStatement*): SQLStatement = {
    stmts.fold(this.copy())((a, b) => a.merge(b))
  }

  def removeViews(): SQLStatement = copy(views = Map())

  def toTemporalSQLStatement(name: String = "t_" + iri2sql(hashCode().abs.toString)): TemporalSQLStatement = {


    val s = s"""ROW(${ansVars.map(_.toField()).mkString(",")},t.id) IN ( SELECT * FROM $name )"""
    val (_, stmt) = addConstraint(PlainConstraint(s)).abstractAsView(name)
    val relation = TemporalRelation(name, ansVars, Polarity.Pos)
    // Add the constraint to extract the intervals from the non-temporal relation

    TemporalSQLStatement(stmt.views, ListSet(relation), ListSet(), ansVars, ansVars)
  }

  //def union(stmts: SQLStatement*): UnionStatement = UnionStatement(ListSet(stmts :_*))

}



object TemporalSQLStatement {
  def empty = TemporalSQLStatement(Map.empty, ListSet.empty, ListSet.empty, ListSet.empty, ListSet.empty)
}

case class TemporalRelation(name: String, fields: ListSet[Var], polarity: Polarity) {
  override def toString: String = name
  //def getId: String = s"r${name.hashCode.abs}"
}

case class TemporalSQLStatement(views: Map[String, View], relations: ListSet[TemporalRelation], constraints: ListSet[TemporalConstraint], vars: ListSet[Var], ansVars: ListSet[Var]) extends View {
  lazy val autoName = "t_" + iri2sql(hashCode().abs.toString)

  override def toSQL(): String = ???

  lazy val posRelations = relations.filter(_.polarity == Polarity.Pos)
  lazy val negRelations = relations.filter(_.polarity == Polarity.Neg)

  /**
    *
    * @param includeIntervals true if the intervals should be returned in which the answer holds.
    * @return
    *
    * GREATEST and LEAST are used to as a conjunctive combination
    */
  def computeSelectFields(includeIntervals: Boolean): String = {
    // Use positive Relations here if only allowed guarded negation
    val s = ansVars.map(x => posRelations.find(_.fields.contains(x)).map{ r => s"${r}.$x AS $x"}.head).mkString(",")
    if (includeIntervals) {
      val l = if (negRelations.nonEmpty) s""", LEAST(${negRelations.map(r => s"$r.dt_to").mkString(",")})""" else ""
      val r = if (negRelations.nonEmpty) s""", GREATEST(${negRelations.map(r => s"$r.dt_from").mkString(",")})""" else ""
      s +
        s""", GREATEST(${posRelations.map(r => s"$r.dt_from").mkString(",")} $l) AS dt_from, LEAST(${posRelations.map(r => s"$r.dt_to").mkString(",")} $r) AS dt_to"""
    }
    else s

  }


  def toTemporalSQL(tp: TimePoint): String = {
    val header = {
      if (views.isEmpty) ""
      else views.map { case (name, v) => s"$name AS (${v.toTemporalSQL()})" }.mkString("WITH RECURSIVE\n", ",\n", "")
    }
    // TODO: CAN BE SET TO NOT INCLUDE INTERVALS WHEN In PRODUCTION
    val select =
      s"""
         |SELECT DISTINCT ${computeSelectFields(true)}
         |FROM ${relations.mkString(",")}
       |""".stripMargin

    val timepointConstraints = ConstraintCreator.createTemporalIntersections(relations,tp.toInterval)
    val objectConstraints = ConstraintCreator.createObjectIntersections(relations)
    val c = constraints.++(timepointConstraints).++(objectConstraints)
    val filters =
      if (c.isEmpty) ""
      else c.map(_.toTemporalSQL()).mkString("WHERE ","\nAND ", "\n")

    return header + select + filters
  }

  override def toTemporalSQL(): String = {
    val header = {
      if (views.isEmpty) ""
      else views.map { case (name, v) => s"$name AS (${v.toTemporalSQL()})" }.mkString("WITH RECURSIVE\n", ",\n", "")
    }

    val select =
      s"""
         |SELECT DISTINCT ${computeSelectFields(true)}
         |FROM ${relations.mkString(",")}
         |""".stripMargin

    val timepointConstraints = ConstraintCreator.createTemporalIntersections(relations)
    val objectConstraints = ConstraintCreator.createObjectIntersections(relations)
    val c = constraints.++(timepointConstraints).++(objectConstraints)
    val filters =
      if (c.isEmpty) ""
      else c.map(_.toTemporalSQL()).mkString("WHERE ","\nAND ", "\n")

    return header + select + filters
  }

  def addView(n: String, v: View): TemporalSQLStatement = v match {
    case v: SQLStatement => {
      // Merge the views
      val stmt = v.removeViews()
      copy(views = views.+((n, stmt)).++(v.views))
    }
    case _ => copy(views = views.+((n, v)))
  }

  def addConstraint(constraint: TemporalConstraint): TemporalSQLStatement = {
    copy(constraints = constraints.+(constraint))
  }

  def addVar(v: Var*): TemporalSQLStatement = {
    this.copy(vars = vars.++(v))
  }

  def addAnsVar(v: Var*): TemporalSQLStatement = {
    this.copy(ansVars = ansVars.++(v), vars = vars.++(v))
  }

  def setAnsVars(v: Var*): TemporalSQLStatement = {
    this.copy(ansVars = ListSet(v: _*), vars = vars.++(v))
  }

  def merge(stmt: TemporalSQLStatement): TemporalSQLStatement = {
    this.copy(views = views.++(stmt.views), relations = relations.++(stmt.relations), vars = vars.++(stmt.vars), constraints = constraints.++(stmt.constraints), ansVars = ansVars.++(stmt.ansVars))
  }

  def merge(stmts: TemporalSQLStatement*): TemporalSQLStatement = {
    stmts.fold(this.copy())((a, b) => a.merge(b))
  }

  def removeViews(): TemporalSQLStatement = copy(views = Map())
  def removeRelations(): TemporalSQLStatement = copy(relations = ListSet())

  def abstractAsView(polarity: Polarity): (String, TemporalSQLStatement) =
    (autoName, TemporalSQLStatement(views.+((autoName, removeViews())), ListSet(TemporalRelation(autoName, ansVars, polarity)), ListSet(), ansVars, ansVars))



  /**
    * Compute the islands for positive relations
    * @return a Statement with one new relation that contains the merged intervals
    */
  def computeIslands():(String, TemporalSQLStatement) = {
    val stmt = TemporalSQLStatement(views.+((autoName, removeViews())), ListSet(TemporalRelation(autoName, ansVars, Polarity.Pos)), ListSet(), ansVars, ansVars)
    val selectFields = ansVars.mkString(",")
    val islands = PlainView(
      s"""
         |  SELECT ${selectFields}, MIN(dt_from) AS dt_from, MAX(dt_to) AS dt_to
         |  	FROM (SELECT ${selectFields}, dt_from, dt_to, SUM(CASE WHEN RunningMaxEndDate >= dt_from - INTERVAL '1 ms' THEN 0 ELSE 1 END) OVER
         |  	    (PARTITION BY ${selectFields} ORDER BY dt_from, dt_to ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS IslandNumber
         |  	     FROM (
         |           SELECT ${selectFields}, dt_from,dt_to,MAX(dt_to)
         |           	        OVER (PARTITION BY ${selectFields} ORDER BY dt_from, dt_to ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING) AS RunningMaxEndDate
         |           	         FROM $autoName
         |         ) AS i0
         |    ) AS i1
         |  	GROUP BY IslandNumber, ${selectFields}
       """.stripMargin
    )

    val islandsViewName = autoName + "_islands"
    val stmt1 = stmt.addView(islandsViewName,islands).copy(relations = ListSet(TemporalRelation(islandsViewName, stmt.ansVars, Polarity.Pos)))
    (islandsViewName, stmt1)
  }

  /**
    * Compute the gaps for negative relations
    * @return a Statement with one new relation that contains the gaps between intervals
    */
  def computeGaps():(String, TemporalSQLStatement) = {
    val (islandsViewName, stmt) = computeIslands()
    val selectFields = ansVars.mkString(",")
    val gaps = PlainView(
      s"""
         |  SELECT DISTINCT ${selectFields}, f AS dt_from, t AS dt_to
         |    FROM (
         |      SELECT * FROM (SELECT x,
         |        dt_from,
         |        dt_to,
         |        INTERVAL '1 ms' + (LAG(dt_to,1,'-infinity') OVER (PARTITION BY ${selectFields} ORDER BY dt_from, dt_to)) AS f,
         |        INTERVAL '-1 ms' + dt_from as t
         |      FROM $islandsViewName) AS g0
         |      WHERE f<dt_from UNION
         |      SELECT * FROM (SELECT ${selectFields},
         |        dt_from,
         |        dt_to,
         |        INTERVAL '1 ms' + dt_to as f,
         |        INTERVAL '-1 ms' + (LEAD(dt_from,1,'infinity') OVER (PARTITION BY ${selectFields} ORDER BY dt_from, dt_to)) AS t
         |      FROM $islandsViewName) AS g0
         |      WHERE t>dt_to
         |    ) AS g1
       """.stripMargin
    )
    val gapsName = autoName + "_gaps"
    val stmt1 = stmt.addView(gapsName, gaps).copy(relations = ListSet(TemporalRelation(gapsName, stmt.ansVars, Polarity.Pos)))
    (gapsName, stmt1)

  }

  /**
    * Compute the union of all current relations
    * @return a Statement with one new relation that contains the gaps between intervals
    */
  def union():(String, TemporalSQLStatement) = {
    if (negRelations.nonEmpty) throw new Error("Union with negative Relations is not yet supported")
    // Compute the SQL Statement recursively

    val unionView = PlainView(relations.map{ case r => s"  SELECT * FROM ${r.name}"}.mkString("\n"," UNION\n", "\n"))
    val unionViewName = "union_" + iri2sql(unionView.hashCode().abs.toString)

    val relation = TemporalRelation(unionViewName, ansVars, Polarity.Pos)
    (unionViewName, TemporalSQLStatement(views.+((unionViewName, unionView)), ListSet(relation), ListSet(), ansVars, ansVars))
  }

}
