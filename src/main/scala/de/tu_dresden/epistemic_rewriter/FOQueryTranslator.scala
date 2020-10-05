package de.tu_dresden.epistemic_rewriter

import de.tu_dresden.epistemic_rewriter.datatypes.SQLStatement.{iri2sql, toVar}
import de.tu_dresden.epistemic_rewriter.datatypes._

import scala.collection.immutable.ListSet

object FOQueryTranslator {

  def toSQLStatement(f: FOQuery): SQLStatement = {
    f match {
      case FOTrue() => SQLStatement.empty
      case FOFalse() => throw new NotImplementedError("This should not happen..")
      case l : FOLiteral => {
        val (a, pol) = l.toAtom()
        a match {
          case ClassAtom(cls, t) => {
            val v = toVar(t)
            cls.isOWLThing match {
              case true => SQLStatement.empty.addAnsVar(v)
              case false => {
                val c = cls.getIRI
                val aliasName = iri2sql(c)
                // sqlQueryC(c).result.statements.head
                val view = StructuredView(List("ind_id"), "concept_assertions", List(s"concept_id = '${c.toString}'"))
                val s = if (pol == Polarity.Pos) PosConstraint(List(v), iri2sql(c))
                else NegConstraint(List(v), iri2sql(c))
                SQLStatement.empty.addView(aliasName, view).addConstraint(s).addAnsVar(v)
              }
            }
          }
          case RoleAtom(role, t1, t2) => {
            val r = role.getIRI
            val aliasName = iri2sql(r)
            //sqlQueryR(r).result.statements.head
            val view = StructuredView(List("ind_1_id", "ind_2_id"), "role_assertions", List(s"role_id = '${r.toString}'"))
            val s = if (pol == Polarity.Pos) PosConstraint(List(toVar(t1), toVar(t2)), iri2sql(r))
            else NegConstraint(List(toVar(t1), toVar(t2)), iri2sql(r))
            SQLStatement.empty.addView(aliasName, view).addConstraint(s).addAnsVar(toVar(t1)).addAnsVar(toVar(t2))
          }
        }
      }
      case FOConj(fs) => SQLStatement.empty.merge(fs.map(toSQLStatement(_)): _*)
      case FODisj(List(FOTrue())) => SQLStatement.empty
      case FODisj(fs) => ???
      /*{
        var stmt = SQLStatement.empty
        val stmts = fs.map(_.toSQLStatement())
        stmt = stmt.copy(views = stmt.views.++(stmts.flatMap(_.views)))
        val iri = s"disj_${stmts.hashCode().abs}"
        val vars = stmts.head.vars
        val definition = stmts.map(x => x.toSQLWithoutHeader(x.vars, "iri")).mkString(" UNION ")

        val constraint = PosConstraint(vars.map(_+ ".iri"), iri)
        stmt.+(iri, PlainView(definition)).+(constraint)
      }*/
      case (FOImpl(FOEx(_, left), FOEx(z2, right))) => {
        val ansVarsForFilter = right.getAnswerVars().filterNot(z2.contains(_)).map(toVar(_)).toList
        var stmt = SQLStatement.empty

        var s1 = toSQLStatement(left)
        var s2 = toSQLStatement(right)

        s1 = s1.setAnsVars(ansVarsForFilter: _*)
        s2 = s2.setAnsVars(ansVarsForFilter: _*)

        // Add universal individuals relation (with temporal option)
        stmt = stmt.addView("sindividuals", StructuredView(List("iri"), "individuals", List.empty))
        //val ansVarsForFilter = s2.ansVars.filterNot(z2.map(_.name).contains(_)).distinct

        val iri = s"filter_on_${ansVarsForFilter.mkString("_and_")}_with_${z2.map(_.name).mkString("_and_")}"
        val namel = "l_" + iri
        val namer = "r_" + iri

        stmt = stmt.addView(namel, s1).addView(namer, s2).addAnsVar(ansVarsForFilter: _*)


        // Add main filter definition using left and right subdefinitions

        val definition = s"""(SELECT * FROM $namer) UNION (SELECT * FROM sindividuals x WHERE ROW(x.iri) NOT IN (SELECT * FROM $namel))"""

        val s = PosConstraint(ansVarsForFilter, iri)
        stmt = stmt.addView(iri, PlainView(definition)).addConstraint(s)
        return stmt
      }
      case FOEx(z, f) => {
        val s = toSQLStatement(f)
        val ansVarsForFilter = s.vars.filterNot(z.map(_.name).contains(_))
        s.addAnsVar(ansVarsForFilter.toSeq: _*)
      }
      case _ => throw new Error(s"This case should not come up with well formed FOQueries..\n Found: $f")
    }
  }

}

object TemporalFOQueryTranslator {

  def toTemporalSQLStatement(f: TemporalFOQuery)(implicit helper: OntologyHelper): TemporalSQLStatement = {
    f match {
      case TemporalAtom(q) => {
        val ansVars = ListSet(q.getAnswerVars().map(toVar(_)).toSeq :_*)
        // Rewrite the inner query
        val qR = EpistemicConjunctiveQueryRewriter.transform(q,helper)
        qR.size match {
          case 1 => {
            val f = qR.head
            val (queryName, q) = FOQueryTranslator.toSQLStatement(f.toFirstOrderQuery()).setAnsVars(f.getAnswerVars().map(toVar(_)).toSeq :_*).abstractAsView()

            val intervalSelectionView = PlainView(
              s"""
                 |  SELECT ${ansVars.map(_.toString).mkString(",")}, timepoints.dt_from AS dt_from, timepoints.dt_to AS dt_to
                 |  FROM $queryName
                 |  INNER JOIN timepoints ON timepoints.id = $queryName.timepoint_id
               """.stripMargin)

            val intervalSelectionViewName =  "t_" + queryName
            val views = q.views.+((intervalSelectionViewName, intervalSelectionView))
            // Add the constraint to extract the intervals from the non-temporal relation
            val relation = TemporalRelation(intervalSelectionViewName, ansVars, Polarity.Pos)
            TemporalSQLStatement(views, ListSet(relation), ListSet(), ansVars, ansVars)
          }
          case _ => {
            // Compute the SQL Statement recursively
            val rewQs = qR.map{ case (f) =>
              val st = FOQueryTranslator.toSQLStatement(f.toFirstOrderQuery()).setAnsVars(f.getAnswerVars().map(toVar(_)).toSeq :_*)
              st.abstractAsView()
            }

            val unionView = PlainView(rewQs.map{ case (n,query) => s"  SELECT * FROM ${n}"}.mkString("\n"," UNION\n", "\n"))
            val unionViewName = "union_" + iri2sql(unionView.hashCode().abs.toString)

            //val (n,x) = SQLStatement.empty.merge(rewQs.unzip._2 :_*).abstractAsView()

            val intervalSelectionView = PlainView(
              s"""
                 |  SELECT ${ansVars.map(_.toString).mkString(",")}, timepoints.dt_from AS dt_from, timepoints.dt_to AS dt_to
                 |  FROM $unionViewName
                 |  INNER JOIN timepoints ON timepoints.id = $unionViewName.timepoint_id
               """.stripMargin)
            //val intervalMappingConstraint = PlainConstraint(s"""ROW(${ansVars.map(_.toField()).mkString(",")},t.id) IN ( SELECT * FROM $unionViewName )""")

            val intervalSelectionViewName =  "t_" + iri2sql(unionView.hashCode().abs.toString)
            val views = rewQs.flatMap{ case (n, q) => q.views }.+:((unionViewName, unionView)).+:((intervalSelectionViewName, intervalSelectionView)).toMap
            // Add the constraint to extract the intervals from the non-temporal relation
            val relation = TemporalRelation(intervalSelectionViewName, ansVars, Polarity.Pos)
            TemporalSQLStatement(views, ListSet(relation), ListSet(), ansVars, ansVars)
            //x2.toTemporalSQLStatement()
          }
        }


    }
      case TemporalConj(fs) => {
        val q = fs.map{ f =>
          TemporalFOQueryTranslator.toTemporalSQLStatement(f)//.computeIslands()._2
        }
        TemporalSQLStatement.empty.merge(q :_*)//.computeIslands()._2
      }
      case TemporalDisj(fs) => {
        val qs = fs.map(f => TemporalFOQueryTranslator.toTemporalSQLStatement(f))
        TemporalSQLStatement.empty.merge(qs:_*).union()._2
      }
      case TemporalNeg(q) => {
        val (name, stmt) = TemporalFOQueryTranslator.toTemporalSQLStatement(q).computeGaps()
        stmt
      }
      case TemporalImpl(left, right) => TemporalFOQueryTranslator.toTemporalSQLStatement(TemporalDisj(List(TemporalNeg(left), right)))
      case TemporalEventually(i, q) => {
        val (name,stmt) = TemporalFOQueryTranslator.toTemporalSQLStatement(q).abstractAsView(Polarity.Pos)
        val sname = s"eventually_in_${i.hashCode().abs}_$name"
        stmt.addView(sname, EventuallyView(stmt.ansVars.map(_.toString()), stmt.relations.head, i)).copy(relations = ListSet(TemporalRelation(sname, stmt.ansVars, Polarity.Pos)))
      }
      case TemporalAlways(i, q) => {
        val (name,stmt) = TemporalFOQueryTranslator.toTemporalSQLStatement(q).computeIslands()
        val sname = s"always_in_${i.hashCode().abs}_$name"
        stmt.addView(sname, AlwaysView(stmt.ansVars.map(_.toString()), stmt.relations.head, i)).copy(relations = ListSet(TemporalRelation(sname, stmt.ansVars, Polarity.Pos)))
      }
      case TemporalConvex(n, q) => ??? /*{
        val t1 = TVar("t1")
        val t2 = TVar("t2")
        val (name,stmt) = TemporalFOQueryTranslator.toTemporalSQLStatement(q).abstractAsView(Polarity.Pos)
        stmt.addVar(t1).addVar(t2)
          .addConstraint(PlainTemporalConstraint({ case v => s" ${v.toField} >= ${t1.toField}" }))
          .addConstraint(PlainTemporalConstraint({ case v => s" ${v.toField} <= ${t2.toField}" }))
          .addConstraint(PlainTemporalConstraint({ case v => s" ${t2.toField} - ${t1.toField} <= INTERVAL '${n.toStandardMinutes.getMinutes} minutes'" }))
      }*/
      case _ => throw new Error(s"This case should not come up with well formed Temporal FO Queries..\n Found: $f")
    }

  }

}