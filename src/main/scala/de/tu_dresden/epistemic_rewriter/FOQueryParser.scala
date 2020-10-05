package de.tu_dresden.epistemic_rewriter

import com.typesafe.scalalogging.StrictLogging
import de.tu_dresden.epistemic_rewriter.datatypes._
import org.phenoscape.scowl._
import org.semanticweb.owlapi.model.IRI

import scala.util.parsing.combinator.RegexParsers

object FOQueryParser extends FOQueryParser {

  def setPrefix(query: QueryPlusFO, databaseManager: DatabaseManager): QueryPlusFO = {
    // connect to Database
    databaseManager.withConnection { sql_connection =>

      def findString(name: String, relation: String): Option[String] = {
        val sqlquery = s"""SELECT iri FROM $relation WHERE iri SIMILAR TO '%(#|/)$name' LIMIT 1"""
        val statement = sql_connection.prepareStatement(sqlquery)
        val resultSet = statement.executeQuery()
        if (resultSet.next()) {
          Some(resultSet.getString("iri"))
        } else {
          throw new NoSuchFieldException(s"Object $name could not be found in relation $relation in DB ${databaseManager.dbname}")
        }

      }

      def replace(name: String, relation: String): String = findString(name, relation).getOrElse(name)

      def modTerm(term: Term): Term = {
        term match {
          case Constant(n) => Constant(replace(n, "individuals"))
          case Variable(n) => term
        }
      }

      def modAtom(atom: Atom): Atom = {
        val a = atom.map(modTerm)
        a match {
          case clA@ClassAtom(cls, _) => {
            if (clA.cls.isOWLThing) clA
            else clA.copy(cls = Class(replace(cls.getIRI.toString, "concepts")))
          }
          case clA@RoleAtom(role, _, _) => clA.copy(role = ObjectProperty(replace(role.getIRI.toString, "roles")))
        }

      }

      query.applyToAtoms(modAtom)
    }
  }

  def parse(string: String, databaseManager: DatabaseManager): QueryPlusFO = setPrefix(parse(string), databaseManager)

  def parse(string: String): QueryPlusFO = parse(string, queryPlusFO)

  def parse[T](string: String, parser: Parser[T]): T = {
    parseAll(parser, string) match {
      case Success(a, _) => a
      case Failure(m, remaining) => throw new Exception(m + "\n" + remaining.pos.longString)
      case Error(m, remaining) => throw new Exception(m + "\n" + remaining.pos.longString)
    }
  }

}
trait FOQueryParser extends RegexParsers with StrictLogging {
  def padWithSpace[T](p: Parser[T]): Parser[T] = """[\s]*""".r ~ p ~ """[\s]*""".r ^^ { case _ ~ str ~ _ => str}
  def repsep1[T](p:Parser[T], q: Parser[Any]): Parser[List[T]] = repsep(padWithSpace(p), q)
  def repsep1[T](p:Parser[T], q: Parser[Any], begin: Parser[Any] = success(""), end: Parser[Any] = success("")): Parser[List[T]] = opt(begin ~> repsep(padWithSpace(p), q) <~ end) ^^ {
    case Some(value) => value
    case None => List.empty
  }
  def surroundWithBrackets[T](p: Parser[T]): Parser[T] = openingBracket ~ p ~ closingBracket ^^ { case _ ~ str ~ _ => str }
  def optSurroundWithBrackets[T](p: Parser[T]): Parser[T] = (p|surroundWithBrackets(p))

  def negationSymb: Parser[String] = padWithSpace("NOT"|"¬") ^^ { _.toString }
  def implicationSymb: Parser[String] = padWithSpace("IMPL"|"→") ^^ { _.toString }
  def conjunctionSymb: Parser[String] = padWithSpace("AND"|"∧") ^^ { _.toString }
  def openingBracket: Parser[String] = "("
  def closingBracket: Parser[String] = ")"
  def existentialSymb: Parser[String] = ("EX"|"∃")
  def wordNumber: Parser[String]   = """[a-zA-Z0-9-]+""".r       ^^ { _.toString }
  def word: Parser[String]   = """[a-zA-Z-]+""".r       ^^ { _.toString }
  def number: Parser[Int]    = """(0|[1-9]\d*)""".r ^^ { _.toInt }
  def variableName: Parser[Variable]   = """[a-z0-9]+""".r       ^^ { s => Variable(s.toString) }
  def constantName: Parser[Constant]   = """[A-Z][A-Za-z0-9]*""".r       ^^ { s => Constant(s.toString) }
  def termName : Parser[Term] = variableName | constantName
  def className: Parser[String]   = wordNumber ^^ { case wd => wd }
  def roleName: Parser[String]   = wordNumber ^^ { case wd => wd }
  def classAtom: Parser[PosLiteral]   = className ~ "(" ~ termName~ ")" ^^ { case clsN ~ _ ~ varN  ~ _ => if (clsN=="Thing") PosLiteral(ClassAtom(OWLTopClass(IRI.create(clsN)), varN)) else PosLiteral(new ClassAtom(clsN, varN))}
  def negClassAtom: Parser[NegLiteral]   = negationSymb ~ className ~ "(" ~ termName~ ")" ^^ { case _ ~ clsN ~ _ ~ varN  ~ _ => NegLiteral(new ClassAtom(clsN, varN)) }
  def roleAtom: Parser[PosLiteral]   = roleName ~ "(" ~ termName ~ "," ~ termName~ ")" ^^ { case clsN ~ _ ~ varN1  ~ _ ~ varN2 ~ _ => PosLiteral(new RoleAtom(clsN, varN1, varN2)) }
  def negRoleAtom: Parser[NegLiteral]   = negationSymb ~ roleName ~ "(" ~ termName ~ "," ~ termName~ ")" ^^ { case _ ~ clsN ~ _ ~ varN1  ~ _ ~ varN2 ~ _ => NegLiteral(new RoleAtom(clsN, varN1, varN2)) }
  def literal: Parser[FOLiteral] = classAtom | negClassAtom | roleAtom | negRoleAtom
  def variableList: Parser[List[Variable]] = "(" ~> repsep1(variableName, ",") <~ ")" ^^ { case vars:List[Variable] => vars }
  def atomList: Parser[List[FOLiteral]] = repsep1(literal, conjunctionSymb)
  def queryStart: Parser[List[Variable]] = word ~ variableList ~ padWithSpace("=") ^^ { case _ ~ vars ~ _ => vars}
  def varsParser: Parser[List[Variable]] = opt(("EX"|"∃") ~> variableList <~ padWithSpace(".")) ^^ {
    case Some(v) => v
    case None => List()
  }
  def exQuantification: Parser[FOEx] = (existentialSymb ~> variableList <~ padWithSpace(".")) ~ formula ^^ { case vars ~ f => FOEx(vars, f) }
  def conjunction: Parser[FOConj] = repsep1(formula, conjunctionSymb) ^^ { case fs => FOConj(fs)}
  def implication: Parser[FOImpl] = formula ~ implicationSymb ~ formula ^^ { case l ~ _ ~ r => FOImpl(l, r)}
  def formula : Parser[FOQuery] =  ("[" ~> implication <~ "]") | exQuantification | ("(" ~> conjunction <~ ")") | literal

  def cq: Parser[FOQuery] = varsParser  ~ surroundWithBrackets(atomList)  ^^ { case boundVars ~ atoms => FOEx(boundVars, FOConj(atoms)) }
  def AtomWithFilterList: Parser[List[FOQuery]] = repsep1(negClassAtom | negRoleAtom | classAtom | roleAtom | foFilter, conjunctionSymb)
  def foFilter: Parser[FOQuery] = "[" ~> cq ~ implicationSymb ~ queryPlusFO <~ "]" ^^ { case l ~ _ ~ r => FOImpl(l, r.toFirstOrderQuery())}
  def queryPlusFO: Parser[QueryPlusFO] = formula.map(_.toQueryPlusFO)
}



