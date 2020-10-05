package de.tu_dresden.epistemic_rewriter

import com.typesafe.scalalogging.StrictLogging
import de.tu_dresden.epistemic_rewriter.datatypes._
import org.joda.time.Period


// Time Intervals are being parsed in ISO 8601 FORMAT (https://en.wikipedia.org/wiki/ISO_8601)

object TemporalFOQueryParser extends FOQueryParser with StrictLogging {

  def setPrefix(query: TemporalFOQuery, databaseManager: DatabaseManager): TemporalFOQuery = {
    query.applyToAtoms(FOQueryParser.setPrefix(_, databaseManager))
  }

  def parse(string: String, databaseManager: DatabaseManager): TemporalFOQuery = setPrefix(parse(string), databaseManager)
  def parse(string: String): TemporalFOQuery = parse(string, tempFOQuery)

  def parse[T](string: String, parser: Parser[T]): T = {
    parseAll(parser, string) match {
      case Success(a,_) => a
      case Failure(m, remaining) => throw new Exception(m+"\n"+remaining.pos.longString)
      case Error(m, remaining) => throw new Exception(m+"\n"+remaining.pos.longString)
    }
  }


  def eventuallySymb: Parser[String] = padWithSpace("E"|"࿇") ^^ { _.toString }
  def alwaysSymb: Parser[String] = padWithSpace("A"|"\u038B") ^^ { _.toString }
  def convexSymb: Parser[String] = padWithSpace("C") ^^ { _.toString }
  def infinitySymb: Parser[String] = padWithSpace("inf") ^^ { _.toString }

  def posInfinitePeriod: Parser[PosInfinitePeriod] = infinitySymb ^^ { case _ => PosInfinitePeriod()}
  def negInfinitePeriod: Parser[NegInfinitePeriod] = "-" ~ infinitySymb ^^ { case _ ~ _ => NegInfinitePeriod()}
  def finitePeriod: Parser[FinitePeriod] = wordNumber ^^ { case x => FinitePeriod(Period.parse(x)) }
  def period: Parser[ExtPeriod] = negInfinitePeriod | posInfinitePeriod | finitePeriod
  def interval: Parser[RelInterval] = "[" ~> period ~ "," ~ period <~ "]" ^^ { case a ~ _ ~ b => RelInterval(a, b)}
  def eventually: Parser[TemporalEventually] = eventuallySymb ~> interval ~ ("(" ~> formulaT <~ ")") ^^ { case i ~ f => TemporalEventually(i, f) }
  def always: Parser[TemporalAlways] = alwaysSymb ~> interval ~ ("(" ~> formulaT <~ ")") ^^ { case i ~ f => TemporalAlways(i, f) }
  def convex: Parser[TemporalConvex] = convexSymb ~> ("[" ~> period <~ "]") ~ ("(" ~> formulaT <~ ")") ^^ { case i ~ f => TemporalConvex(i, f) }
  def atom: Parser[TemporalAtom] = queryPlusFO ^^ { case f => TemporalAtom(f)}
  def negation: Parser[TemporalNeg]   = negationSymb ~ formulaT ^^ { case _ ~ f => TemporalNeg(f) }
  def conjunctionT: Parser[TemporalConj] = repsep1(formulaT, conjunctionSymb) ^^ { case fs => TemporalConj(fs)}
  def implicationT: Parser[TemporalImpl] = formulaT ~ implicationSymb ~ formulaT ^^ { case l ~ _ ~ r => TemporalImpl(l, r)}
  def formulaT : Parser[TemporalFOQuery] =  ("[" ~> implicationT <~ "]") |  ("(" ~> conjunctionT <~ ")") | atom | negation | eventually | always | convex

  def tempFOQuery: Parser[TemporalFOQuery] = formulaT
}



