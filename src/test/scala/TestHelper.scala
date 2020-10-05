package de.tu_dresden.epistemic_rewriter.test

import com.typesafe.scalalogging.StrictLogging
import de.tu_dresden.epistemic_rewriter._
import de.tu_dresden.epistemic_rewriter.datatypes.{Answer, QueryPlusFO}


object TestHelper extends StrictLogging {
  def testECQ(ecqS: String, expAnswer: Set[Answer])(implicit model: Modellike, helper: OntologyHelper) : Unit = {
    val ecq = time(FOQueryParser.parse(ecqS, model.getDBManager()), "Query Parsing")
    val cqRewritten = time(EpistemicConjunctiveQueryRewriter.transform(ecq, helper), "Query Rewriting")
    val answers = time(model.getAnswers(cqRewritten), "ECQ checking of " + ecq.pShow)
    assert(answers == expAnswer, s"${ecq.pShow} \n Answers: $answers \n Expectd: $expAnswer")
  }

  def testECQ(ecq: QueryPlusFO, comp: Set[Answer] => Boolean)(implicit model: Modellike, helper: OntologyHelper):Unit= {
    val cqRewritten = time(EpistemicConjunctiveQueryRewriter.transform(ecq, helper), "Query Rewriting")
    val answers = time(model.getAnswers(cqRewritten), "ECQ checking of " + ecq.pShow)
    assert(comp(answers), ecq.pShow + "\n Answers: " + answers)
  }

  def testECQ(ecqS: String, comp: Set[Answer] => Boolean)(implicit model: Modellike, helper: OntologyHelper):Unit= {
    val ecq = time(FOQueryParser.parse(ecqS, model.getDBManager()), "Query Parsing")
    testECQ(ecq, comp)
  }

  def time[R](block: => R, msg: String = ""): R = {
    logger.info( msg + "...")
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    logger.info("Done in: " + (t1 - t0) / 1000000000 + "s")
    result
  }

  /*def timedFuture[T](block: => Future[T]): Future[T] = {
    val start = System.currentTimeMillis()
    val result = block
    result.onComplete({
      case _ => println(s"Future took ${System.currentTimeMillis() - start} ms")
    })
    result
  }*/
}
