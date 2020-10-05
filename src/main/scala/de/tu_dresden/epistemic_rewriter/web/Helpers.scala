package de.tu_dresden.epistemic_rewriter.web

import de.tu_dresden.epistemic_rewriter.datatypes.{Answer, AnswerWithJustifications, Query, TimePoint}


case class QueryResult(query: Query, answers: Map[_ <: Query, Set[AnswerWithJustifications]], allAnswers: Set[Answer], millis: Option[Long] = None, groundTruthQuery: Option[Query] = None, groundTruthAnswers: Option[Set[Answer]] = None)
case class FormInput(text: String, groundTruth: String, ontologyOption: Int, withInference: Boolean, temporal: Boolean)

case class TemporalQueryResult(query: Query, timePoint: TimePoint, answers: Map[_ <: Query, Set[AnswerWithJustifications]], allAnswers: Set[Answer], millis: Option[Long] = None, groundTruthQuery: Option[Query] = None, groundTruthAnswers: Option[Set[Answer]] = None)
case class TemporalFormInput(text: String, groundTruth: String, timepoint: String)

case class OntologyFormInput(ontologyOption: Int)
