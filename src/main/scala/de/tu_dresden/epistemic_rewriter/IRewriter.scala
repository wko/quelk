package de.tu_dresden.epistemic_rewriter

trait IRewriter[T,Q] {
  def transform(query: T, ontologyHelper: OntologyHelper): Q
}
