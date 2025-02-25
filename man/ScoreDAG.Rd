% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scorer_funs.R
\name{ScoreDAG}
\alias{ScoreDAG}
\title{Score DAG.}
\usage{
ScoreDAG(dag, scorer)
}
\arguments{
\item{dag}{Adjacency matrix of (parent, child) entries with 1 denoting an
edge and 0 otherwise.}

\item{scorer}{Scorer object.}
}
\value{
Log of DAG score.
}
\description{
Score DAG.
}
\examples{
dag <- UniformlySampleDAG(names(bnlearn::asia))
scorer <- CreateScorer(data = bnlearn::asia)
ScoreDAG(dag, scorer)

}
