% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/concordance.R
\name{PlotConcordance}
\alias{PlotConcordance}
\title{Concordance plot}
\usage{
PlotConcordance(x, ...)
}
\arguments{
\item{x}{A list of adjacency matrices representing edge probabilities, a
chains object, or a collections object with states as DAGs.}

\item{...}{Additional parameter to send to the appropriate method. This includes
'highlight' (defauled to 0.3) which sets the cutoff difference that is used to
highlight the points, and the probability edge estimation 'method' for a
cia_collections object.}
}
\value{
A ggplot object or patchwork of ggplot objects.
}
\description{
Plot a concordance plot to compare point-estimates for quantities of interest
between chains.
}
\examples{
data <- bnlearn::learning.test
dag <- UniformlySampleDAG(colnames(data))
partitioned_nodes <- DAGtoPartition(dag)
scorer <- CreateScorer(scorer = BNLearnScorer, data = data)

results <- SampleChains(10, partitioned_nodes, PartitionMCMC(), scorer, n_parallel_chains = 2)
dags <- PartitiontoDAG(results, scorer)

p_edge <- CalculateEdgeProbabilities(dags)
PlotConcordance(p_edge)

}
