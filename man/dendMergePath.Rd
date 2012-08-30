\name{dendMergePath}
\alias{dendMergePath}
\title{dendMergePath}
\usage{
  dendMergePath(dend, height)
}
\arguments{
  \item{dend}{dendrogram}

  \item{height}{the height where to split}
}
\value{
  either \item{leftD}{recursive list for the left branch}
  \item{rightD}{recursive list for the right branch}
  \item{height}{the height at which to re-merge leftD and
  rightD} \item{members}{the number of unsplit branches in
  this dendrogram} or \item{dend}{the unsplit dendrogram}
  \item{members}{always 1 -- needed for the recursion}
}
\description{
  Recursively split the dendrogram down to the specified
  height
}
\details{
  This function recurses down the dendrogram until it
  reaches the specified height and splits the dendrogram at
  that level, thereby returning some information on the
  sub-branches that are needed for the coloring and the
  re-merging
}
\author{
  Andreas Leha
}

