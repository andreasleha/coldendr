\name{dendFindHeight}
\alias{dendFindHeight}
\title{dendFindHeight}
\usage{
  dendFindHeight(dend, nbranches)
}
\arguments{
  \item{dend}{dendrogram}

  \item{nbranches}{the desired number of branches}
}
\value{
  an appropriate height where to cut
}
\description{
  Given a dendrogram and the desired number of branches,
  this function finds the height where to cut the
  dendrogram
}
\details{
  This funciton uses binary search to find a height for
  cutting the supplied dendrogram.  When cut at the
  returned height, the dendrogram will split up into the
  desired number of branches.
}
\author{
  Andreas Leha
}

