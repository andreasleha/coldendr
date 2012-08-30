\name{colorDendrogram}
\alias{colorDendrogram}
\title{colorDendrogram}
\usage{
  colorDendrogram(dend, nbranches, branchcols)
}
\arguments{
  \item{dend}{the dendrogram}

  \item{nbranches}{the number of branches (from the top)}

  \item{branchcols}{optional.  the colors to use.  will be
  shortened/recycled}
}
\value{
  a dendrogram with colored branches
}
\description{
  Colorize Branches of a Dendrogram
}
\details{
  This is the main function of the coldendr package.  It
  splits the dendrogram into nbranches branches and gives
  each branch another color.
}
\author{
  Andreas Leha
}

