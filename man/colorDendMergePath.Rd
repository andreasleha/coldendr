\name{colorDendMergePath}
\alias{colorDendMergePath}
\title{colorDendMergePath}
\usage{
  colorDendMergePath(dend, colors)
}
\arguments{
  \item{dend}{split dendrogram as produced by
  dendMergePath}

  \item{colors}{optional. the colors to use. will be
  shortened/recycled to the correct length}
}
\value{
  a split dendrogram
}
\description{
  Adds the color attribute to sub-branches of the
  dendrogram
}
\details{
  This function takes a split dendrogram as produced by the
  dendMergePath function of this package and adds the
  supplied colors to the sub-branches
}
\author{
  Andreas Leha
}

