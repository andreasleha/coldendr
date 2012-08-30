#' coldendr
#' 
#' @name coldendr
#' @docType package
NULL

##' Colorize Branches of a Dendrogram
##'
##' This is the main function of the coldendr package.  It splits the
##' dendrogram into nbranches branches and gives each branch another color.
##' @title colorDendrogram
##' @param dend the dendrogram
##' @param nbranches the number of branches (from the top)
##' @param branchcols optional.  the colors to use.  will be shortened/recycled
##' @return a dendrogram with colored branches
##' @author Andreas Leha
##' @export
colorDendrogram <- function(dend, nbranches, branchcols) {
  ## fix colors
  if (missing(branchcols)) {
    branchcols <- 1:nbranches
  } else {
    branchcols <- rep(branchcols, length.out=nbranches)
  }
  
  h <- dendFindHeight(dend, nbranches)
  dend <- dendMergePath(dend, h)
  dend <- colorDendMergePath(dend, branchcols)
  dend <- mergeDendMergePath(dend)
  
  dend
}

##' Given a dendrogram and the desired number of branches, this function
##' finds the height where to cut the dendrogram
##'
##' This funciton uses binary search to find a height for cutting the
##' supplied dendrogram.  When cut at the returned height, the dendrogram will
##' split up into the desired number of branches.
##' @title dendFindHeight 
##' @param dend dendrogram
##' @param nbranches the desired number of branches
##' @return an appropriate height where to cut
##' @author Andreas Leha
##' @export
dendFindHeight <- function(dend, nbranches) {
  if (! (nbranches %in% 1:(attributes(dend)$member)))
    stop(paste("The dendrogram has",
               attributes(dend)$member,
               "members, s.t. coloring",
               nbranches,
               "branches is not possible"))
  
  h <- attributes(dend)$height
  
  if (nbranches == 1)
    return(2*h)
  
  if (nbranches == 2)
    return(h)
  
  oh <- h
  nh <- h/2
  tmp <- cut(dend, h=nh)
  cnbranches <- length(tmp$lower) 
  while (! (cnbranches == nbranches)) {
    hdiff <- abs(oh - nh)/2
    oh <- nh
    if (cnbranches < nbranches) {
      nh <- nh - hdiff
    } else {
      nh <- nh + hdiff
    }
    tmp <- cut(dend, h=nh)
    cnbranches <- length(tmp$lower) 
  }
  nh
}

##' Recursively split the dendrogram down to the specified height
##'
##' This function recurses down the dendrogram until it reaches the specified
##' height and splits the dendrogram at that level, thereby returning some
##' information on the sub-branches that are needed for the coloring and the
##' re-merging
##' @title dendMergePath
##' @param dend dendrogram
##' @param height the height where to split
##' @return either
##'         \item{leftD}{recursive list for the left branch}
##'         \item{rightD}{recursive list for the right branch}
##'         \item{height}{the height at which to re-merge leftD and rightD}
##'         \item{members}{the number of unsplit branches in this dendrogram}
##'         or
##'         \item{dend}{the unsplit dendrogram}
##'         \item{members}{always 1 -- needed for the recursion}
##' @author Andreas Leha
dendMergePath <- function(dend, height) {
  if (attributes(dend)$height < height) {
    return(list(dend=dend, members=1))
  } else {
    leftD=dendMergePath(dend[[1]], height)
    rightD=dendMergePath(dend[[2]], height)
    return(list(left=leftD,
                right=rightD,
                height=attributes(dend)$height,
                members=(leftD$members + rightD$members)))
  }
}

##' re-merge a previously split dendrogram
##'
##' This function re-merges a dendrogram that was split using the dendMergePath
##' function from this package
##' @title mergeDendMergePath
##' @param dend dendrogram
##' @return the re-merged dendrogram
##' @author Andreas Leha
mergeDendMergePath <- function(dend) {
  if (length(dend)==2) {
    return(dend$dend)
  } else {
    return(merge(mergeDendMergePath(dend$left),
                 mergeDendMergePath(dend$right),
                 height=dend$height))
  }
}

##' Adds the color attribute to sub-branches of the dendrogram
##'
##' This function takes a split dendrogram as produced by the dendMergePath
##' function of this package and adds the supplied colors to the sub-branches
##' @title colorDendMergePath
##' @param dend split dendrogram as produced by dendMergePath
##' @param colors optional. the colors to use. will be shortened/recycled to the correct length
##' @return a split dendrogram
##' @author Andreas Leha
colorDendMergePath <- function(dend, colors) {
  addEdgeCol <- function(n, edgecol) {
    attr(n, "edgePar") <- list(col=edgecol)
    n
  }
  if (length(dend)==2) {
    dend$dend <- dendrapply(dend$dend, addEdgeCol, colors)
    return(dend)
  } else {
    dend$left <- colorDendMergePath(dend$left, colors[1:dend$left$members])
    dend$right <-
      colorDendMergePath(dend$right,
                         colors[(dend$left$members+1):(dend$left$members + dend$right$members)])
    return(dend)
  }
}
