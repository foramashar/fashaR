#' Updated version of make.groups
#'
#' @param ... A vector with name of dataframes to be merged
#' @export
#'
#' @seealso lattice
#'

get.make.groups=function (...) 
{
    tmp <- sapply(..., get, simplify=F)
    nms <- names(tmp)
    if (is.null(names(tmp))) 
        names(tmp) <- nms
    else {
        unnamed <- names(tmp) == ""
        names(tmp)[unnamed] <- nms[unnamed]
    }
    if (all(sapply(tmp, is.data.frame))) {
        cbind(do.call(rbind, tmp), which = rep(gl(length(tmp), 
            1, labels = names(tmp)), sapply(tmp, nrow)))
    }
    else data.frame(data = unlist(tmp), which = rep(gl(length(tmp), 
        1, labels = names(tmp)), sapply(tmp, length)))
}

