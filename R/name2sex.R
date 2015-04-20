#' Names to Gender
#' 
#' A wrapper for the \code{\link[gender]{gender}} function used to predict 
#' gender based on first name.
#' 
#' @param names.list Character vector containing first names.
#' @param USE.NAMES logical.  If \code{TRUE} names.list is used to name the 
#' gender vector.
#' @param \ldots Other arguments passed to \code{\link[gender]{gender}}.
#' @return Returns a vector of predicted gender (M/F) based on first name.
#' @keywords name gender
#' @export
#' @importFrom qdapTools lookup
#' @importFrom gender gender
#' @seealso \code{\link[gender]{gender}}
#' @examples
#' \dontrun{
#' name2sex(qcv(mary, jenn, linda, JAME, GABRIEL, OLIVA, 
#'     tyler, jamie, JAMES, tyrone, cheryl, drew))
#' }
name2sex <- function(names.list, USE.NAMES = FALSE, ...) {

    nms <- if (USE.NAMES) {names.list} else {NULL}
    genkey <- data.frame(c("male", "female"), c("M", "F")) 
    `%lc_qdap%` <- qdapTools::`%lc%`
    
    gender::gender(names.list, ...) %>% 
        sapply("[[", "gender") %lc_qdap% 
        genkey %>% 
        as.factor() %>% 
        setNames(nm=nms)

}