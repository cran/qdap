#' Transform Codes to Start-End Times
#' 
#' Transforms the range coding structure(s) from \code{\link[qdap]{cm_time.temp}} 
#' (in list format) into a data frame of start and end times in long format.
#' 
#' @param \ldots List object(s) in the form generated by 
#' \code{\link[qdap]{cm_time.temp}}.
#' @param v.name An optional name for the column created for the list.var 
#' argument
#' @param list.var logical.  If \code{TRUE} creates a column for the data frame 
#' created by each time.list passed to \code{cm_t2l}.
#' @param debug logical. If \code{TRUE} debugging mode is on.  
#' \code{\link[qdap]{cm_time2long}} will return possible errors in time span 
#' inputs.
#' @param object A list of list object(s) generated by 
#' \code{\link[qdap]{cm_time.temp}}.  
#' @return Generates a dataframe of start and end times for each code.
#' @seealso 
#' \code{\link{cm_df2long}},
#' \code{\link{cm_time.temp}}
#' @references Miles, M. B. & Huberman, A. M. (1994). An expanded sourcebook: 
#' Qualitative   data analysis. 2nd ed. Thousand Oaks, CA: SAGE Publications.
#' @export
#' @importFrom qdapTools sec2hms
#' @examples
#' \dontrun{
#' x <- list(
#'     transcript_time_span = qcv(00:00 - 1:12:00),
#'     A = qcv(terms = "2.40:3.00, 5.01, 6.02:7.00, 9.00"),
#'     B = qcv(terms = "2.40, 3.01:3.02, 5.01, 6.02:7.00, 
#'         9.00, 1.12.00:1.19.01"),
#'     C = qcv(terms = "2.40:3.00, 5.01, 6.02:7.00, 9.00, 17.01")
#' )
#' (dat <- cm_time2long(x))
#' plot(dat)
#' 
#' bar1 <- list(
#'     transcript_time_span = qcv(00:00 - 1:12:00),
#'     A = qcv(terms = "2.40:3.00, 5.01, 6.02:7.00, 9.00"),
#'     B = qcv(terms = "2.40, 3.01:3.02, 5.01, 6.02:7.00, 9.00,
#'         1.12.00:1.19.01"),
#'     C = qcv(terms = "2.40:3.00, 5.01, 6.02:7.00, 9.00, 16.25:17.01")
#' )
#' 
#' bar2 <- list(
#'     transcript_time_span = qcv(00:00 - 1:12:00),
#'     A = qcv(terms = "2.40:3.00, 5.01, 6.02:7.00, 9.00"),
#'     B = qcv(terms = "2.40, 3.01:3.02, 5.01, 6.02:7.00, 9.00,
#'         1.12.00:1.19.01"),
#'     C = qcv(terms = "2.40:3.00, 5.01, 6.02:7.00, 9.00, 17.01")
#' )
#'
#' ## General ldots Approach
#' cm_time2long(bar1)
#' cm_time2long(bar1, bar2, v.name="time")
#' 
#' ## Specify `object` Approach
#' cm_time2long(object=list(bar1=bar1))
#' cm_time2long(object=list(bar1=bar1, bar2=bar2), v.name="time")
#' cm_time2long(object=list(a=bar1, b=bar2), v.name="time")
#' }
cm_time2long <-
function(..., v.name = "variable", list.var = TRUE, debug = TRUE,
    object = NULL){

    if(!is.null(object)){
        L1 <- object
        objs <- names(L1)
    }else{
        mf <- match.call(expand.dots = FALSE)
        objs <- as.character(mf[[2]])
        L1 <- lapply(objs, get)
        names(L1) <- objs
    }
    
    if(debug){
        x <- suppressMessages(lapply(L1, function(x) {
            cm_debug(x)
        }))
        m <- x[!sapply(x, is.null)]
        if (!identical(as.character(m), character(0))) {
            message("Warning: possible errors found:\n")
            print(m); stop("Check warnings")
        }  
    } 

    L1 <- lapply(L1, subout)

    L2 <- lapply(L1, cm_t2l, list.var = FALSE)

### added 10-12-13 and added to class
    spns <- sapply(L2, function(x) {
        as.numeric(gsub("tspan_", "", class(x)[grepl("tspan_", class(x))]))
    })
    spns <- paste(spns, collapse = "||")
### added 10-12-13

    if (list.var) {
        L2 <- lapply(seq_along(L2), function(i) data.frame(L2[[i]], 
            VAR = objs[i], stringsAsFactors = FALSE))
    }
    DF <- data.frame(do.call(rbind, L2), row.names = NULL, stringsAsFactors = FALSE)
    if (list.var) {
        colnames(DF)[ncol(DF)] <- v.name
    }
    class(DF) <- c("cmspans", "cmtime", "cmtime2long", paste0("vname_", v.name), 
        class(DF), paste0("spans_", spns))
    DF
}

## helper to fix poorly formatted overall time span
subout <- function(x) {

    x[[1]] <- Trim(x[[1]])
    if(length(x[[1]]) == 3 && x[[1]][1] == "-") {
        y <- x[[1]][c(2, 1, 3)]
    } else {
        y <- x[[1]]
    }

    x[[1]] <- unlist(strsplit(gsub("-" , "|-|", paste(y, collapse = "")), "\\|"))
    x
}

