#' Delete a Particular Comment
#'
#' @param id   String. Required. id of the comment being retrieved
#' @param \dots Additional arguments passed to \code{\link{tuber_DELETE}}.
#'
#'
#' @references \url{https://developers.google.com/youtube/v3/docs/comments/delete}
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # Set API token via yt_oauth() first
#'
#' delete_comments(id = "y3ElXcEME3lSISz6izkWVT5GvxjPu8pA")
#' }

delete_comments <- function(id = NULL, ...) {

  if (!is.character(id) || length(id) != 1 || nchar(id) == 0) {
    stop("Must specify a valid ID.")
  }

  querylist <- list(id = id)
  raw_res <- tuber_DELETE("comments", query = querylist, ...)

  raw_res
}
