#' Delete a Video
#'
#' @param id   String. Required. id of the video that is to be deleted
#' @param \dots Additional arguments passed to \code{\link{tuber_DELETE}}.
#'
#' @references \url{https://developers.google.com/youtube/v3/docs/playlistItems/delete}
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # Set API token via yt_oauth() first
#'
#' delete_videos(id = "y3ElXcEME3lSISz6izkWVT5GvxjPu8pA")
#' }

delete_videos <- function(id = NULL, ...) {

  if (!is.character(id) || length(id) != 1 || nchar(id) == 0) {
    stop("Must specify a valid ID.")
  }

  querylist <- list(id = id)
  raw_res <- tuber_DELETE("videos", query = querylist, ...)

  raw_res
}
