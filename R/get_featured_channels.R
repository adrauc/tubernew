#' Get all the featured channels on a channel page
#'
#' @param channel_id string; Required.
#' \code{channel_id}: channel ID.
#'
#' @param \dots Additional arguments passed to \code{\link{tuber_GET}}.
#'
#' @return
#' a \code{data.frame} with the following columns:
#' \code{kind, etag, id,}
#' \code{ contentDetails.playlists, contentDetails.channels}
#'
#' @export
#' @references \url{https://developers.google.com/youtube/v3/docs/channelSections}
#'
#' @examples
#' \dontrun{
#'
#' # Set API token via yt_oauth() first
#'
#' get_featured_channels(channel_id = "UCG8rbF3g2AMX70yOd8vqIZg")
#' }

get_featured_channels <- function(channel_id = NULL, ...) {
  querylist <- list(channelId = channel_id, part = "contentDetails")
  res <- tuber_GET("channelSections", query = querylist, ...)
  df <- fromJSON(toJSON(res$items), flatten = TRUE)
  df
}

