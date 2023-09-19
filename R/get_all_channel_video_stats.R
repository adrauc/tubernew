#' Get statistics on all the videos in a Channel
#'
#' @param channel_id Character. Id of the channel
#' @param mine Boolean. TRUE if you want to fetch stats of your own channel. Default is FALSE.
#' @param \dots Additional arguments passed to \code{\link{tuber_GET}}.
#'
#' @return nested named list with top element names:
#' \code{kind, etag, id,}
#' \code{snippet (list of details of the channel including title)}
#' \code{, statistics (list of 5)}
#'
#' If the \code{channel_id} is mistyped or there is no information, an empty list is returned
#'
#' @export
#'
#' @references \url{https://developers.google.com/youtube/v3/docs/channels/list}
#'
#' @examples
#' \dontrun{
#'
#' # Set API token via yt_oauth() first
#'
#' get_all_channel_video_stats(channel_id="UCxOhDvtaoXDAB336AolWs3A")
#' get_all_channel_video_stats(channel_id="UCMtFAi84ehTSYSE9Xo") # Incorrect channel ID
#' }

get_all_channel_video_stats <- function(channel_id = NULL, mine = FALSE, ...) {
  if (!is.character(channel_id) && !identical(tolower(mine), "true")) {
    stop("Must specify a valid channel ID or set mine = 'true'.")
  }

  channel_resources <- list_channel_resources(filter = list(channel_id = channel_id), part = "contentDetails")
  playlist_id <- check_test$items[[1]]$contentDetails$relatedPlaylists$uploads

  playlist_items <- get_playlist_items(filter = list(playlist_id = playlist_id), max_results = 100)
  vid_ids <- playlist_items$contentDetails$videoId

  merged_df <- get_video_details_all(video_id = vid_ids, part = c("snippet","statistics"), as.data.frame = T)
  return(merged_df)
}

