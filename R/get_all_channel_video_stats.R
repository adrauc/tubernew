#' Get statistics on all the videos in a Channel
#'
#' @param channel_id Character. Id of the channel
#' @param username Character. Username of the channel
#' @param mine Boolean. TRUE if you want to fetch stats of your own channel. Default is FALSE.
#' @param \dots Additional arguments passed to \code{\link{tuber_GET}}.
#'
#' @return nested named list with top element names:
#' \code{kind, etag, id,}
#' \code{snippet (list of details of the channel including title)}
#' \code{, statistics (list of 5)}
#'
#' If the \code{channel_id} is mistyped or there is no information, an empty list is returned.
#' Only provide either the channel_id or the username, not both.
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
#' get_all_channel_video_stats(channel_id="loganpaulvlogs")
#' get_all_channel_video_stats(username="UCMtFAi84ehTSYSE9Xo") # Incorrect channel ID
#' }

get_all_channel_video_stats <- function(channel_id = NULL, username = NULL, mine = FALSE, ...) {
  if (!(is.character(channel_id) || is.character(username) || identical(tolower(as.character(mine)), "true"))) {
    stop("Must specify a valid channel ID, username, or set mine = 'true'.")
  }

  filter <- list()
  if (!is.null(channel_id)) filter$channel_id <- channel_id
  if (!is.null(username)) filter$username <- username

  channel_resources <- list_channel_resources(filter = filter, part = "contentDetails")
  playlist_id <- channel_resources$items[[1]]$contentDetails$relatedPlaylists$uploads

  playlist_items <- get_playlist_items(filter = list(playlist_id = playlist_id), max_results = 100)
  vid_ids <- playlist_items$contentDetails.videoId

  merged_df <- get_video_details_all(video_id = vid_ids, part = c("snippet","statistics","contentDetails"), as.data.frame = T)
  return(merged_df)
}

