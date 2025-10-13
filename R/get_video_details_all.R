#' Get Details of list of videos
#'
#' Get details such as when the video was published, the title, description, thumbnails, category etc.
#'
#' @param video_id Comma separated list of IDs of the videos for which details are requested. Required.
#' @param part Comma-separated list of video resource properties requested. Options include:
#' \code{contentDetails, fileDetails, id, liveStreamingDetails, localizations, player, processingDetails,
#' recordingDetails, snippet, statistics, status, suggestions, topicDetails}
#' @param \dots Additional arguments passed to \code{\link{tuber_GET}}.
#'
#' @return list. If part is snippet, the list will have the following elements:
#' \code{id} (video id that was passed), \code{publishedAt, channelId, title, description, thumbnails,
#' channelTitle, categoryId, liveBroadcastContent, localized, defaultAudioLanguage}
#'
#' @export

#' @references \url{https://developers.google.com/youtube/v3/docs/videos/list}

#' @examples
#' \dontrun{
#'
#' # Set API token via yt_oauth() first
#'
#' get_video_details(video_id = "yJXTXN4xrI8")
#' get_video_details(video_id = "yJXTXN4xrI8", part = "contentDetails")
#' }

get_video_details_all <- function(video_id = NULL, part = c("snippet","statistics","contentDetails"), as.data.frame = FALSE, ...) {

  results <- list()
  n_loop <- ceiling(length(video_id)/50)

  for(i in 1:n_loop) {
    if(i == n_loop) {
      results[[i]] <- get_video_details(video_id = video_id[(i*50-49):length(video_id)], as.data.frame = as.data.frame,  part = part)
    } else {
      results[[i]] <- get_video_details(video_id = video_id[(i*50-49):(i*50)], as.data.frame = as.data.frame,  part = part)
    }
    message("finished with ", i, "/",n_loop)
  }
  results <- dplyr::bind_rows(results)

  results$created_at <- as.POSIXct(results$publishedAt, "%Y-%m-%dT%H:%M:%SZ", tz="UTC")
  if("statistics" %in% part) {
    results$statistics_viewCount <- as.numeric(results$statistics_viewCount)
    if("statistics_commentCount" %in% colnames(results)) {
      results$statistics_commentCount <- as.numeric(results$statistics_commentCount)
    }
    results$statistics_likeCount <- as.numeric(results$statistics_likeCount)
    results$statistics_favoriteCount <- as.numeric(results$statistics_favoriteCount)
  }
  if("contentDetails" %in% part) {
    results$video_duration <- lubridate::duration(results$contentDetails_duration)
  }
  return(results)
}
