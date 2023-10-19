#' Get statistics of a Channels
#'
#' @param ids Character. Id of the channel
#' @param part What parts should be returned
#' @param \dots Additional arguments passed to \code{\link{tuber_GET}}.
#'
#' @return data frame:
#' \code{kind, etag, id, snippet (list of details of the channel
#' including title), statistics}
#'
#' If the \code{ids} is mistyped or there is no information, an empty list is returned
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
#' get_channel_stats_all(ids="UCMtFAi84ehTSYSE9XoHefig")
#' get_channel_stats(ids="UCMtFAi84ehTSYSE9Xo") # Incorrect channel ID
#' }

get_channel_stats_all <- function(ids, part=c("snippet", "statistics"), ...) {
  channel_list <- list()
  length_ids <- ceiling(length(ids)/50)
  for(i in 1:length_ids) {
    if(i != length_ids) {
      querylist = list(part = paste(part, collapse=","),
                       id=paste(ids[(i*50-49):(i*50)], collapse=","),
                       maxResults = "50")
    } else {
      querylist = list(part = paste(part, collapse=","),
                       id=paste(ids[(i*50-49):length(ids)], collapse=","),
                       maxResults = "50")
    }
    test <- tuber_GET("channels", query = querylist, ...)
    if (length(test$items) == 0) {
      warning("No channel stats available. Likely cause: Incorrect channel_id.\n")
      return(list())
    }
    temp_df <- lapply(test$items, function(x) unlist(x$snippet))
    temp_df <- plyr::ldply(temp_df, rbind)
    temp_stats <- lapply(test$items, function(x) unlist(x$statistics))
    temp_stats <- plyr::ldply(temp_stats, rbind)
    temp_df <- cbind(temp_df, temp_stats)
    temp_df[, ] <- lapply(temp_df[, ], as.character)
    temp_df$id <- sapply(test$items, function(x) unlist(x$id))
    temp_df$etag <- sapply(test$items, function(x) unlist(x$etag))
    temp_df$viewCount <- as.numeric(temp_df$viewCount)
    temp_df$subscriberCount <- as.numeric(temp_df$subscriberCount)
    temp_df$videoCount <- as.numeric(temp_df$videoCount)
    temp_df$created_at <- as.POSIXct(temp_df$publishedAt, "%Y-%m-%dT%H:%M:%S", tz="UTC")
    channel_list[[i]] <- temp_df
  }
  rbind.fill(channel_list)
}



