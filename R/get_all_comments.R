#' Get all the comments for a video including replies
#'
#' @param video_id string; Required.
#' \code{video_id}: video ID.
#'
#' @param \dots Additional arguments passed to \code{\link{tuber_GET}}.
#'
#' @return
#' a \code{data.frame} with the following columns:
#' \code{authorDisplayName, authorProfileImageUrl, authorChannelUrl,}
#' \code{ authorChannelId.value, videoId, textDisplay,
#' canRate, viewerRating, likeCount, publishedAt, updatedAt,
#' id, moderationStatus, parentId}
#'
#' @export
#' @references \url{https://developers.google.com/youtube/v3/docs/commentThreads/list}
#'
#' @examples
#' \dontrun{
#'
#' # Set API token via yt_oauth() first
#'
#' get_all_comments(video_id = "a-UQz7fqR3w")
#' }

get_all_comments <- function(video_id = NULL, ...) {
  querylist <- list(videoId = video_id, part = "id,replies,snippet", maxResults=100)
  res <- tuber_GET("commentThreads", query = querylist, ...)
  result_list <- list()
  result_list[[1]] <- process_page(res)
  page_token <- res$nextPageToken
  i=2
  while (is.character(page_token)) {

    querylist$pageToken <- page_token
    res <- try(tuber_GET("commentThreads", querylist), silent = T)

    if( class(res)[1] == "try-error" ) {
      Sys.sleep(5)
      res <- tuber_GET("commentThreads", querylist)
    }

    if( res$pageInfo$totalResults == 0) {
      Sys.sleep(5)
      res <- tuber_GET("commentThreads", querylist)
    }

    result_list[[i]] <- process_page(res)
    page_token  <- res$nextPageToken
    i <- i+1
  }
  agg_res <- rbind.fill(result_list)
  agg_res$created_at <- as.POSIXct(agg_res$publishedAt, "%Y-%m-%dT%H:%M:%SZ", tz="UTC")
  agg_res
}


process_page <- function(res = NULL) {

  simple_res  <- lapply(res$items, function(x) {
    unlist(x$snippet$topLevelComment$snippet)
  }
  )

  agg_res <- map_df(simple_res, bind_rows)
  agg_res <- cbind(agg_res, id = sapply(res$items, `[[`, "id"),
                   stringsAsFactors = FALSE)

  agg_res$parentId <- NA

  if (!("moderationStatus" %in% names(agg_res))) {
    agg_res$moderationStatus <- NA
  }

  n_replies   <- sapply(res$items, function(x) {
    unlist(x$snippet$totalReplyCount)
  }
  )

  for (i in seq_len(length(n_replies))) {

    if (n_replies[i] == 1) {

      replies_1  <- lapply(res$items[[i]]$replies$comments,
                           function(x) c(unlist(x$snippet), id = x$id))
      replies_1  <- map_df(replies_1, bind_rows)

      if (nrow(replies_1) > 0 & ! ("moderationStatus" %in% names(replies_1))) {
        replies_1$moderationStatus <- NA
      }
      agg_res    <- rbind(agg_res, replies_1, stringsAsFactors = FALSE)
    }

    if (n_replies[i] > 1) {

      replies_1p  <- lapply(res$items[[i]]$replies$comments,
                            function(x) c(unlist(x$snippet), id = x$id))
      replies_1p  <- map_df(replies_1p, bind_rows)

      if (nrow(replies_1p) > 0 & ! ("moderationStatus" %in% names(replies_1p))) {
        replies_1p$moderationStatus <- NA
      }
      agg_res     <- rbind(agg_res, replies_1p, stringsAsFactors = FALSE)
    }
  }
  agg_res
}


