#' @title visualization
#' @description This function creates plots to visualize data for a number of
#' clusters
#' @param d dataframe
#' @param n number of clusters
#' @keywords
#' @export
#' @examples
#' visualization(d = iris, n = 2:4)

visualization <- function(d, n) {
  d2 <- d[,sapply(d, is.numeric)]
  d2 <- na.omit(d2)
  d2 <- scale(d2)

  x <- list()
  v <- list()

  for (i in 1:length(n))  {
    x[[i]] <- kmeans(d2, n[[i]], nstart = 25)
  }

  for (i in 1:length(x)) {
    v[[i]] <- factoextra::fviz_cluster(x[[i]], data = d2)
  }
  v
}
