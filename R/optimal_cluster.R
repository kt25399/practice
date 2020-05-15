#' @title optimal_cluster
#' @description This function creates an elbow plow, silhouette,
#' and gap statistic to help determine the optimal number of clusters
#' for a k-means cluster anaylsis
#' @param d dataframe
#' @keywords
#' @export
#' @examples
#' optimal_cluster(d = iris)

optimal_cluster <- function(d) {
  d2 <- d[,sapply(d, is.numeric)]
  d2 <- na.omit(d2)
  d2 <- scale(d2)

  p1 <- factoextra::fviz_nbclust(d2, kmeans, method = "wss")
  p2 <- factoextra::fviz_nbclust(d2, kmeans, method = "silhouette")
  set.seed(123)
  gap_stat <- cluster::clusGap(d2, FUN = kmeans, nstart = 25,
                      K.max = 10, B = 50)
  p3 <- factoextra::fviz_gap_stat(gap_stat)
  return(list(p1, p2, p3))
}

