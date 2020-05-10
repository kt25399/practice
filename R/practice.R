#' @title optimal_cluster
#' @description This function creates an elbow plow, silhouette,
#' and gap statistic to help determine the optimal number of clusters
#' for a k-means cluster anaylsis
#' @param df
#' @param df dataframe
#' @keywords
#' @export
#' @examples
#' optimal_cluster(df = iris[, -5])

optimal_cluster <- function(df) {
  df <- na.omit(df)
  df <- scale(df)
  head(df)

  p1 <- factoextra::fviz_nbclust(df, kmeans, method = "wss")
  p2 <- factoextra::fviz_nbclust(df, kmeans, method = "silhouette")
  set.seed(123)
  gap_stat <- cluster::clusGap(df, FUN = kmeans, nstart = 25,
                      K.max = 10, B = 50)
  p3 <- factoextra::fviz_gap_stat(gap_stat)
  return(list(p1, p2, p3))
}
