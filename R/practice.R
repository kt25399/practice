#' @title A heatmap function
#' @description This function will create a heatmap for rows in a data frame
#' @param d
#' @param d dataframe
#' @keywords
#' @export
#' @examples
#' heat_map(d = USArrests)

heat_map <- function(df) {
  distance <- factoextra::get_dist(df)
  factoextra::fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
}

