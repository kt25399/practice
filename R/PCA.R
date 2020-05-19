#' @title PCA
#' @description This function does a principle component analysis on a data frame then creates
#' plots to visualize the data
#' @param d dataframe
#' @param group groups to create confidence interval for PCA plot
#' @keywords PCA
#' @export
#' @examples
#' PCA(d = iris, group = iris$Species)

PCA <- function(d, group) {
  d2 <- d[,sapply(d, is.numeric)]
  d2 <- na.omit(d2)
  p <- prcomp(d2, scale = TRUE)
  p1 <- plot(p, type = 'l')
  p2 <- summary(p)
  p3 <- biplot(p, scale = 0)
  d3 <- cbind(d, p$x)

  p4 <- ggplot2::ggplot(data = d3, ggplot2::aes(x = PC1, y = PC2, color = group, fill = group)) +
    ggplot2::stat_ellipse(geom = "polygon", color = "black", alpha = 0.5) +
    ggplot2::geom_point(color = "black")

  stuff <- list(p1, p2, p3, p4, p, d3)
  stuff
}
