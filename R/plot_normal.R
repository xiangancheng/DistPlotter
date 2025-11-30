#' 绘制正态分布密度图
#'
#' 这是一个基于 ggplot2 的函数，用于快速可视化正态分布。
#'
#' @param mu 数值型。正态分布的均值，默认为 0。
#' @param sigma 数值型。正态分布的标准差，默认为 1。
#' @param fill_color 字符型。分布区域的填充颜色，默认为 "skyblue"。
#'
#' @return 返回一个 ggplot 对象。
#' @export
#'
#' @importFrom ggplot2 ggplot aes stat_function geom_vline labs theme_minimal
#'
#' @examples
#' plot_normal(mu = 0, sigma = 1, fill_color = "orange")
#' plot_normal(mu = 10, sigma = 5)
plot_normal <- function(mu = 0, sigma = 1, fill_color = "skyblue") {

  # 定义绘图范围 (均值左右 4 个标准差)
  range_limit <- c(mu - 4 * sigma, mu + 4 * sigma)

  p <- ggplot2::ggplot(data.frame(x = range_limit), ggplot2::aes(x = x)) +
    # 绘制密度曲线
    ggplot2::stat_function(fun = dnorm,
                           args = list(mean = mu, sd = sigma),
                           geom = "area",
                           fill = fill_color,
                           alpha = 0.6) +
    # 添加均值虚线
    ggplot2::geom_vline(xintercept = mu, linetype = "dashed", color = "black") +
    # 添加标签和主题
    ggplot2::labs(title = paste0("Normal Distribution (mu=", mu, ", sigma=", sigma, ")"),
                  y = "Density",
                  x = "Value") +
    ggplot2::theme_minimal()

  return(p)
}


