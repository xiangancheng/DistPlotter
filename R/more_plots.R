#' 绘制 Beta 分布图
#'
#' Beta 分布常用于描述概率或比例（范围 0 到 1）。
#'
#' @param alpha 数值型。形状参数 alpha，必须大于 0。
#' @param beta 数值型。形状参数 beta，必须大于 0。
#' @param fill_color 字符型。填充颜色。
#'
#' @return ggplot 对象
#' @export
#' @examples
#' plot_beta(alpha = 2, beta = 5)
#' plot_beta(alpha = 0.5, beta = 0.5, fill_color = "red")
plot_beta <- function(alpha = 2, beta = 2, fill_color = "lightgreen") {

  p <- ggplot2::ggplot(data.frame(x = c(0, 1)), ggplot2::aes(x = x)) +
    ggplot2::stat_function(fun = dbeta,
                           args = list(shape1 = alpha, shape2 = beta),
                           geom = "area",
                           fill = fill_color,
                           alpha = 0.6) +
    ggplot2::labs(title = paste0("Beta Distribution (alpha=", alpha, ", beta=", beta, ")"),
                  y = "Density", x = "Probability") +
    ggplot2::theme_minimal()

  return(p)
}

#' 绘制卡方分布 (Chi-Squared)
#'
#' @param df 数值型。自由度 (Degrees of Freedom)。
#' @param fill_color 字符型。填充颜色。
#'
#' @return ggplot 对象
#' @export
#' @examples
#' plot_chisq(df = 3)
#' plot_chisq(df = 10, fill_color = "purple")
plot_chisq <- function(df = 3, fill_color = "orange") {

  # 根据自由度自动决定 x 轴的范围
  limit_x <- c(0, max(10, df * 3))

  p <- ggplot2::ggplot(data.frame(x = limit_x), ggplot2::aes(x = x)) +
    ggplot2::stat_function(fun = dchisq,
                           args = list(df = df),
                           geom = "area",
                           fill = fill_color,
                           alpha = 0.6) +
    ggplot2::labs(title = paste0("Chi-Squared Distribution (df=", df, ")"),
                  y = "Density", x = "Value") +
    ggplot2::theme_minimal()

  return(p)
}

#' 对比 T 分布与标准正态分布
#'
#' 用来展示 T 分布的“厚尾”特性。红色为 T 分布，灰色虚线为标准正态分布。
#'
#' @param df 数值型。T 分布的自由度。自由度越小，尾部越厚。
#'
#' @return ggplot 对象
#' @export
#' @examples
#' plot_t_vs_normal(df = 2) # 差异明显
#' plot_t_vs_normal(df = 30) # 几乎重合
plot_t_vs_normal <- function(df = 1) {

  p <- ggplot2::ggplot(data.frame(x = c(-5, 5)), ggplot2::aes(x = x)) +
    # 画标准正态分布（作为背景参考）
    ggplot2::stat_function(fun = dnorm,
                           aes(color = "Normal (0,1)", linetype = "Normal (0,1)"),
                           size = 1) +
    # 画 T 分布
    ggplot2::stat_function(fun = dt,
                           args = list(df = df),
                           aes(color = "T-Distribution", linetype = "T-Distribution"),
                           size = 1.2) +

    ggplot2::scale_color_manual(name = "Distributions",
                                values = c("Normal (0,1)" = "gray", "T-Distribution" = "red")) +
    ggplot2::scale_linetype_manual(name = "Distributions",
                                   values = c("Normal (0,1)" = "dashed", "T-Distribution" = "solid")) +

    ggplot2::labs(title = paste0("T-Distribution (df=", df, ") vs Normal Distribution"),
                  subtitle = "注意观察红色曲线的尾部是否比灰色曲线更高（厚尾）",
                  y = "Density", x = "Value") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "top")

  return(p)
}
