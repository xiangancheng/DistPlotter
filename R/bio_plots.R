#' 绘制 F 分布 (F-Distribution)
#'
#' F 分布常用于方差分析 (ANOVA) 和回归模型的比较。
#'
#' @param df1 数值型。分子自由度 (Numerator DF)。
#' @param df2 数值型。分母自由度 (Denominator DF)。
#' @param fill_color 字符型。填充颜色。
#'
#' @return ggplot 对象
#' @export
#' @importFrom ggplot2 ggplot aes stat_function labs theme_minimal
#' @examples
#' # 经典的 F(5, 20) 分布
#' plot_f_dist(df1 = 5, df2 = 20)
plot_f_dist <- function(df1 = 5, df2 = 20, fill_color = "lightcoral") {

  # 确定 x 轴范围
  limit_x <- c(0, max(5, df1 * 2))

  p <- ggplot2::ggplot(data.frame(x = limit_x), ggplot2::aes(x = x)) +
    ggplot2::stat_function(fun = df,
                           args = list(df1 = df1, df2 = df2),
                           geom = "area",
                           fill = fill_color,
                           alpha = 0.6) +
    ggplot2::labs(title = paste0("F-Distribution (DF1=", df1, ", DF2=", df2, ")"),
                  y = "Density", x = "F Statistic") +
    ggplot2::theme_minimal()

  return(p)
}

#' 绘制火山图 (Volcano Plot)
#'
#' 生物信息学中用于可视化差异表达分析结果。
#'
#' @param data 数据框。至少包含两列: logFC 和 pvalue。
#' @param fc_cutoff 数值型。logFC 的阈值，默认为 1。
#' @param p_cutoff 数值型。pvalue 的阈值，默认为 0.05。
#'
#' @return ggplot 对象
#' @export
#' @importFrom ggplot2 ggplot aes geom_point labs theme_minimal scale_color_manual
#' @examples
#' # 模拟差异表达数据
#' set.seed(42)
#' df_volcano <- data.frame(
#'     logFC = c(rnorm(100, 0, 0.5), rnorm(10, 3, 0.5), rnorm(10, -3, 0.5)),
#'     pvalue = runif(120, 0, 1)
#' )
#' plot_volcano(df_volcano, fc_cutoff = 2, p_cutoff = 0.01)
plot_volcano <- function(data, fc_cutoff = 1, p_cutoff = 0.05) {

  # 计算 -log10(pvalue)
  data$log10P <- -log10(data$pvalue)

  # 定义显著性组别
  data$Significance <- "NS" # Not Significant
  data$Significance[data$log10P > -log10(p_cutoff) & abs(data$logFC) > fc_cutoff] <- "Significant"
  data$Significance[data$logFC > fc_cutoff & data$log10P > -log10(p_cutoff)] <- "Up-regulated"
  data$Significance[data$logFC < -fc_cutoff & data$log10P > -log10(p_cutoff)] <- "Down-regulated"

  # 调整颜色
  color_map <- c("Up-regulated" = "red", "Down-regulated" = "blue", "NS" = "gray")

  p <- ggplot2::ggplot(data, ggplot2::aes(x = logFC, y = log10P, color = Significance)) +
    ggplot2::geom_point(alpha = 0.6, size = 1.5) +
    ggplot2::scale_color_manual(values = color_map) +

    # 添加截止线
    ggplot2::geom_hline(yintercept = -log10(p_cutoff), linetype = "dashed", color = "black") +
    ggplot2::geom_vline(xintercept = c(-fc_cutoff, fc_cutoff), linetype = "dashed", color = "black") +

    ggplot2::labs(title = "Volcano Plot (Differential Expression)",
                  x = "Log2(Fold Change)",
                  y = "-Log10(P-value)") +
    ggplot2::theme_minimal()

  return(p)
}
