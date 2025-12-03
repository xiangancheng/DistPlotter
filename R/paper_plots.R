#' 绘制论文级 PCA 图 (Principal Component Analysis)
#'
#' 用于展示样本间的聚类关系和批次效应。
#'
#' @param data 矩阵或数据框。行是样本 (Samples)，列是特征 (Genes/Features)。
#' @param metadata 数据框。包含样本的元数据，必须有一列与 data 的行名匹配。
#' @param group_var 字符型。用于分组着色的元数据列名（如 "Condition"）。
#'
#' @return ggplot 对象
#' @export
#' @importFrom stats prcomp
#' @importFrom ggplot2 ggplot aes geom_point labs theme_bw
#' @examples
#' # 模拟数据：15个样本，100个特征
#' set.seed(42)
#' expr_matrix <- matrix(rnorm(1500), nrow = 15)
#' rownames(expr_matrix) <- paste0("Sample", 1:15)
#'
#' # 模拟元数据：分为 Treatment 和 Control 两组
#' meta_data <- data.frame(
#'   SampleID = paste0("Sample", 1:15),
#'   Condition = factor(rep(c("Control", "Treatment"), each = 7.5))
#' )
#'
#' plot_pca_samples(data = expr_matrix, metadata = meta_data, group_var = "Condition")
plot_pca_samples <- function(data, metadata, group_var) {

  # 1. 运行 PCA (确保行是样本)
  pca_result <- stats::prcomp(data, scale. = TRUE)

  # 2. 提取主成分得分
  pca_scores <- as.data.frame(pca_result$x)

  # 3. 计算方差解释比例
  variance <- (pca_result$sdev)^2 / sum(pca_result$sdev^2)
  pc1_var <- round(variance[1] * 100, 2)
  pc2_var <- round(variance[2] * 100, 2)

  # 4. 合并元数据
  # 假设 metadata 的第一列是样本 ID
  if (nrow(data) == nrow(metadata)) {
    pca_plot_data <- cbind(pca_scores, metadata)
  } else {
    stop("Data rows and Metadata rows must match.")
  }

  # 5. 绘图
  p <- ggplot2::ggplot(pca_plot_data,
                       ggplot2::aes(x = PC1, y = PC2, color = .data[[group_var]])) +
    ggplot2::geom_point(size = 3, alpha = 0.8) +
    ggplot2::labs(
      title = "PCA of Samples",
      x = paste0("PC1 (", pc1_var, "%)"),
      y = paste0("PC2 (", pc2_var, "%)"),
      color = group_var
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(aspect.ratio = 1) # 保持图表为正方形

  return(p)
}

#' 绘制分组统计柱状图 (带标准误)
#'
#' 适用于比较实验组和对照组的平均值，并用标准误 (SE) 表示不确定性。
#'
#' @param data 数据框。包含分组变量 (group_var) 和测量变量 (value_var)。
#' @param group_var 字符型。分组列名（如 "Treatment"）。
#' @param value_var 字符型。测量值列名（如 "Expression"）。
#' @param error_type 字符型。误差棒类型 ("se" 或 "sd")。
#'
#' @return ggplot 对象
#' @export
#' @importFrom dplyr group_by summarise mutate %>%
#' @importFrom ggplot2 ggplot aes geom_col geom_errorbar labs theme_classic
#' @examples
#' # 模拟实验数据
#' df_summary <- data.frame(
#'   Treatment = factor(rep(c("Control", "Drug_A", "Drug_B"), each = 20)),
#'   Measurement = rnorm(60, mean = rep(c(10, 15, 8), each = 20), sd = rep(c(1, 2, 1), each = 20))
#' )
#'
#' plot_stat_bar(data = df_summary, group_var = "Treatment", value_var = "Measurement", error_type = "se")
plot_stat_bar <- function(data, group_var, value_var, error_type = "se") {

  # 计算标准误函数
  standard_error <- function(x) sd(x) / sqrt(length(x))

  # 汇总数据
  summary_data <- data %>%
    dplyr::group_by(.data[[group_var]]) %>%
    dplyr::summarise(
      Mean = mean(.data[[value_var]], na.rm = TRUE),
      SD = sd(.data[[value_var]], na.rm = TRUE),
      SE = standard_error(.data[[value_var]])
    ) %>%
    dplyr::mutate(
      Error = if (error_type == "se") SE else SD # 选择误差类型
    )

  # 绘图
  p <- ggplot2::ggplot(summary_data, ggplot2::aes(x = .data[[group_var]], y = Mean)) +
    ggplot2::geom_col(fill = "steelblue", alpha = 0.8) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = Mean - Error, ymax = Mean + Error),
      width = 0.2, position = ggplot2::position_dodge(0.9)
    ) +
    ggplot2::labs(
      title = paste("Group Means with", toupper(error_type)),
      x = group_var,
      y = value_var
    ) +
    ggplot2::theme_classic()

  return(p)
}
