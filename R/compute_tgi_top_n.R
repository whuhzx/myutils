#' Compute dataset TGI
#' @param x 数据集
#' @param target 目标对象
#' @param category 分组类型
#' @param tgi_value 数值
#' @param top_number 最大排名
#' @export
compute_tgi_top_n <- function(x, target, category, tgi_value, top_number = 10){
  x %>%
    group_by(!!sym(target), !!sym(category)) %>%
    summarise(value = sum(!!sym(tgi_value))) %>%
    ungroup() %>%
    mutate(total = sum(value)) %>%
    group_by(!!sym(target)) %>%
    mutate(target_ratio = value / sum(value)) %>%
    ungroup() %>%
    group_by(!!sym(category)) %>%
    mutate(total_ratio = sum(value) / total) %>%
    mutate(tgi = round(target_ratio / total_ratio * 100, 0)) %>%
    ungroup() %>%
    arrange(desc(tgi)) %>%
    group_by(!!sym(target)) %>%
    top_n(top_number, tgi) %>%
    ggplot(aes(x = tgi, y = reorder_within(!!sym(category), tgi, !!sym(target)))) +
    geom_col() +
    geom_text(aes(label = tgi)) +
    facet_wrap(vars(!!sym(target)), scale = "free") +
    scale_y_reordered() +
    theme(text = element_text(family = "PingFang SC"))
}
