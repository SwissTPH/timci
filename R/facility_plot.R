#' Retrieves toy data from an Excel file
#'
#' This function retrieves toy data for testing purpose.
#' @return hist
#' @export
#' @import ggplot2 stats

facility_plot <- function() {

  df <- df %>%
    mutate(color_name=case_when(df$x<25 ~ "#ff0000",
                                (df$x>=25 & df$x<45) ~ "#f9c800",
                                df$x>=45 ~ "#a6d40d"))
  hist <- ggplot2::ggplot(df, aes(x=stats::reorder(df$Group.1, -df$x), y=df$x)) +
    geom_bar(stat="identity", position = "dodge", fill=df$color_name) +
    labs(x="Facilities", y="Number of children enrolled", title="") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.ticks.y = element_blank()) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_fill_brewer(palette = "Set1")

  return(hist)

}
