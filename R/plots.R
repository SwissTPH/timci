#' Create an enrolment histogram
#'
#' generate_enrolment_hist() creates a histogram plot that represents the enrolment level in the different health facilities.
#' Red bars indicate health facilities which are far below the enrolment target.
#' Orange bars indicate health facilities which are slightly below the enrolment target.
#' Green bars indicate health facilities which are above the enrolment target.
#'
#' @param df Dataframe to use for the plot
#' @return This function returns a ggplot object which contains a histogram representing the enrolment in each facility.
#' @export
#' @import ggplot2

generate_enrolment_hist <- function(df){

  df <- df %>%
    mutate(color_name=case_when(df$x<25 ~ "#ff0000",
                                (df$x>=25 & df$x<45) ~ "#f9c800",
                                df$x>=45 ~ "#a6d40d"))
  ggplot(df, aes(x= reorder(Group.1, -x), y=x)) +
    geom_bar(stat="identity", position = "dodge", fill=df$color_name) +
    labs(x="Facilities", y="Number of children enrolled", title="") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.ticks.y = element_blank()) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_fill_brewer(palette = "Set1")

}

#' Create an enrolment gauge
#'
#' plot_enrolment_gauge() creates a gauge plot that represents the global enrolment with respect to the target.
#'
#' Description of plot_enrolment_gauge
#' @param df Dataframe
#' @param m Maximal gauge value
#' @param s Success
#' @param w Warning
#' @param d Danger
#' @return This function plots a gauge representing enrolment.
#' @export
#' @import flexdashboard

plot_enrolment_gauge <- function(df, m, s, w, d){

  flexdashboard::gauge(sum(df$x), min = 0, max = m, sectors = gaugeSectors(success = s, warning = w, danger = d))

}

#' Create a pie chart plot
#'
#' generate_pie_chart() creates a pie chart plot.
#'
#' @param df Dataframe to use for the plot
#' @return This function returns a ggplot object which contains a pie chart.
#' @export
#' @import ggplot2 scales
#' @importFrom stats reorder

generate_pie_chart <- function(df){

  # Create percentage labels
  df$percentage <- paste(round(df$value / sum(df$value) * 100, 1), " %")

  # Create the pie chart
  ggplot(df, aes(x="", y=value, fill=group)) +
    geom_bar(stat="identity", width=1, color="white") +
    coord_polar("y", start=0) +
    scale_fill_brewer(palette = "Set1", name = "Group:") +
    geom_text(aes(label = percentage), position = position_stack(vjust = 0.5))+
    theme_void() # remove background, grid, numeric labels

}
