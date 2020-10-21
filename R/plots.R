#' Generate enrolment hist
#'
#' This function plots a histogram representing enrolment.
#'
#' @param data Data
#' @return This function returns a ggplot object of a histogram representing enrolment in each facility.
#' @export
#' @import ggplot2

generate_enrolment_hist <- function(data){

  data <- data %>%
    mutate(color_name=case_when(data$x<25 ~ "#ff0000",
                                (data$x>=25 & data$x<45) ~ "#f9c800",
                                data$x>=45 ~ "#a6d40d"))
  ggplot(data, aes(x= reorder(Group.1, -x), y=x)) +
    geom_bar(stat="identity", position = "dodge", fill=data$color_name) +
    labs(x="Facilities", y="Number of children enrolled", title="") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.ticks.y = element_blank()) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_fill_brewer(palette = "Set1")

}

#' Generate pie chart with label
#'
#' This function plots a pie chart.
#'
#' @param df Dataframe
#' @return This function returns a ggplot object.
#' @export
#' @import ggplot2 scales
#' @importFrom stats reorder

generate_pie_chart <- function(df){

  # Create the pie chart
  ggplot(df, aes(x="", y=value, fill=group)) +
    geom_bar(stat="identity", width=1, color="white") +
    coord_polar("y", start=0) +
    scale_fill_brewer(palette = "Set1", name = "Group:") +
    #geom_text(aes(y = value/3 + c(0, cumsum(value)[-length(value)]),label = scales::percent(value/100)), size=5) +
    theme_void() # remove background, grid, numeric labels

}
