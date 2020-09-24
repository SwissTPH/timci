#' Generate enrolment hist
#'
#' This function plots a histogram representing enrolment.
#'
#' Description of generate_enrolment_hist
#' @param data Data
#' @return This function generates a histogram representing enrolment in each facility.
#' @export

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
