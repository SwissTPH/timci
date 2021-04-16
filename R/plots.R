#' Create a histogram that represents the distribution of values in `col2` for each category in `col1`
#'
#' Red bars indicate health facilities which are far below the lower target.
#' Orange bars indicate health facilities which are above the lower target, but below the upper target.
#' Green bars indicate health facilities which are above the upper target.
#'
#' @param df Data frame to use for the plot
#' @param col1 Column name in data frame `df`
#' @param col2 Column name in data frame `df` that contains numeric values
#' @param lthres Numeric value,  lower threshold
#' @param uthres Numeric value, upper threshold
#' @param lblx Label of the x-axis
#' @param lbly Label of the y-axis
#' @return This function returns a ggplot object which contains a histogram representing the distribution of values in `col2` for each category in `col1`.
#' @export
#' @import ggplot2

generate_enrolment_hist <- function(df, col1, col2, lthres, uthres, lblx, lbly){

  # Quote the arguments that refer to data frame columns
  col1 <- dplyr::enquo(col1)
  col2 <- dplyr::enquo(col2)

  # Transform data
  df <- df %>%
    dplyr::mutate(color_name = dplyr::case_when(!!col2<lthres ~ "#ff0000",
                                                (!!col2>=lthres & !!col2<uthres) ~ "#f9c800",
                                                !!col2>=uthres ~ "#a6d40d"))
  ggplot(df, aes(x= reorder(!!col1, -!!col2), y = !!col2)) +
    geom_bar(stat="identity", position = "dodge", fill = df$color_name) +
    labs(x=lbly, y=lblx, title="") +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.ticks.y = element_blank()) +
    theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
    scale_fill_brewer(palette = "Set1")  +
    coord_flip()

}

#' Generate a calendar heatmap
#'
#' @param df Data frame to use for the plot
#' @param datecol Column name in data frame `df` that contains dates
#' @return This function returns a ggplot object which contains a calendar heatmap.
#' @export
#' @import ggplot2

generate_calendar_heatmap <- function(df, datecol){

  # Quote `datecol`
  datecol <- dplyr::enquo(datecol)

  df1 <- df %>%
    dplyr::group_by(!!datecol) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::select(!!datecol, n) %>%
    dplyr::rename(ndate = !!datecol)

  sdate <- as.Date(min(df1$ndate))
  day_seq <- data.frame(ndate = seq(lubridate::floor_date(sdate, 'month'), as.Date(lubridate::ceiling_date(Sys.Date(), "month") - 1), "days"))
  df2 <- merge(day_seq, df1, by = "ndate", all = TRUE)
  df2$n[(df2$ndate >= sdate) & (df2$ndate <= Sys.Date()) & is.na(df2$n)] <- 0

  df2 <- df2 %>%
    dplyr::mutate(weekday = lubridate::wday(ndate, label = FALSE, abbr = FALSE, week_start = 7),
                  month = lubridate::month(ndate, label = TRUE),
                  date = lubridate::yday(ndate),
                  week = lubridate::epiweek(ndate))
  df2$week[df2$month == "Dec" & df2$week == 1] = 53
  df2 <- df2 %>%
    dplyr::group_by(month) %>%
    dplyr::mutate(monthweek = 1 + week - min(week))

  # Plot
  df2 %>%
    ggplot(aes(weekday,-week, fill = n)) +
    geom_tile(colour = "white")  +
    geom_text(aes(label = lubridate::day(ndate)), size = 2.5, color = "black") +
    theme(aspect.ratio = 1/2,
          legend.position = "top",
          legend.key.width = unit(3, "cm"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          panel.grid = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_blank(),
          legend.title.align = 0.5,
          strip.background = element_blank(),
          strip.text = element_text(face = "bold", size = 15),
          panel.border = element_rect(colour = "grey", fill=NA, size=1),
          plot.title = element_text(hjust = 0.5, size = 21, face = "bold",
                                    margin = margin(0,0,0.5,0, unit = "cm"))) +
    scale_fill_gradientn(colours = c("#ff0000", "#f9c800", "#a6d40d"),
                         values = scales::rescale(c(-1, -0.05, 0, 0.05, 1)),
                         name = "Number of facility submissions",
                         guide = guide_colorbar(title.position = "top",
                                                direction = "horizontal")) +
    facet_wrap(~month, nrow = 4, ncol = 3, scales = "free")

}

#' Create an enrolment gauge
#'
#' plot_enrolment_gauge() creates a gauge plot that represents the global enrolment with respect to the target.
#'
#' Description of plot_enrolment_gauge
#' @param val Value
#' @param lbl Label
#' @param m Maximal gauge value
#' @param lthres Numeric value,  lower threshold
#' @param uthres Numeric value, upper threshold
#' @return This function plots a gauge representing enrolment.
#' @export

plot_enrolment_gauge <- function(val, lbl, m, lthres, uthres){

  df <- data.frame(matrix(nrow = 1, ncol = 2))

  names(df) <- c("variable", "percentage")
  df$variable <- c(lbl)
  df$percentage <- c(val)

  lthres <- lthres / m
  uthres <- uthres / m

  df <- df %>% dplyr::mutate(group=ifelse(percentage < lthres, "red", ifelse(percentage >= lthres & percentage < uthres, "orange","green")),
                             label=paste0(format(round(percentage * 100, 1), nsmall = 1), "%"),
                             title=lbl)

  ggplot(df, aes(fill = group, ymax = percentage, ymin = 0, xmax = 2, xmin = 1)) +
    geom_rect(aes(ymax = 1, ymin = 0, xmax = 2, xmin = 1), fill = "#dcdcdc") +
    geom_rect() +
    coord_polar(theta = "y", start = -pi/2) + xlim(c(0, 2)) + ylim(c(0,2)) +
    geom_text(aes(x = 0, y = 0, label = label, colour = group), size = 10, family = "Poppins SemiBold") +
    geom_text(aes(x = 0.5, y = 1.5, label = title), family = "Poppins Light", size = 4.2) +
    theme_void() +
    scale_fill_manual(values = c("red" = "#ff0000", "orange" = "#f9c800", "green" = "#a6d40d")) +
    scale_colour_manual(values = c("red" = "#ff0000", "orange" = "#f9c800", "green" = "#a6d40d")) +
    theme(strip.background = element_blank(),
          strip.text.x = element_blank()) +
    guides(fill = FALSE) +
    guides(colour = FALSE)

}

#' Create a pie chart plot
#'
#' generate_pie_chart() creates a pie chart plot.
#'
#' @param df Data frame to use for the plot
#' @return This function returns a ggplot object which contains a pie chart.
#' @export
#' @import ggplot2 scales
#' @importFrom stats reorder

generate_pie_chart <- function(df){

  # Create percentage labels
  df$percentage <- paste(round(df$value / sum(df$value) * 100, 1), " %")

  # Create the pie chart
  ggplot(df, aes(x = "", y = value, fill = group)) +
    geom_bar(stat="identity", width=1, color="white") +
    coord_polar("y", start=0) +
    scale_fill_brewer(palette = "Set1", name = "Group:") +
    geom_text(aes(label = percentage), position = position_stack(vjust = 0.5))+
    theme_void() # remove background, grid, numeric labels

}

#' Create a daily bar plot
#'
#' generate_day_bar_plot() creates a bar plot.
#'
#' @param date_vec Vector containing dates
#' @param date_min Start date of the plot
#' @param date_max End date of the plot
#' @param ylbl String of the y-axis
#' @return This function returns a ggplot object which contains a bar plot of frequencies by dates
#' @export
#' @import ggplot2 scales
#' @importFrom stats aggregate

generate_day_bar_plot <- function(date_vec, date_min, date_max, ylbl = "Frequencies"){

  # Count elements
  counts <- aggregate(date_vec, by = list(date_vec), FUN = length)
  counts$names <- as.Date(counts$Group.1, format = "%Y-%m-%d")

  ggplot(counts, aes(x = names, y = x)) +
    geom_bar(stat="identity", fill = "#7dbbd6", width=1) +
    ylab(ylbl) +
    xlab("Date") +
    ggplot2::scale_x_date(limits = c(date_min, date_max),
                          breaks = waiver(),
                          date_labels = "%y-%m-%d") +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_blank(),
          panel.background = element_blank())

}

#' Create a cumulative daily bar plot
#'
#' generate_day_cumbar_plot() creates a bar plot.
#'
#' @param date_vec1 Vector #1 containing dates
#' @param lbl1 Name of vector #1
#' @param date_vec2 Vector #2 containing dates
#' @param lbl2 Name of vector #1
#' @param date_min Start date of the plot
#' @param date_max End date of the plot
#' @param ylim Numeric vector of length 2 that contains the min and max values of the y-axis
#' @param ylbl String of the y-axis
#' @return This function returns a ggplot object which contains a cumulative bar plot of frequencies by dates
#' @export
#' @import ggplot2 scales
#' @importFrom stats aggregate

generate_day_cumbar_plot <- function(date_vec1, lbl1, date_vec2, lbl2, date_min, date_max, ylim, ylbl = "Frequencies"){

  # Count elements
  c1 <- aggregate(date_vec1, by = list(date_vec1), FUN = length)
  c1$names <- as.Date(c1$Group.1, format = "%Y-%m-%d")
  c1$type <- lbl1

  if (length(date_vec2) > 0) {
    c2 <- aggregate(date_vec2, by = list(date_vec2), FUN = length)
    c2$names <- as.Date(c2$Group.1, format = "%Y-%m-%d")
    c2$type <- lbl2
    counts <- rbind(c1, c2)
  } else {
    counts <- c1
  }

  ggplot(counts, aes(x = names, y = x, fill = factor(type, levels=c(lbl2, lbl1)))) +
    geom_bar(stat = "identity", width = 1) +
    ylab(ylbl) +
    xlab("Date") +
    scale_y_continuous(limits = ylim) +
    scale_x_date(limits = c(date_min, date_max),
                 breaks = waiver(),
                 date_labels = "%y-%m-%d") +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_blank(),
          legend.title = element_blank(),
          panel.background = element_blank(),
          legend.position="bottom")

}
