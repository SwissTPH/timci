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

generate_enrolment_hist <- function(df,
                                    col1,
                                    col2,
                                    lthres,
                                    uthres,
                                    lblx,
                                    lbly){

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

#' Create an enrolment gauge
#'
#' plot_gauge() creates a gauge plot that represents the current rate with respect to a target.
#'
#' Description of plot_enrolment_gauge
#' @param val Value
#' @param lbl Label
#' @param m Maximal gauge value
#' @param lthres Numeric value,  lower threshold
#' @param uthres Numeric value, upper threshold
#' @param scale Numeric value (optional, default 1), must vary between 0 and 1 to scale the text size
#' @return This function plots a gauge with rate and title.
#' @export

plot_gauge <- function(val, lbl, m, lthres, uthres, scale = 1){

  df <- data.frame(matrix(nrow = 1, ncol = 2))

  names(df) <- c("variable", "percentage")
  df$variable <- c(lbl)
  df$percentage <- c(val)

  lthres <- lthres / m
  uthres <- uthres / m

  df <- df %>% dplyr::mutate(group = ifelse(percentage < lthres, "red", ifelse(percentage >= lthres & percentage < uthres, "orange","green")),
                             label = paste0(format(round(percentage * 100, 1), nsmall = 1), "%"),
                             title = lbl)

  ggplot(df, aes(fill = group, ymax = min(percentage, 1), ymin = 0, xmax = 2, xmin = 1)) +
    geom_rect(aes(ymax = 1, ymin = 0, xmax = 2, xmin = 1), fill = "#dcdcdc") +
    geom_rect() +
    coord_polar(theta = "y", start = -pi/2) + xlim(c(0, 2)) + ylim(c(0,2)) +
    geom_text(aes(x = 0, y = 0, label = label, colour = group), size = 10 * scale) +
    geom_text(aes(x = 0.9, y = 1.5, label = title), size = 4.2 * (scale * (2 - scale))) +
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
#' @import ggplot2 scales ggrepel

generate_pie_chart <- function(df){

  # Create percentage labels
  df$percentage <- paste(round(df$value / sum(df$value) * 100, 1), " %")

  # Create the pie chart
  ggplot(df, aes(x = "", y = value, fill = group)) +
    geom_bar(stat="identity", width=1, color="white") +
    coord_polar("y", start=0) +
    scale_fill_brewer(palette = "Set1", name = "Group:") +
    ggrepel::geom_label_repel(data = df,
                              aes(label = percentage),
                              position = position_stack(vjust = 0.5),
                              max.overlaps = Inf,
                              size = 3,
                              segment.alpha = 0,
                              show.legend = FALSE) +
    theme_void() # remove background, grid, numeric labels

}

#' Create a daily bar plot
#'
#' generate_day_bar_plot() creates an absolute or a relative bar plot.
#'
#' @param date_vec Vector containing dates
#' @param date_min Start date of the plot
#' @param date_max End date of the plot
#' @param ylim Numeric vector of length 2 that contains the min and max values of the y-axis (optional, default = NULL)
#' @param ylbl String of the y-axis (optional, default = NULL)
#' @param rref reference value (optional, default = NULL)
#' @param relative Boolean for plotting relative/absolute plots (optional, default = FALSE)
#' @param date_vec_ref Vector containing reference dates (optional, default = NULL)
#' @return This function returns a ggplot object which contains a bar plot of frequencies by dates
#' @export
#' @import ggplot2 scales dplyr

generate_day_bar_plot <- function(date_vec,
                                  date_min,
                                  date_max,
                                  ylim = NULL,
                                  ylbl = "Frequencies",
                                  rref = NULL,
                                  relative = FALSE,
                                  date_vec_ref = NULL){

  # Count elements
  counts <- aggregate(date_vec, by = list(date_vec), FUN = length)
  if (relative) {
    counts$x <- counts$x / rref
  }
  counts$names <- as.Date(counts$Group.1, format = "%Y-%m-%d")

  p <- ggplot2::ggplot(data = counts,
                       mapping = aes(x = names, y = x)) +
    ggplot2::geom_bar(stat = "identity",
                      fill = "#7dbbd6",
                      width = 1) +
    ylab(ylbl) +
    xlab("Date") +
    ggplot2::scale_x_date(limits = c(date_min, date_max),
                          breaks = "1 day",
                          date_labels = "%d.%m.%y") +
    ggplot2::theme(panel.border = element_blank(),
                   panel.grid.major.x = element_blank(),
                   panel.grid.minor.x = element_blank(),
                   panel.grid.major.y = element_line(color = "grey60",
                                                     linetype = "dashed"),
                   panel.grid.minor.y = element_line(color = "grey",
                                                     linetype = "dashed"),
                   axis.line = element_blank(),
                   panel.background = element_blank(),
                   axis.text.x = element_text(angle = 45, hjust = 1))

  if (!is.null(date_vec_ref)) {
    # Count elements
    counts_ref <- aggregate(date_vec_ref, by = list(date_vec_ref), FUN = length)
    if (relative) {
      counts_ref$x <- counts_ref$x / rref
    }
    counts_ref$names <- as.Date(counts_ref$Group.1, format = "%Y-%m-%d")

    counts_ref <- counts_ref %>%
      tidyr::complete(names = seq.Date(date_min, date_max, by="day"))
    counts_ref$x[is.na(counts_ref$x)] <- 0

    p <- p + ggplot2::geom_step(data = counts_ref,
                                mapping = aes(x = names, y = x),
                                color = "gray30",
                                size = 0.5,
                                stat = "identity",
                                direction = "mid")

  }

  if (relative) {
    if (!is.null(ylim)) {
      p <- p + scale_y_continuous(labels = percent, limits = ylim)
    } else{
      p <- p + scale_y_continuous(labels = percent)
    }
    p <- p + geom_hline(yintercept = 1, linetype = "dashed")
  } else{
    if (!is.null(ylim)) {
      p <- p + scale_y_continuous(limits = ylim)
    }
    if (!is.null(rref)) {
      p <- p +
        geom_hline(yintercept = rref, linetype = "dashed") +
        annotate("text",
                 x = as.Date(date_max),
                 y = 1.05 * rref,
                 label = rref,
                 fontface = "bold")
    }
  }

  p

}

#' Create a weekly bar plot
#'
#' generate_week_bar_plot() creates an absolute or a relative bar plot.
#'
#' @param date_vec Vector containing dates.
#' @param date_min Start date of the plot.
#' @param date_max End date of the plot.
#' @param ylim Numeric vector of length 2 that contains the min and max values of the y-axis (optional, default = NULL).
#' @param ylbl String of the y-axis.
#' @param rref reference value (optional, default = NULL).
#' @param relative Boolean for plotting relative/absolute plots.
#' @param date_vec_ref Vector containing reference dates (optional, default = NULL).
#' @return This function returns a ggplot object which contains a bar plot of frequencies by week.
#' @export
#' @import ggplot2 scales

generate_week_bar_plot <- function(date_vec,
                                   date_min,
                                   date_max,
                                   ylim = NULL,
                                   ylbl = "Frequencies",
                                   rref = NULL,
                                   relative = FALSE,
                                   date_vec_ref = NULL){

  # Convert to week
  week_vec <- as.Date(cut(as.Date(date_vec),
                          breaks = "week",
                          start.on.monday = TRUE))
  wdate_min <- as.Date(cut(as.Date(date_min - 7),
                           breaks = "week",
                           start.on.monday = TRUE))
  # Count elements
  counts <- aggregate(week_vec, by = list(week_vec), FUN = length)
  if (relative) {
    counts$x <- counts$x / rref
  }
  counts$week <- as.Date(counts$Group.1, format = "%Y-%m-%d")

  p <- ggplot(counts, aes(x = week, y = x)) +
    geom_bar(stat = "identity",
             fill = "#7dbbd6",
             width = 7) +
    ylab(ylbl) +
    xlab("Date") +
    ggplot2::scale_x_date(limits = c(wdate_min, date_max),
                          breaks = "1 week",
                          date_labels = "%d.%m.%y") +
    theme(panel.border = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line(color = "grey60",
                                            linetype = "dashed"),
          panel.grid.minor.y = element_line(color = "grey",
                                            linetype = "dashed"),
          axis.line = element_blank(),
          panel.background = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1))

  if (!is.null(date_vec_ref)) {
    # Convert to week
    week_vec_ref <- as.Date(cut(as.Date(date_vec_ref),
                            breaks = "week",
                            start.on.monday = TRUE))

    # Count elements
    counts_ref <- aggregate(week_vec_ref, by = list(week_vec_ref), FUN = length)
    if (relative) {
      counts_ref$x <- counts_ref$x / rref
    }
    counts_ref$week <- as.Date(counts_ref$Group.1, format = "%Y-%m-%d")

    counts_ref <- counts_ref %>%
      tidyr::complete(week = seq.Date(date_min, date_max, by="week"))
    counts_ref$x[is.na(counts_ref$x)] <- 0

    p <- p + ggplot2::geom_step(data = counts_ref,
                                mapping = aes(x = week, y = x),
                                color = "gray30",
                                size = 0.5,
                                stat = "identity",
                                direction = "mid")

  }

  if (relative) {
    if (!is.null(ylim)) {
      p <- p + scale_y_continuous(labels = percent, limits = ylim)
    } else{
      p <- p + scale_y_continuous(labels = percent)
    }
    p <- p + geom_hline(yintercept = 1, linetype = "dashed")
  } else{
    if (!is.null(ylim)) {
      p <- p + scale_y_continuous(limits = ylim)
    }
    if (!is.null(rref)) {
      p <- p +
        geom_hline(yintercept = rref, linetype = "dashed") +
        annotate("text",
                 x = as.Date(date_max),
                 y = 1.05 * rref,
                 label = rref,
                 fontface = "bold")
    }
  }

  p

}

#' Create a weekly bar plot by adding values from a specific column
#'
#' generate_week_bar_plot() creates an absolute or a relative bar plot.
#'
#' @param date_vec Vector containing dates
#' @param values Vector containing values to sum
#' @param date_min Start date of the plot
#' @param date_max End date of the plot
#' @param ylim Numeric vector of length 2 that contains the min and max values of the y-axis
#' @param ylbl String of the y-axis
#' @return This function returns a ggplot object which contains a bar plot of frequencies by week
#' @export
#' @import ggplot2 scales

generate_week_bar_plot2 <- function(date_vec, values, date_min, date_max, ylim = NULL, ylbl = "Values"){

  # Convert to week
  week <- as.Date(cut(as.Date(date_vec),
                      breaks = "week",
                      start.on.monday = TRUE))

  wdate_min <- as.Date(cut(as.Date(date_min - 7),
                           breaks = "week",
                           start.on.monday = TRUE))

  df <- data.frame(week, values)

  p <- ggplot2::ggplot(df, aes(x = week, y = values)) +
    geom_bar(stat = "identity", fill = "#7dbbd6", width = 7)  +
    ylab(ylbl) +
    xlab("Date") +
    ggplot2::scale_x_date(limits = c(wdate_min, date_max),
                          breaks = "1 week",
                          date_labels = "%d.%m.%y") +
    theme(panel.border = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line(color = "grey60",
                                            linetype = "dashed"),
          panel.grid.minor.y = element_line(color = "grey",
                                            linetype = "dashed"),
          axis.line = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.background = element_blank())
  p

}

#' Create a cumulative daily bar plot
#'
#' generate_day_cumbar_plot() creates a bar plot.
#'
#' @param date_vec_list List of vectors containing dates
#' @param lbl_vec Vector of strings containing the name of each group
#' @param date_min Start date of the plot
#' @param date_max End date of the plot
#' @param ylim Numeric vector of length 2 that contains the min and max values of the y-axis
#' @param ylbl String of the y-axis
#' @return This function returns a ggplot object which contains a cumulative bar plot of frequencies by dates
#' @export
#' @import ggplot2 scales dplyr
#' @importFrom stats aggregate

generate_day_cumbar_plot <- function(date_vec_list, lbl_vec, date_min, date_max, ylim = NULL, ylbl = "Frequencies"){

  tmp = list()
  lbl_idx <- numeric(0)
  for (i in 1:length(date_vec_list)) {
    vec <- date_vec_list[[i]]
    # Count elements
    if (length(vec) > 0) {
      c <- aggregate(vec, by = list(vec), FUN = length)
      c$names <- as.Date(c$Group.1, format = "%Y-%m-%d")
      c$type <- lbl_vec[i]
      tmp[[i]] <- c
    } else{
      lbl_idx <- c(lbl_idx, i)
    }
  }

  if (length(lbl_idx) > 0) {
    lbl_vec <- lbl_vec[-lbl_idx]
  }
  counts <- dplyr::bind_rows(tmp)

  p <- ggplot(counts, aes(x = names, y = x, fill = factor(type, levels = lbl_vec))) +
    geom_bar(stat = "identity", width = 1) +
    ylab(ylbl) +
    xlab("Date") +
    scale_x_date(limits = c(date_min, date_max),
                 breaks = "1 day",
                 date_labels = "%d.%m.%y") +
    theme(panel.border = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line(color = "grey60",
                                            linetype = "dashed"),
          panel.grid.minor.y = element_line(color = "grey",
                                            linetype = "dashed"),
          axis.line = element_blank(),
          legend.title = element_blank(),
          panel.background = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom")

  if (!is.null(ylim)) {
    p <- p + scale_y_continuous(limits = ylim)
  }

  p

}

#' Generate an histogram of day times
#'
#' @param df Dataframe
#' @param title Title
#' @return This function returns an histogram of the distribution of data over day times
#' @import ggplot2 graphics
#' @export

generate_time_hist_plot <- function(df, title){

  df$timestamp <- strptime(df$start_time, format = "%H:%M:%S")
  df$hours <-  as.numeric(format(df$timestamp, format = "%H"))
  breaks <- seq(0,23,1)
  graphics::hist(df$hours,
                 main = title,
                 xlab = "Times",
                 border = "#7dbbd6",
                 col = "#7dbbd6",
                 las = 1,
                 breaks = breaks)

  # df$timestamp <- strptime(df$start_time, format = "%H:%M:%S")
  # df$hours <- as.numeric(format(df$timestamp, format = "%H"))
  # p <- ggplot(df, aes(x=df$hours)) +
  #   geom_histogram(binwidth = 24,
  #                  color = "#7dbbd6",
  #                  fill = "#7dbbd6")

}

#' Plot string in image
#'
#' @param string String to be converted to an image
#' @return This function returns a plot
#' @import graphics
#' @export

text_2_plot <- function(string){

  plot(c(0, 1), c(0, 1), ann = FALSE, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
  graphics::text(x = 0.5, y = 0.5, paste(string), cex = 4, col = "black", family = "serif", font = 20, adj = 0.5)

  # mo_plot <- shelters %>%
  #   group_by(date) %>%
  #   summarise(month_occupancy = sum(occupancy) / 1000) %>%
  #   mutate(date = parse_date_time(date, "ym")) %>%
  #   ggplot(aes(date, month_occupancy)) +
  #   geom_point(size = 2, color = "#371206") +
  #   geom_smooth(se = FALSE, color = "#F72C25", size = 1.5) +
  #   labs(x = NULL, y = "Monthly Occupancy", subtitle = "**<span style='color:#F72C25;font-size:60px;'>50%</span><br>increase in monthly shelter occupancy<br>from Jan 2017 to Dec 2019.**") +
  #   theme(text = element_text(family = my_font),
  #         plot.subtitle = element_markdown(color = "#292929"),
  #         plot.background = element_rect("#EBEBEB"),
  #         panel.background = element_blank(),
  #         axis.title = element_text(color = "#292929", face = "bold"),
  #         axis.text = element_text(color = "#292929", face = "bold"),
  #         panel.grid = element_blank())

}

#' Create a numeric indicator
#'
#' plot_numeric_indicator() creates an image that contains a numeric indicator as well as a short description of the variable it represents.
#'
#' @param val Value
#' @param lbl Label
#' @param scale Numeric value (optional, default 1), must vary between 0 and 1 to scale the text size
#' @return This function returns a ggplot2 object.
#' @import ggplot2
#' @export

plot_numeric_indicator <- function(val, lbl, scale = 1){

  ggplot2::ggplot() +
    ggplot2::geom_rect(aes(xmin = 0, xmax = 1, ymin = 0, ymax = 1),
                       fill = "white") +
    ggplot2::annotate("text",
                      x = 0.5,
                      y = 0.7,
                      size = 10 * scale,
                      label = val,
                      colour = "#7dbbd6") +
    ggplot2::annotate("text",
                      x = 0.5,
                      y = 0.3,
                      size = 2.7 * scale,
                      label = lbl) +
    ggplot2::theme_void()

}

#' Create facets with bar plots over time
#'
#' plot_geom_bar_by_facility_over_time() creates an image that contains facets with bar plots over time.
#'
#' @param df Data frame to use for the plot.
#' @param facility_col Column name in data frame `df`.
#' @param date_col Column name in data frame `df`, should contain dates.
#' @param date_lbl String that contains the x-axis label for the plot.
#' @param date_break_str A string giving the distance between date breaks like "2 weeks", or "10 years".
#' @param date_format A string giving the date formatting specification for the date labels (for instance `\%b\%y`).
#' @param fill_col Column name in data frame `df`(optional, default `NULL`).
#' @param n_facet_per_row Numeric value (optional, default 5) to set the number of facets per rows.
#' @param text_size Numeric value (optional, default 7) to scale the text size.
#' @return This function returns a ggplot2 object.
#' @import ggplot2 dplyr
#' @export

plot_geom_bar_by_facility_over_time <- function(df,
                                                facility_col,
                                                date_col,
                                                date_lbl,
                                                date_break_str,
                                                date_format,
                                                fill_col = NULL,
                                                n_facet_per_row = 5,
                                                text_size = 7){

  # Quote the arguments that refer to data frame columns
  date_col <- rlang::sym(date_col)
  facility_col <- rlang::sym(facility_col)

  # handling NULL fill_col
  if ( !missing(fill_col) ) {
    fill_col <- rlang::sym(fill_col)
    p <- ggplot2::ggplot(df,
                         ggplot2::aes(x = !!date_col, fill = !!fill_col))
  } else{
    p <- ggplot2::ggplot(df,
                         ggplot2::aes(x = !!date_col))
  }

  p <- p + ggplot2::geom_bar() +
    ggplot2::labs(x = date_lbl,
                  y = "Number of records") +
    ggplot2::scale_x_date(date_breaks = date_break_str,
                          date_labels = date_format) +
    ggplot2::facet_wrap(ggplot2::vars(!!facility_col), ncol = n_facet_per_row) +
    ggplot2::theme(text = element_text(size = text_size),
                   panel.grid.major.x = element_blank(),
                   panel.grid.minor.x = element_blank())
  p

}
