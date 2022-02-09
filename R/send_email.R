#' Send email
#'
#' This function sends an automated email with the generated reports
#'
#' @param study_type String containing the type of study to specific the email subject
#' @param dest Vector of strings containing email addresses
#' @param attachment_list Vector containing the paths to the files to be attached to the email
#' @import magrittr
#' @export

send_email <- function(study_type, dest, attachment_list) {

  if (!requireNamespace("emayili", quietly = TRUE)) {
    stop("Package \"emayili\" needed for this function to work. Please install it.",
         call. = FALSE)
  } else {
    email <- emayili::envelope() %>%
      emayili::from(Sys.getenv('EMAIL_UN')) %>%
      emayili::to(dest) %>%
      emayili::subject(paste0("TIMCI ", study_type, " daily reports - ", Sys.getenv('TIMCI_COUNTRY'), " - ", format(Sys.time(), '%Y-%m-%d'))) %>%
      emayili::text(paste0("Dear all,\n\nPlease receive the TIMCI daily reports for ",
                           Sys.getenv('TIMCI_COUNTRY'),
                           ".\n\nBest regards,\n\nThe TIMCI team\n\n[THIS IS AN AUTOMATED MESSAGE - PLEASE DO NOT REPLY DIRECTLY TO THIS EMAIL]"))

    smtp <- emayili::server(host = "smtp.gmail.com",
                            port = 465,
                            username = Sys.getenv('EMAIL_UN'),
                            password = Sys.getenv('EMAIL_PW'))

    for (atch in attachment_list) {
      if (file.exists(atch)) {
        email <- email %>%
          emayili::attachment(atch)
      }
    }

    smtp(email, verbose = TRUE)
  }

}

#' Send email
#'
#' This function sends an automated email with the generated reports
#'
#' @param study_type String containing the type of study to specific the email subject
#' @param dest Vector of strings containing email addresses
#' @param attachment_list Vector containing the paths to the files to be attached to the email
#' @import magrittr
#' @export

send_weekly_email <- function(study_type, dest, attachment_list) {

  if (!requireNamespace("emayili", quietly = TRUE)) {
    stop("Package \"emayili\" needed for this function to work. Please install it.",
         call. = FALSE)
  } else {
    email <- emayili::envelope() %>%
      emayili::from(Sys.getenv('EMAIL_UN')) %>%
      emayili::to(dest) %>%
      emayili::subject(paste0("TIMCI ", study_type, " weekly reports - ", Sys.getenv('TIMCI_COUNTRY'), " - Week from ", format(as.Date(Sys.time())-4, '%Y-%m-%d'), " to ", format(Sys.time(), '%Y-%m-%d'))) %>%
      emayili::text(paste0("Dear all,\n\nPlease receive the TIMCI weekly reports for ",
                           Sys.getenv('TIMCI_COUNTRY'),
                           ".\n\nBest regards,\n\nThe TIMCI team\n\n[THIS IS AN AUTOMATED MESSAGE - PLEASE DO NOT REPLY DIRECTLY TO THIS EMAIL]"))

    smtp <- emayili::server(host = "smtp.gmail.com",
                            port = 465,
                            username = Sys.getenv('EMAIL_UN'),
                            password = Sys.getenv('EMAIL_PW'))

    for (atch in attachment_list) {
      if (file.exists(atch)) {
        email <- email %>%
          emayili::attachment(atch)
      }
    }

    smtp(email, verbose = TRUE)
  }

}

#' Send email
#'
#' This function sends an automated email with the generated reports.
#' The gmail() function is only available for emayili v0.6.5 and above.
#' It uses by default port 587 (TLS), but the port argument can be specified (e.g., port 465 for SSL).
#'
#' @param study_type String containing the type of study to specific the email subject
#' @param dest Vector of strings containing email addresses
#' @param attachment_list Vector containing the paths to the files to be attached to the email
#' @import magrittr
#' @export

send_email_tls <- function(study_type, dest, attachment_list) {

  if (!requireNamespace("emayili", quietly = TRUE)) {
    stop("Package \"emayili\" needed for this function to work. Please install it.",
         call. = FALSE)
  } else {
    email <- emayili::envelope() %>%
      emayili::from(Sys.getenv('EMAIL_UN')) %>%
      emayili::to(dest) %>%
      emayili::subject(paste0("TIMCI ", study_type, " reports - ", Sys.getenv('TIMCI_COUNTRY'), " - ", format(Sys.time(), '%Y-%m-%d'))) %>%
      emayili::text(paste0("Dear all,\n\nPlease receive the TIMCI daily report for ",
                           Sys.getenv('TIMCI_COUNTRY'),
                           ".\n\nBest regards,\n\nThe TIMCI team\n\n[THIS IS AN AUTOMATED MESSAGE - PLEASE DO NOT REPLY DIRECTLY TO THIS EMAIL]"))

    smtp <- emayili::gmail(username = Sys.getenv('EMAIL_UN'),
                           password = Sys.getenv('EMAIL_PW'))

    for (atch in attachment_list) {
      if (file.exists(atch)) {
        email <- email %>%
          emayili::attachment(atch)
      }
    }

    smtp(email, verbose = TRUE)
  }

}
