#' Send email
#'
#' This function sends an automated email with the generated reports
#'
#' @param report_dir Path to the folder where the generated HTML and Microsoft Word DOCX Rmarkdown reports are stored
#' @param report_fn Filename of the Rmarkdown rendered report
#' @param dest vector of strings containing email addresses
#' @import emayili magrittr
#' @export

send_email <- function(report_dir, report_fn, dest) {

  filename <- paste0(report_fn, '_',Sys.Date(),'.docx')

  email <- emayili::envelope() %>%
    emayili::from(Sys.getenv('EMAIL_UN')) %>%
    emayili::to(dest) %>%
    emayili::subject(paste0("TIMCI reports - ", Sys.getenv('TIMCI_COUNTRY'), " - ", format(Sys.time(), '%Y-%m-%d')))

  smtp <- server(host = "smtp.gmail.com",
                 port = 465,
                 username = Sys.getenv('EMAIL_UN'),
                 password = Sys.getenv('EMAIL_PW'))

  email <- email %>%
    emayili::attachment(path = c(file.path(report_dir, filename)))

  smtp(email, verbose = TRUE)

}
