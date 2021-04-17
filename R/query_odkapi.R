#' Create an ODK Central draft form
#'
#' @param un string containing the ODK Central username.
#' @param pw string containing the password for user `un`.
#' @param form_url string containing the URL of the ODK Central form to be published, e.g. "https://sandbox.central.getodk.org/v1/projects/project-id/forms/form-id".
#' @return This function returns the content of the request to the ODK Central API.
#' @export
#' @import dplyr magrittr

create_odkc_draft_form <- function(un, pw, form_url) {

  response <- httr::RETRY("POST",
                          paste0(form_url, "/draft"),
                          httr::authenticate(un, pw))
  httr::content(response)

}

#' Upload the new CSV created as a form attachment
#'
#' @param un string containing the ODK Central username.
#' @param pw string containing the password for user `un`.
#' @param form_url string containing the URL of the ODK Central form to be published, e.g. "https://sandbox.central.getodk.org/v1/projects/project-id/forms/form-id".
#' @param csv_path string containing the path to the CSV file to attach to the form.
#' @return This function returns the response of the request to the ODK Central API.
#' @export

upload_odkc_csv_attachment <- function(un, pw, form_url, csv_path) {

  csv_fn <- basename(csv_path)
  response <- httr::RETRY("POST",
                          paste0(form_url, "/draft/attachments/", csv_fn),
                          body = httr::upload_file(csv_path),
                          httr::authenticate(un, pw))
  httr::content(response)

}

#' Publish a draft form
#'
#' @param un string containing the ODK Central username.
#' @param pw string containing the password for user `un`.
#' @param form_url string containing the URL of the ODK Central form to be published, e.g. "https://sandbox.central.getodk.org/v1/projects/project-id/forms/form-id".
#' @param new_id string containing the version of the form to be published, to be written in its XForms XML definition.
#' @return This function returns the content of the request to the ODK Central API.
#' @export
#' @import dplyr magrittr

publish_odkc_draft_form <- function(un, pw, form_url, new_id) {

  response <- httr::RETRY("POST",
                          paste0(form_url, "/draft/publish?version=", new_id),
                          httr::authenticate(un, pw))
  httr::content(response)
}
