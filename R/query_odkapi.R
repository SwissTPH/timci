#' Create an ODK Central draft form
#' See reference ODK API documentation in \url{https://odkcentral.docs.apiary.io/#reference/forms/draft-form/creating-a-draft-form} for more details.
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
#' See reference ODK API documentation in \url{https://odkcentral.docs.apiary.io/#reference/forms/draft-form/uploading-a-draft-form-attachment} for more details.
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
#' See reference ODK API documentation in \url{https://odkcentral.docs.apiary.io/#reference/forms/draft-form/publishing-a-draft-form} for more details.
#'
#' @param un string containing the ODK Central username.
#' @param pw string containing the password for user `un`.
#' @param form_url string containing the URL of the ODK Central form to be published, e.g. "https://sandbox.central.getodk.org/v1/projects/project-id/forms/form-id".
#' @param new_id string containing the version of the form to be published. If `new_id` conflicts with an older version of the form, you will get an error.
#' @return This function returns the content of the request to the ODK Central API.
#' @export
#' @import dplyr magrittr

publish_odkc_draft_form <- function(un, pw, form_url, new_id) {

  response <- httr::RETRY("POST",
                          paste0(form_url, "/draft/publish?version=", new_id),
                          httr::authenticate(un, pw))
  httr::content(response)
}
