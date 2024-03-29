% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/query_odkapi.R
\name{publish_odkc_draft_form}
\alias{publish_odkc_draft_form}
\title{Publish a draft form
See reference ODK API documentation in \url{https://odkcentral.docs.apiary.io/#reference/forms/draft-form/publishing-a-draft-form} for more details.}
\usage{
publish_odkc_draft_form(un, pw, form_url, new_id)
}
\arguments{
\item{un}{string containing the ODK Central username.}

\item{pw}{string containing the password for user `un`.}

\item{form_url}{string containing the URL of the ODK Central form to be published, e.g. "https://sandbox.central.getodk.org/v1/projects/project-id/forms/form-id".}

\item{new_id}{string containing the version of the form to be published. If `new_id` conflicts with an older version of the form, you will get an error.}
}
\value{
This function returns the content of the request to the ODK Central API.
}
\description{
Publish a draft form
See reference ODK API documentation in \url{https://odkcentral.docs.apiary.io/#reference/forms/draft-form/publishing-a-draft-form} for more details.
}
