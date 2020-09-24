#' Load dummy data
#'
#' This function loads dummy data.
#'
#' Description of load_dummy_data
#' @return This function processes ODK data.
#' @export
#' @import hash readxl

load_dummy_data <- function(){

  facilities_xls <- system.file("extdata", "facilities.xlsx", package = "timci")
  data_xls <- system.file("extdata", "dummy_data.xlsx", package = "timci")

  # Retrieves toy data from Excel files
  facilities_info <- readxl::read_excel(facilities_xls, col_names = TRUE)
  tdata <- hash::hash()
  mydata <- hash::hash()
  facilities <- hash::hash()
  sheetlist =list("Arm I CDSA POX", "Arm II POX", "Arm III Control")

  for(s in sheetlist){
    tdata[[s]] <- readxl::read_excel(data_xls, sheet = s, col_names = TRUE)
    facilities[[s]] <- list(tdata[[s]]$facility)

    mydata[[s]] <- tdata[[s]]
    mydata[[s]]$date <- lapply(mydata[[s]]$date, scales::date_format("%Y-%m-%d"))
    mydata[[s]]$age_in_months <- as.integer(mydata[[s]]$age_in_months)
    mydata[[s]]$screening_id <- as.integer(mydata[[s]]$screening_id)
    mydata[[s]]$eligible <- as.integer(mydata[[s]]$eligible)
    mydata[[s]]$enrolled <- as.integer(mydata[[s]]$enrolled)
    mydata[[s]]$consent <- as.integer(mydata[[s]]$consent)
    mydata[[s]]$referred <- as.integer(mydata[[s]]$referred)
    mydata[[s]]$'7_day_followup' <- as.integer(mydata[[s]]$'7_day_followup')
    mydata[[s]]$'28_day_followup' <- as.integer(mydata[[s]]$'28_day_followup')
  }

  output <- hash::hash()
  output[['data']] <- mydata

  df1 <- aggregate(mydata[["Arm I CDSA POX"]]$enrolled, by=facilities[["Arm I CDSA POX"]], FUN=sum)
  df2 <- aggregate(mydata[["Arm II POX"]]$enrolled, by=facilities[["Arm II POX"]], FUN=sum)
  df3 <- aggregate(mydata[["Arm III Control"]]$enrolled, by=facilities[["Arm III Control"]], FUN=sum)
  df <- do.call("rbind", list(df1, df2, df3))
  output[['df']] <- df

  return(output)

}
