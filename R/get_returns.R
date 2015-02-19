#'Loads Mutual Fund data from folder named Data
#'@export
#'@return data.frame with mutual fund data

get_returns <- function (transposed = TRUE) {
  data.folder <- file.path(getwd(),"../../","Data/csv_files")

  raw.df <- read.table(file.path(data.folder, "raw_data_IFR2.csv"),
                       sep = ";",header = TRUE, as.is=TRUE, check.names = TRUE)

  categories <- unique(gsub(" ", "_", as.character(raw.df[1,-1]), fixed = TRUE))
  colnames(raw.df) <- c("dates",paste(gsub(" ", "_", as.character(raw.df[1,-1]), fixed = TRUE),
                                      gsub(".", "_", colnames(raw.df)[-1], fixed = TRUE), sep="_"))

  df <- slice(raw.df, -1) %>% mutate(dates = as.Date(dates, "%d-%m-%Y"))
  df[,2:ncol(df)] <- sapply(df[,2:ncol(df)],as.numeric)

  market_rates <- read.table(file.path(data.folder, "market_rates.csv"),
                             sep = ";",header = TRUE,
                             as.is=TRUE, check.names = TRUE)  %>%
    mutate(dates = as.Date(Date, "%d-%m-%Y")) %>%
    select(dates, contains("rate"))

  risk_free <- read.table(file.path(data.folder, "risk_free_rates.csv"),
                          sep = ";",header = TRUE,
                          as.is=TRUE, check.names = TRUE)  %>%
    mutate(dates = as.Date(Date, "%d-%m-%Y")) %>%
    select(dates, EURIBOR, TBILL,CIBOR)

  data <- inner_join(df, market_rates, by = "dates") %>%
    inner_join(risk_free, by = "dates")

  if(transposed == TRUE)
  {
    data_trans <- as.data.frame(t(data[,-1]))
    colnames(data_trans) <- data$dates
    data_trans <- cbind("Fund" = row.names(data_trans),data_trans)
    row.names(data_trans) <- NULL
    return(list(data,data_trans))
  }
  else
  {
    return(data)
  }

}
