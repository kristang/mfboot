#' Calculate equal weighted portfolio
#' @param df data.frame With returns to be used in creating an equal-weighted
#'   return
#' @param rm A character containing the market return
#' @return data.frame Returns a data.frame with dates and the equal-weighted
#'   returns
#' @export
equal_weight <- function(df, rm){
  funds <- str_detect(colnames(df), "Aktier")  %>%  which %>%
    colnames(df)[.]
  res <- mutate_each_(df, funs(equal_weighted = .*1/length(funds)),
                      (paste(funds, sep = "\n")))
  weighted_returns <-
    data.frame("dates" = df$dates,
               "weighted_return" = rowSums(res[,funds], na.rm = TRUE),
               rm = df[,paste(rm)])
  return(weighted_returns)
}