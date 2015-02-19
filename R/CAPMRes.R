#' CAPM results
#' @param df data.frame With mutual fund returns and market benchmark
#' @param rm character Name of the market benchmark
#' @return A data.frame with CAPM results
#' @export
CAPMRes <- function(df, rm, siglevel = 0.05, Newey = TRUE){
  funds <- str_detect(colnames(df), "Aktier")  %>%  which
  res <- lapply(funds, function(i){
    fm <- as.formula(paste(paste(colnames(df)[i]), paste("1", rm, sep = "+"), sep=" ~ "))
    model <- lm(fm, df)
    sumlm <- summary.lm(model)
      NeweyWest(model)
      NW_std <- coeftest(model, vcov. = NeweyWest(model))
    return(data.frame("alpha" = sumlm$coefficients[1,1],
                      "beta" = sumlm$coefficients[2,1],
                      "std_alpha" = ifelse(Newey == TRUE, NW_std[1,2], sumlm$coefficients[1,2]),
                      "std_beta" = ifelse(Newey == TRUE, NW_std[2,2], sumlm$coefficients[2,2]),
                      "t_alpha" = ifelse(Newey == TRUE, NW_std[1,3], sumlm$coefficients[1,3]),
                      "t_beta" = ifelse(Newey == TRUE, NW_std[2,3], sumlm$coefficients[2,3]),
                      "p_alpha" = ifelse(Newey == TRUE, NW_std[1,4], sumlm$coefficients[1,4]),
                      "p_beta" = ifelse(Newey == TRUE, NW_std[2,4], sumlm$coefficients[2,4]),
                      "R2_adj" = sumlm$adj.r.squared,
                      "noalphasiglevel" = ifelse(sumlm$coefficients[1,4] <= siglevel, 1, 0),
                      "nobetasiglevel" = ifelse(sumlm$coefficients[2,4] <= siglevel, 1, 0)))
  }) %>% rbind_all
  return(res)
}