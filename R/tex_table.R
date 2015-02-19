#'Creates a .tex table
#'@param df data.frame To be turned into table
#'@param dig numeric Number of digits to include in table
#'@param out.file Filename for the table. Will be placed in folder called
#'  "Plots" two leves up from wd
#'@param notes Character vector to be included as footnote in table. Will accept
#'  LaTeX style formatting.
#'@export
#'@return NULL
tex_table <-
  function(df, dig, out.file, align = NULL, notes = NULL, sig_markers = FALSE) {
  plot_folder <- file.path(getwd(),"../../Plots")

  digit <- replicate(ncol(df), 0)
  digit[str_detect(colnames(df), "\\<alpha|\\<beta|alpha\\>|beta\\>") %>% which] <- dig
  digit <- c(0,digit)

  if(sig_markers == TRUE)
  {
    df %<>% mutate(alpha = alpha %>%
    {round(.,dig) %>%
    {ifelse(alpha/t_alpha < 0.01, paste0(., "\\tnote{a}"),
            ifelse(alpha/t_alpha >= 0.01 & alpha/t_alpha < 0.05, paste0(., "\\tnote{b}"),
                   ifelse(alpha/t_alpha >= 0.05  & alpha/t_alpha < 0.10, paste0(., "\\tnote{c}"),
                          paste0(.," "))))
    }
    }
    )
  }

  if(sig_markers == TRUE)
  {
      df %<>% mutate(beta = beta %>%
      {round(.,dig) %>%
      {ifelse(beta/t_beta < 0.01, paste0(., "\\tnote{a}"),
              ifelse(beta/t_beta >= 0.01 & beta/t_beta < 0.05, paste0(., "\\tnote{b}"),
                     ifelse(beta/t_beta >= 0.05  & beta/t_beta < 0.10, paste0(., "\\tnote{c}"),
                            paste0(.," "))))
      }
      }
      )
  }

  actual_notes <-
    if(is.null(notes))
    {
     "Testing a long line. What about mathematics? $\\\\alpha = 2+2+3$"
    }else{
      notes
    }

  align <-
    if(is.null(align))
    {
      paste("ll",paste(replicate((ncol(df)-1), "c"), collapse = ""), sep ="")
    }else{
      align
    }

  footnote  <-
    paste("\n",
      "\\\\end{tabular}
      \\\\begin{tablenotes}[flushleft]
      \\\\item\\\\scriptsize\\\\textit{Notes:}"
      ,actual_notes,"
      \\\\vspace{5pt}"
      ,
      ifelse(sig_markers==TRUE, paste("\\\\tiny \\\\item[a] Significant at the 1\\\\% level.
                                      \\\\tiny \\\\item[b] Significant at the 5\\\\% level.
                                      \\\\tiny \\\\item[c] Significant at the 10\\\\% level."), "")
      ,
      "\\\\end{tablenotes}
      \n",
      sep = "\n")

  colnames(df) %<>% str_replace("\\balpha", "$\\\\alpha$") %>%
    str_replace("\\bbeta", "$\\\\beta$") %>%
    str_replace_all("t_alpha", "$t(\\\\alpha)$") %>%
    str_replace_all("t_beta", "$t(\\\\beta)$") %>%
    str_replace_all("std_alpha", "$\\\\sigma^2_\\\\alpha$") %>%
    str_replace_all("std_beta", "$\\\\sigma^2_\\\\beta$") %>%
    str_replace_all("p_alpha", "$Pr(>|\\\\alpha|)$") %>%
    str_replace_all("p_beta", "$Pr(>|\\\\beta|)$") %>%
    str_replace_all("OBS", "$n$") %>%
    str_replace_all("average_alpha_stat", "$sig(\\\\alpha)$") %>%
    str_replace_all("average_beta_stat", "$sig(\\\\beta)$")


  xtable(df, digits = digit, align = align) %>%
  print(
    print.results = FALSE,
    type = "latex",
    include.rownames = FALSE,
    booktabs = TRUE,
    comment = FALSE,
    sanitize.text.function = function(x){x},
    floating.environment = "threeparttable",
    size = "small",
    hline.after = c(-1,-1,0,nrow(.),nrow(.))) %>%
      str_replace("[\\]end[{]tabular[}]", footnote) %>%
      write(file = file.path(plot_folder, out.file))
}
