colFmt <- function(x, color = 'red'){

  require(knitr)
  
  # http://stackoverflow.com/questions/29067541/rmarkdown-how-to-change-the-font-color
  x <- as.character(x)
  outputFormat <- opts_knit$get("rmarkdown.pandoc.to")
  if(outputFormat == 'latex')
    paste("\\textcolor{", color, "}{", x, "}", sep = "")
  else if(outputFormat == 'html')
    paste("<font color='", color, "'>", x, "</font>", sep = "")
  else
    x
}
