
### Opens a data frame directly in Excel (on windows, with office)

## OJO
## Check out what happens if text fields contain "  with opening.. ie check out the character separators and 
## end of lines, comment characters, same as when read.table... 

## Make the useName thing automatic to recognise a name that won't be able to open and not use it. Or get rid...
# eg w.xls(cbind(1:10, 1:10), useName = T) # won't open

w.xls <- function(x, row=FALSE, useName = F, ...){ # , encoding = "UTF-8" - can add encoding if needed
  #x is a data frame
  # row is T/F whether to include row names in write.table call
  
  # include the object name in the tmp file name
  tmp <- tempfile(pattern = ifelse(useName, paste0(deparse(substitute(x)), "_"), "file"), fileext=".txt") # create temp text file
  ifelse(row==T, col <- NA, col <- TRUE)
  write.table(x, tmp, sep="\t", row.names=row, col.names=col, ...) # write the table to it. #,fileEncoding = encoding
  
  shell(paste("start", "excel", tmp)) # open in excel
  return(tmp)
}

# eg
# df <- data.frame(x = rnorm(15), y = runif(15), z = letters[1:15])
# df
# w.xls(df)