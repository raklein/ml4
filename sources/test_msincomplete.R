# helper function for creating msincomplete variable using in-house text responses
# feed it the two MS-prompt columns and two control-prompt columns
#    and specify minimum response length to be considered complete

test_msincomplete <- function(ms1, ms2, control1, control2, minchar = 10) {
  test1 <- (nchar(ms1) < minchar) & (nchar(control1) < minchar) # insufficient response to 1st prompt
  test2 <- (nchar(ms2) < minchar) & (nchar(control2) < minchar) # insufficient response to 2nd prompt
  result <- ifelse(test1 | test2, 1, 0) # insufficent response to either gets a flag
  return(result)
}