# TODO: Add comment
# 
# Author: Michael
###############################################################################

getskew <- function(dat) {
  # determine whether there is an identifiable skew in the data
  # dat: a vector of data points
  # returns NA, 'left', or 'right': 
  #  'left' means the data is skewed with a tail to the right
  #  'right' means the data is skewed with a tail to the left
  #   NA means data is approximately symmetrical
  # get the lower and higher quartiles and eigths
  m <- median(dat)
  lq <- median(dat[dat < m])
  uq <- median(dat[dat > m])
  le <- median(dat[dat < lq])
  ue <- median(dat[dat > uq])
  # following Tukey’s Exploratory Data Analysis
  mq <- mean(c(uq, lq))
  me <- mean(c(ue, le))
  if(me > mq && mq > m){
    getskew <- 1
  } else if(me < mq && mq < m){
    getskew <- -1
  }
  else{
    getskew <- 0
  }
}