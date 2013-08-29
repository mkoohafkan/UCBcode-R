# process_fogrun.r
process_fogrun <- function(path){
# function processes data from an individual run, formatted like:
# <trial name>
# <leaf area>
# STARTING BASE VOLTAGE
# ...
# ...
# STARTING CALIBRATION VOLTAGE
# ...
# ...
# DRY LEAF VOLTAGE
# ...
# ...
# BEGIN RUN
# ...
# ...
# ...
# WET LEAF VOLTAGE
# ...
# ...
# DRY RAG VOLTAGE
# ...
# ...
# WET RAG VOLTAGE
# ...
# ...
# LAST LWS VOLTAGE
# .
# MAX LWS VOLTAGE
# .
# ENDING BASE VOLTAGE
# ...
# ...
# ENDING CALIBRATION VOLTAGE
# ...
# ...
# <blank line>
# (.) is a single number, e.g. the voltage read by the LWS
# (...) is tab delimited data copied directly from the raw data files
# produced by CR1000. columns are: 
# timestamp, record number, LWS voltage, load cell voltage
  # helper functions
  getmatch <- function(tag, dat, stopifmissing=TRUE){
  # get indices for data components
    idx <- match(tag, dat)
    if(is.na(idx)){
      if(stopifmissing){
        stop(paste(tag, 'is missing.'))
      } else {
        warning(paste(tag, 'is missing.'))
      }
    }
    return(idx)
  }
  scandata <- function(dat, idx){
  # pull numeric values 
    values <- NULL
    n <- idx
    while(!is.na(as.numeric(substr(dat[n], 1, 1)))){
      values <- rbind(values, unlist(strsplit(dat[n], '\t')))
      n <- n + 1
    }
    return(data.frame(timestamp=strptime(values[, 1], "%m/%d/%Y %H:%M:%S"), 
                      record=as.integer(values[, 2]), 
                      LWSmV=as.numeric(values[, 3]),
                      SEmV=as.numeric(values[, 4])))
  }  
  # read lines from file
  datalines <- readLines(path)
  # trial name should be first line
  name <- datalines[1]
  # leaf area should be second line, in cm^2
  leafarea <- as.numeric(datalines[2])
  # indices for data processing
  startbaseidx <- getmatch('STARTING BASE VOLTAGE', datalines) + 1
  endbaseidx <- getmatch('ENDING BASE VOLTAGE', datalines) + 1
  startcalibrationidx <- getmatch('STARTING CALIBRATION VOLTAGE', datalines) + 1
  endcalibrationidx <- getmatch('ENDING CALIBRATION VOLTAGE', datalines, FALSE) + 1
  dryleafidx <- getmatch('DRY LEAF VOLTAGE', datalines) + 1
  wetleafidx <- getmatch('WET LEAF VOLTAGE', datalines) + 1
  dryragidx <- getmatch('DRY RAG VOLTAGE', datalines) + 1
  wetragidx <- getmatch('WET RAG VOLTAGE', datalines) + 1
  startrunidx <- getmatch('BEGIN RUN', datalines) + 1
  lastLWSidx <- getmatch('LAST LWS VOLTAGE', datalines) + 1
  maxLWSidx <- getmatch('MAX LWS VOLTAGE', datalines) + 1
  # get start and end base voltages
  sbv <- scandata(datalines, startbaseidx)
  ebv <- scandata(datalines, endbaseidx)
  # get dry and wet rag voltages
  drv <- scandata(datalines, dryragidx)
  wrv <- scandata(datalines, wetragidx)
  # get dry and wet leaf voltages
  dlv <- scandata(datalines, dryleafidx)
  wlv <- scandata(datalines, wetleafidx)
  # get start (and end) calibration voltages
  scv <- scandata(datalines, startcalibrationidx)
  if(!is.na(endcalibrationidx)){ 
    ecv <- scandata(datalines, endcalibrationidx)
  } else {
    ecv <- NA
  }
  # get LWS voltages
  lwsvoltlast <- as.numeric(datalines[lastLWSidx])
  lwsvoltmax <- as.numeric(datalines[maxLWSidx])
  # get run data
  rundata <- scandata(datalines, startrunidx)
  runtime <- rundata[nrow(rundata), 'timestamp'] - rundata[1, 'timestamp']
  return(list(name=name, leafarea=leafarea, startbasevolt=sbv, endbasevolt=ebv, 
              dryragvolt=drv, wetragvolt=wrv, rundata=rundata, dryleafvolt=dlv, 
			  runtime=runtime, startcalibvolt=scv, wetleafvolt=wlv, 
			  endcalibvolt=ecv, lastLWSvolt=lwsvoltlast, maxLWSvolt=lwsvoltmax))
  # output contains
  # name = the name of the trial
  # startbasevolt = a collection of values in the raw data representative of the
  #   voltage read by the sensor when only the cradle is attached
  # endbasevolt = same concept as startbasevolt, but at the end of the trial
  # dryragvolt = a collection of values in the raw data representative of the
  #   voltage read by the sensor with the rag attached, before the LWS water has
  #   been collected
  # wetragvolt =  same concept as dryragvolt, but for the rag after it has been 
  #   used to absorb water collected on the LWS surface
  # startcalibvolt = a collection of values in the raw data representative of the
  #   voltage read by the sensor when the known load is attached at the beginning
  #   of the trial
  # endcalibvolt = same concept as startcalibvolt, but at the end of the trial
  # lastLWSvolt = the last voltage reported by the LWS before wiping
  # maxLWSvolt = the highest voltage reported by the LWS during the trial
  # rundata = the raw data for the run, starting from the dry leaf measurement
  #   (immediately prior to turning fog generator on) and ending on the wet 
  #   leaf measurement (immediately prior to turning fog generator off)
}