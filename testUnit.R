file_URL <- "http://d396qusza40orc.cloudfront.net/rprog%2Fscripts%2Fsubmitscript1.R"
dest_file <- "submitscript1.R"
mode_type <- "wb"
download.file(file_URL, dest_file, mode = mode_type)
source("submitscript1.R")