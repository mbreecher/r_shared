setwd("C:/r/workspace/shared")
source("import_functions.R")
source("headings.R")

daily <- import_daily_hours()
daily$week <- format(daily$Date, format = "%y-%U")
daily$range <- weekyear_to_written_with_year(daily$week)
daily$header <- paste(daily$week, daily$range, sep = "\n")

setwd("C:/R/workspace/temp")
write.csv(daily, "daily.csv", row.names = F)
