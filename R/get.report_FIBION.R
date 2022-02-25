#' get.report_FIBION
#'
#' @param datadir Path to directory where the output csv files exported from FIBION are stored.
#' @param outputdir Path to directory where the data sets should be stored.
#' @param store.csv Logical to indicate whether the csv reports should be stored as csv files in outputdir.
#'
#' @return FIBION data aggregated at day and week levels.
#' @export
get.report_FIBION = function(datadir, outputdir = "./", store.csv = FALSE) {
  
  # files to analyze
  if (isTRUE(dir.exists(datadir)) & isFALSE(file.exists(datadir))) { # then, a directory is provided
    files = dir(datadir, full.names = T, pattern = "*.csv")
  } else if (isFALSE(unique(dir.exists(datadir))) & isTRUE(unique(file.exists(datadir)))) { #then filenames are provided
    files = datadir
    datadir = dirname(files[1])
  } else { 
    stop("Revise your datadir path, the directory does not exist or does not contain any csv file")
  }

  # daily output ----
  pb = utils::txtProgressBar(min = 0, max = length(files), style = 3,
                      title = paste0("Processing ", length(files), " files..."))
  for (i in 1:length(files)) {
    utils::setTxtProgressBar(pb, i)
    # read data
    dat = utils::read.csv(files[i])

    # ID
    id = tools::file_path_sans_ext(basename(files[i]))

    # date
    dat$date = as.POSIXct(dat$local, tz = "Europe/Stockholm")

    options(warn = -1)
    for (ci in 4:(ncol(dat) - 1)) {
      if (ci == 4) daily = stats::aggregate(dat[, ci] ~ dat$date, FUN = sum)
      if (ci > 4) daily = merge(daily, stats::aggregate(dat[, ci] ~ dat$date, FUN = sum), by = "dat$date")
    }
    options(warn = 0)

    # some extra vars
    daily$Weekday = as.numeric(format(daily[, 1], "%u"))
    daily$window.time = rowSums(daily[,2:(ncol(daily) - 5)])
    daily$ID = id

    colnames(daily) = c("Date", colnames(dat)[4:(ncol(dat) - 1)], "Weekday", "Window.time", "ID")

    daily = daily[, c(ncol(daily), 1, (ncol(daily) - 2):(ncol(daily) - 1), 2:(ncol(daily) - 3))]

    # weekly averages ----
    weekly = as.data.frame(matrix(NA, nrow = 1, ncol = 100))
    weekly[1,1] = id
    weekly[1,2] = nrow(daily)
    weekly[1,3] = sum(daily$Weekday <= 5)
    weekly[1,4] = sum(daily$Weekday >= 6)
    w_names = c("ID", "nDays", "nWeekdays", "nWeekends")

    ci = 4
    # Averages
    day2week = grep("time|count", colnames(daily))
    # plain means
    for (wk in 1:length(day2week)) {
      ci = ci + 1
      weekly[1, ci] = mean(daily[,day2week[wk]])
      w_names[ci] = paste0(colnames(daily)[day2week[wk]], "_pla")
    }
    # weighted means
    for(wk in 1:length(day2week)) {
      ci = ci + 1
      weekly[1, ci] = ((mean(daily[which(daily$Weekday <= 5), day2week[wk]])*5) + (mean(daily[which(daily$Weekday >= 5), day2week[wk]])*2)) / 7
      w_names[ci] = paste0(colnames(daily)[day2week[wk]], "_wei")
    }
    colnames(weekly)[1:ci] = w_names
    weekly = weekly[, 1:ci]
    
    if(i == 1) {
      week_out = weekly
      day_out = daily
    } else {
      week_out = plyr::rbind.fill(week_out, weekly)
      day_out = plyr::rbind.fill(day_out, daily)
    }
  }

  # store output
  if(isTRUE(store.csv)) {
    if(!dir.exists(file.path(outputdir))) dir.create(outputdir)
    
    utils::write.csv(day_out, file.path(outputdir, "daysummary.csv"), row.names = F)
    utils::write.csv(week_out, file.path(outputdir, "weeksummary.csv"), row.names = F)
  }
  
  # return
  return(list(daySummary = day_out, weekSummary = week_out))
}
