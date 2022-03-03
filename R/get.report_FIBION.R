#' get.report_FIBION
#'
#' @param datadir Character. Path to directory where the output csv files exported from FIBION are stored.
#' @param outputdir Character. Path to directory where the data sets should be stored.
#' @param store.output Logical (default = FALSE). Indicate whether the generated datasets should be stored in outputdir.
#' @param ID Numeric or character (default = NULL). Specify the ID of the file being processed (only works if processing 1 file).
#'
#' @return FIBION data aggregated at day and week levels.
#' @export
get.report_FIBION = function(datadir = NULL, data = NULL, outputdir = "./", store.output = FALSE, ID = NULL) {
  
  # IF FILEPATH OR DIRECTORY PATH PROVIDED... ----
  if (!is.null(datadir) & is.null(data)) {
    if (isTRUE(dir.exists(datadir)) & isFALSE(file.exists(datadir))) { # then, a directory is provided
      files = dir(datadir, full.names = T, pattern = ".csv|.xlsx")
    } else if (isFALSE(unique(dir.exists(datadir))) & isTRUE(unique(file.exists(datadir)))) { #then filenames are provided
      files = datadir
      datadir = dirname(files[1])
    } else { 
      stop("Revise your datadir path, the directory does not exist or does not contain any csv file")
    }
    toProcess = length(files)
  }
  
  # IF DATA.FRAME PROVIDED... ----
  if (!is.null(data) & is.null(datadir)) {
    files = data
    toProcess = 1
  }
  
  if (!is.null(data) & !is.null(datadir)) stop("datadir and data provided, please define only one of them")

  # daily output ----
  pb = utils::txtProgressBar(min = 0, max = toProcess , style = 3,
                      title = paste0("Processing ", length(files), " files..."))
  
  for (i in 1:toProcess) {
    utils::setTxtProgressBar(pb, i)
    filename_split = unlist(strsplit(files[i], ".", fixed = T))
    format = filename_split[length(filename_split)]
    # read data
    if (is.character(files)) {
      if (format == "csv") dat = utils::read.csv(files[i])
      if (format == "xlsx") dat = openxlsx::read.xlsx(files[i])
      # ID
      id = tools::file_path_sans_ext(basename(files[i]))
    }
    if (is.data.frame(files)) {
      dat = files
      id = "not extracted"
      if (!is.null(ID)) id = ID
    }
    
    # date
    dat$date = substr(as.character(as.POSIXct(dat$unixts/1000, origin = "1970-01-01")), 1, 10)
    date = as.POSIXct(dat$unixts/1000, origin = "1970-01-01")

    options(warn = -1)
    for (ci in 4:(ncol(dat) - 1)) {
      if (ci == 4) daily = stats::aggregate(dat[, ci] ~ dat$date, FUN = sum)
      if (ci > 4) daily = merge(daily, stats::aggregate(dat[, ci] ~ dat$date, FUN = sum), by = "dat$date")
    }
    options(warn = 0)

    # some extra vars
    daily$Weekday = as.numeric(format(as.POSIXct(daily[, 1]), format="%u"))
    daily$window.time = rowSums(daily[,2:(ncol(daily) - 5)])
    daily$ID = id

    colnames(daily) = c("Date", colnames(dat)[4:(ncol(dat) - 1)], "Weekday", "Window.time", "ID")

    daily = daily[, c(ncol(daily), 1, (ncol(daily) - 2):(ncol(daily) - 1), 2:(ncol(daily) - 3))]

    # weekly averages ----
    # clean days with less than 23 hours or less than 10 hours of wear data
    nodata_column = grep("nodata", colnames(daily))
    days2exclude = which(daily$Window.time < 23*60 | (daily$Window.time - daily[,nodata_column]) < 10*60)
    if (length(days2exclude) > 0) daily_clean = daily[-days2exclude,]
    if (length(days2exclude) == 0) daily_clean = daily
    
    # weekly values
    weekly = as.data.frame(matrix(NA, nrow = 1, ncol = 100))
    weekly[1,1] = id
    weekly[1,2] = nrow(daily_clean)
    weekly[1,3] = sum(daily_clean$Weekday <= 5)
    weekly[1,4] = sum(daily_clean$Weekday >= 6)
    w_names = c("ID", "nDays", "nWeekdays", "nWeekends")

    ci = 4
    # Averages
    day2week = grep("time|count", colnames(daily_clean))
    # plain means
    for (wk in 1:length(day2week)) {
      ci = ci + 1
      weekly[1, ci] = mean(daily_clean[,day2week[wk]])
      w_names[ci] = paste0(colnames(daily_clean)[day2week[wk]], "_pla")
    }
    # weighted means
    for (wk in 1:length(day2week)) {
      ci = ci + 1
      weekly[1, ci] = ((mean(daily_clean[which(daily_clean$Weekday <= 5), day2week[wk]])*5) + (mean(daily_clean[which(daily_clean$Weekday >= 5), day2week[wk]])*2)) / 7
      w_names[ci] = paste0(colnames(daily_clean)[day2week[wk]], "_wei")
    }
    colnames(weekly)[1:ci] = w_names
    weekly = weekly[, 1:ci]
    
    if (i == 1) {
      week_out = weekly
      day_out = daily
      dayClean_out = daily_clean
    } else {
      week_out = plyr::rbind.fill(week_out, weekly)
      day_out = plyr::rbind.fill(day_out, daily)
      dayClean_out = plyr::rbind.fill(dayClean_out, daily_clean)
    }
  }

  # store output
  if (isTRUE(store.output)) {
    if (!dir.exists(file.path(outputdir))) dir.create(outputdir)
    
    openxlsx::write.xlsx(day_out, file.path(outputdir, "daysummary.xlsx"))
    openxlsx::write.xlsx(dayClean_out, file.path(outputdir, "daysummary_clean.xlsx"))
    openxlsx::write.xlsx(week_out, file.path(outputdir, "weeksummary.xlsx"))
  }
  
  # return
  invisible(list(daySummary = day_out, dayCleanSummary = dayClean_out, weekSummary = week_out))
}
