library(GeoPressureR)
library(ggplot2)
library(plotly)
devtools::install_github("KiranLDA/pamlr")
library("pamlr")

# load custom functions
add_time_to_dates <- function(PAM_data, ref_datetime, minutes_to_add) {
  # Convert reference datetime to POSIXct with UTC timezone
  ref_datetime <- as.POSIXct(ref_datetime, tz = "UTC")

  # Iterate through each component
  for (name in names(PAM_data)) {
    element <- PAM_data[[name]]

    if (is.data.frame(element) && "date" %in% names(element)) {
      # Preserve original timezone
      original_tz <- attr(element$date, "tzone")
      if (is.null(original_tz)) original_tz <- "UTC"

      # Convert ref_datetime to element's timezone
      ref_datetime_local <- as.POSIXct(ref_datetime, tz = original_tz)

      # Split data into pre-mask and post-mask
      mask <- element$date >= ref_datetime_local
      pre_mask <- element[!mask, , drop = FALSE]
      post_mask <- element[mask, , drop = FALSE]

      # Generate 5-minute sequence for gap filling
      if (minutes_to_add > 0) {
        start_time <- ref_datetime_local
        end_time <- ref_datetime_local + minutes_to_add * 60
        time_seq <- seq(from = start_time, to = end_time - 1, by = "5 min")

        # Create zero-filled rows for the gap
        if (length(time_seq) > 0) {
          new_rows <- data.frame(date = time_seq)
          for (col in names(element)) {
            if (col != "date") {
              new_rows[[col]] <- if (is.numeric(element[[col]])) 0 else NA
            }
          }
          new_rows <- new_rows[, names(element)]
        } else {
          new_rows <- data.frame()
        }
      } else {
        new_rows <- data.frame()
      }

      # Shift post-mask dates
      post_mask$date <- post_mask$date + minutes_to_add * 60

      # Combine and order all components
      updated_element <- rbind(pre_mask, new_rows, post_mask)
      updated_element <- updated_element[order(updated_element$date), ]

      # Maintain POSIXct format
      updated_element$date <- as.POSIXct(updated_element$date,
                                         origin = "1970-01-01",
                                         tz = original_tz)

      PAM_data[[name]] <- updated_element
    }
  }
  return(PAM_data)
}


# Helper function for NULL coalescing
`%||%` <- function(a, b) if (!is.null(a)) a else b
save_adjusted_pam <- function(PAM_data, original_path, output_path) {
  if (!dir.exists(output_path)) dir.create(output_path, recursive = TRUE)
  id <- PAM_data$id

  # Measurement type configuration
  meas_config <- list(
    pressure = list(ext = ".pressure", cols = c("Date", "Time", "Pressure[hPa]")),
    light = list(ext = c(".glf", ".gle"), cols = c("Date", "Time", "Light[lux]")),
    acceleration = list(ext = ".acceleration", cols = c("Date", "Time", "Pit[deg]", "Activity[sum]")),
    temperature = list(ext = c(".temperature", ".AirTemperature"), cols = c("Date", "Time", "Temp[C]")),
    bodytemperature = list(ext = ".BodyTemperature", cols = c("Date", "Time", "Temp[C]")),
    magnetic = list(ext = ".magnetic", cols = c("Date", "Time", "Temp[C]", "gX[mG]", "gY[mG]", "gZ[mG]", "mX[mG]", "mY[mG]", "mZ[mG]"))
  )

  # Process each measurement type
  for (meas in names(PAM_data)[-1]) {
    config <- meas_config[[meas]]
    if (is.null(config)) next

    # Find original file
    found_file <- NULL
    for (ext in config$ext) {
      f <- list.files(original_path, pattern = paste0(ext), full.names = TRUE)
      if (length(f) > 0) {
        found_file <- f[1]
        break
      }
    }

    if (is.null(found_file)) next

    # Read original header
    header <- readLines(found_file, n = 6)

    # Prepare data
    df <- PAM_data[[meas]]
    date_parts <- data.frame(
      date = format(df$date, "%d.%m.%Y %H:%M")
      # Time = format(df$date, "%H:%M")
    )

    # Build output columns
    output <- switch(meas,
                     "pressure" = cbind(date_parts, df$obs),
                     "light" = cbind(date_parts, df$obs),
                     "acceleration" = cbind(date_parts, df$pit, df$act),
                     "temperature" = cbind(date_parts, df$obs),
                     "bodytemperature" = cbind(date_parts, df$obs),
                     "magnetic" = cbind(date_parts, df$dummy, df$gX, df$gY, df$gZ, df$mX, df$mY, df$mZ)
    )

    # Write to file
    output_file <- file.path(output_path, paste0(id, sub(".*(\\..+)$", "\\1", found_file)))
    writeLines(header, output_file)
    write.table(output, output_file, append = TRUE, sep = "\t",
                row.names = FALSE, col.names = FALSE, quote = FALSE, na = "")
  }

  message("Data successfully saved to:\n", normalizePath(output_path))
  return(invisible(output_path))
}
create_import_adjusted <-
  function (pathname = pathname, measurements = c(".pressure",
                                                  ".glf", ".acceleration", ".temperature", ".magnetic")) {
    dta = list()
    id = substring(list.files(pathname, pattern = measurements[1],
                              full.names = FALSE), 1, 4)
    dta$id = id
    if (".pressure" %in% measurements) {
      pressure = read.delim(list.files(pathname, pattern = ".pressure",
                                       full.names = TRUE), skip = 6, sep = "", header = FALSE)
      pressure = as.data.frame(list(date = as.POSIXct(strptime(paste(pressure[,
                                                                              1], pressure[, 2]), tz = "UTC", format = "%d.%m.%Y %H:%M")),
                                    obs = pressure[, 3]))
      dta$pressure = pressure
    }
    if (".glf" %in% measurements) {
      light = read.delim(list.files(pathname, pattern = ".glf",
                                    full.names = TRUE)[1], skip = 6, sep = "", header = FALSE)
      light = as.data.frame(list(date = as.POSIXct(strptime(paste(light[,
                                                                        1], light[, 2]), tz = "UTC", format = "%d.%m.%Y %H:%M")),
                                 obs = light[, 3]))
      dta$light = light
    }
    if (".gle" %in% measurements) {
      light = read.delim(list.files(pathname, pattern = ".gle",
                                    full.names = TRUE)[1], skip = 6, sep = "", header = FALSE)
      light = as.data.frame(list(date = as.POSIXct(strptime(paste(light[,
                                                                        1], light[, 2]), tz = "UTC", format = "%d.%m.%Y %H:%M")),
                                 obs = light[, 3]))
      dta$light = light
    }
    if (".acceleration" %in% measurements) {
      acceleration = read.delim(list.files(pathname, pattern = ".acceleration",
                                           full.names = TRUE), skip = 6, sep = "", header = FALSE)
      acceleration = as.data.frame(list(date = as.POSIXct(strptime(paste(acceleration[,
                                                                                      1], acceleration[, 2]), tz = "UTC", format = "%d.%m.%Y %H:%M")),
                                        pit = acceleration[, 3], act = acceleration[, 4]))
      dta$acceleration = acceleration
    }
    if (".temperature" %in% measurements) {
      temperature = read.delim(list.files(pathname, pattern = ".temperature",
                                          full.names = TRUE), skip = 6, sep = "", header = FALSE)
      temperature = as.data.frame(list(date = as.POSIXct(strptime(paste(temperature[,
                                                                                    1], temperature[, 2]), tz = "UTC", format = "%d.%m.%Y %H:%M")),
                                       obs = temperature[, 3]))
      dta$temperature = temperature
    }
    if (".AirTemperature" %in% measurements) {
      temperature = read.delim(list.files(pathname, pattern = ".AirTemperature",
                                          full.names = TRUE), skip = 6, sep = "", header = FALSE)
      temperature = as.data.frame(list(date = as.POSIXct(strptime(paste(temperature[,
                                                                                    1], temperature[, 2]), tz = "UTC", format = "%d.%m.%Y %H:%M")),
                                       obs = temperature[, 3]))
      dta$temperature = temperature
    }
    if (".BodyTemperature" %in% measurements) {
      bodytemperature = read.delim(list.files(pathname, pattern = ".BodyTemperature",
                                              full.names = TRUE), skip = 6, sep = "", header = FALSE)
      bodytemperature = as.data.frame(list(date = as.POSIXct(strptime(paste(bodytemperature[,
                                                                                            1], bodytemperature[, 2]), tz = "UTC", format = "%d.%m.%Y %H:%M")),
                                           obs = bodytemperature[, 3]))
      dta$bodytemperature = bodytemperature
    }
    if (".magnetic" %in% measurements) {
      magnetic = read.delim(list.files(pathname, pattern = ".magnetic",
                                       full.names = TRUE), skip = 6, sep = "", header = FALSE)
      magnetic = as.data.frame(list(date = as.POSIXct(strptime(paste(magnetic[,
                                                                              1], magnetic[, 2]), tz = "UTC", format = "%d.%m.%Y %H:%M")),
                                    dummy = magnetic[, 3],
                                    gX = magnetic[, 4], gY = magnetic[, 5], gZ = magnetic[,
                                                                                          6], mX = magnetic[, 7], mY = magnetic[, 8], mZ = magnetic[,
                                                                                                                                                    9]))
      dta$magnetic = magnetic
    }
    return(dta)
  }






# Choose the id
id <- "14AZ"

PAM_data = create_import_adjusted(pathname = paste0("data/raw-tag/",id),
                                  measurements = c(".pressure",
                                                   ".glf",
                                                   ".acceleration",
                                                   ".temperature",
                                                   ".magnetic"))

# PLot - just to visually check
# Create plots with 4 together (mfrow)
par( mfrow= c(1,4), oma=c(0,2,0,6))

par(mar =  c(4,2,4,2))
plot_sensorimage(PAM_data$acceleration$date, ploty=FALSE,
                 PAM_data$acceleration$act, main = "Activity",
                 col=c("black",viridis::cividis(90)), cex=1.2, cex.main = 2)

par(mar =  c(4,2,4,2))
plot_sensorimage(PAM_data$pressure$date, plotx=TRUE, ploty=FALSE, labely=FALSE,
                 PAM_data$pressure$obs,  main="Pressure",
                 col=c("black",viridis::cividis(90)), cex=1.2, cex.main = 2)

par(mar =  c(4,2,4,2))
plot_sensorimage(PAM_data$temperature$date, plotx=TRUE, ploty=FALSE, labely=FALSE,
                 PAM_data$temperature$obs,  main="Temperature",
                 col=c("black",viridis::cividis(90)), cex=1.2, cex.main = 2)

par(mar =  c(4,2,4,2))
plot_sensorimage(PAM_data$light$date, labely=FALSE,
                 PAM_data$light$obs,  main="Light",
                 col=c("black",viridis::cividis(90)), cex=1.2, cex.main = 2)




# Apply xx-minute shift to all date&time starting at the specified ref_datetime
PAM_data <- add_time_to_dates(
  PAM_data,
  ref_datetime = "2015-09-20 23:55:00",
  minutes_to_add = 60*2+40 # make sure this is in 5-min increments
)


# Plot again
par( mfrow= c(1,4), oma=c(0,2,0,6))

par(mar =  c(4,2,4,2))
plot_sensorimage(PAM_data$acceleration$date, ploty=FALSE,
                 PAM_data$acceleration$act, main = "Activity",
                 col=c("black",viridis::cividis(90)), cex=1.2, cex.main = 2)

par(mar =  c(4,2,4,2))
plot_sensorimage(PAM_data$pressure$date, plotx=TRUE, ploty=FALSE, labely=FALSE,
                 PAM_data$pressure$obs,  main="Pressure",
                 col=c("black",viridis::cividis(90)), cex=1.2, cex.main = 2)

par(mar =  c(4,2,4,2))
plot_sensorimage(PAM_data$temperature$date, plotx=TRUE, ploty=FALSE, labely=FALSE,
                 PAM_data$temperature$obs,  main="Temperature",
                 col=c("black",viridis::cividis(90)), cex=1.2, cex.main = 2)

par(mar =  c(4,2,4,2))
plot_sensorimage(PAM_data$light$date, labely=FALSE,
                 PAM_data$light$obs,  main="Light",
                 col=c("black",viridis::cividis(90)), cex=1.2, cex.main = 2)



# save adjusted files in the same fromat as originals
# in original_path == output_path the originalfiles will be over saved with the adjusted
save_adjusted_pam(
  PAM_data,
  original_path = paste0("data/raw-tag/",id),
  output_path = paste0("data/raw-tag/",id,2)
)



