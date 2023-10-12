# Down scaling to 30 min data spoke with Ben BL and agreed that for a first 
# cut that would be to do the simplest thing aka linear interpolation (or some 
# sort of averaging as appropriate). 

# 0. Set Up --------------------------------------------------------------------
library(assertthat)
library(dplyr)
library(ncdf4)
library(lubridate)
library(zoo)

INPUT_DIR <- here::here("met", "SEM_3hrly")
WRITE_TO <- here::here("met", "SEM")
dir.create(WRITE_TO, showWarnings = FALSE)

# 1. Set Up --------------------------------------------------------------------

# Read in the data frame and then figure out the 

lapply(list.files(INPUT_DIR, pattern = "csv", full.names =TRUE), 
       function(file){
         input_df <- read_SEM_met(file) %>% 
           mutate(year = year(time), 
                  month = month(time), 
                  day = day(time), 
                  hour = hour(time), 
                  minute = minute(time), 
                  second = second(time))
         
         start_ts <- as.Date(input_df$time[1])
         end_ts <- as.Date(input_df$time[nrow(input_df)])
         
         #TODO how do we handle an issue of leap days/years? 
         full_ts <- seq(as.POSIXct(start_ts, tz = "UTC"),
                        as.POSIXct((as.Date(end_ts)), tz = "UTC"),
                        by = "30 min")
        
        last_row <- input_df[1, ]
        last_row[["time"]] <- paste0( full_ts[length(full_ts)]," 00:00:00")
        input_df <- rbind(input_df, last_row) %>% 
          mutate(year = year(time)) %>% 
          distinct()

         # Check to make sure not missing data is an issue or not... 
         # missing_obs <- time[!time %in% full_ts]
         # if(length(missing_obs) > 0 ){
         #   warning("there are missing obs... ", missing_obs)
         # }
         
         to_add <- full_ts[!full_ts %in% input_df$time]
         
         missing_df <- data.frame(time = to_add, 
                                  PAR = NA, 
                                  temp = NA, 
                                  VPD = NA, 
                                  precip = NA) %>% 
           mutate(year = year(time), 
                  month = month(time), 
                  day = day(time), 
                  hour = hour(time), 
                  minute = minute(time), 
                  second = second(time))
      
         dplyr::bind_rows(input_df, missing_df) %>% 
           select(-time) %>% 
           arrange(year, month, day, hour, minute, second) %>% 
           mutate(PAR = na.approx(PAR, na.rm = FALSE),
                  temp = na.approx(temp, na.rm = FALSE),
                  VPD = na.approx(VPD, na.rm = FALSE),
                  precip = 1/6 * na.locf(precip, na.rm = FALSE)) -> 
           out

         time <- as.POSIXlt(paste0(out$year, "-", out$month, "-", out$day, 
                        " ", out$hour, ":", out$minute, ":", out$second), tz = "UTC")
         out$time <- time
         out <- out[-nrow(out), ]
         
         # input_precip <- sum(input_df$precip)
         # out_precip <- sum(out$precip)
         # if(abs(input_precip - out_precip) > 1){
         #   warning("difference in precip is ", abs(input_precip - out_precip))
         # }
         
         yr <- unique(out$year)
         ofile <- file.path(WRITE_TO, paste0("30min_met_converted_", yr, ".csv"))
         write.csv(x = out, file = ofile, row.names = FALSE)  
         
       })




# Concerns ---------------------------------------------------------------------
# # okay slightly worried that the PAR is going to be over estimated because is 
# # does not really respect the hours of daylight... check out august is there 
# # PAR up until midnight? 
# new_df %>% 
#   filter(date == "1979-08-30") %>% 
#   ggplot() + 
#   geom_point(aes(time, VPD, color = "obs")) 


# the temperautre I think is normalized it seems odd that is is close to 0! 
# precip disbtrution oover the time steps dose have a few desciprtencides in mm
# but only a few are larger than 1 and don't seem to be that big... 
# Warning messages:
#   1: In FUN(X[[i]], ...) : difference in precip is 2.94784222227827
# 2: In FUN(X[[i]], ...) : difference in precip is 1.0318879936508
# 3: In FUN(X[[i]], ...) : difference in precip is 1.98787511123953
# 4: In FUN(X[[i]], ...) : difference in precip is 1.29407185756997
# 5: In FUN(X[[i]], ...) : difference in precip is 1.77101594491614
# 6: In FUN(X[[i]], ...) : difference in precip is 1.75260416381172
# 7: In FUN(X[[i]], ...) : difference in precip is 1.90503540443365
