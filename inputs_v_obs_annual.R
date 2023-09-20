# Compare SEM inputs with the UMBS data
library(ggplot2)

theme_set(theme_bw())
yr <- 2008

read.csv("met/SEM/SEM_input_", yr, ".csv") %>% 
  mutate(time = ymd_h(paste0(year, "-", month, '-', day, " ", hour))) -> 
  my_met

umbs_obs <- read.csv("ameriflux/AMF_US-UMd_FLUXNET_SUBSET_HH_2007-2021_3-5.csv")  %>% 
  mutate(year = substr(x = TIMESTAMP_START, start = 1, stop = 4), 
         month = substr(x = TIMESTAMP_START, start = 5, stop = 6), 
         day = substr(x = TIMESTAMP_START, start = 7, stop = 8), 
         hour = substr(x = TIMESTAMP_START, start = 9, stop = 10)) %>% 
  mutate(time = ymd_h(paste0(year, "-", month, '-', day, " ", hour))) %>%  
  filter(year == yr)

umbs_obs %>% 
  filter(PPFD_IN != -9999) %>% 
  mutate(PPFD_IN = ifelse(PPFD_IN < 0, 0, PPFD_IN)) %>% 
  select(time, par = PPFD_IN) -> 
  umbs_par

umbs_obs %>% 
  select(time, VPD = VPD_F) -> 
  umbs_vpd

umbs_obs %>% 
  select(time, temp = TA_F) -> 
  umbs_temp

umbs_obs %>% 
  select(time, precip = P_F) -> 
  umbs_P

alpha <- 0.7
ggplot() + 
  geom_point(data = umbs_par, aes(time, par, color = "UMBS"), alpha = alpha) + 
  geom_point(data = my_met, aes(time, PAR, color = "SEM"), alpha = alpha) + 
  labs(title = "PAR")

ggplot() + 
  geom_point(data = umbs_vpd, aes(time, VPD/10, color = "UMBS"), alpha = alpha) + 
  geom_point(data = my_met, aes(time, VPD, color = "SEM"), alpha = alpha) + 
  labs(title = "VPD")

ggplot() + 
  geom_point(data = umbs_temp, aes(time, temp, color = "UMBS"), alpha = alpha) + 
  geom_point(data = my_met, aes(time, temp, color = "SEM"), alpha = alpha) + 
  labs(title = "Temp")

total_umbs_p <- sum(umbs_P$precip)
total_met_p <- sum(my_met$precip)
ggplot() + 
  geom_point(data = umbs_P, aes(time, precip, color = "UMBS"), alpha = alpha) + 
  geom_point(data = my_met, aes(time, precip, color = "SEM"), alpha = alpha) + 
  labs(title = "Precip", 
       caption = paste0("total UMBS precip: ", round(total_umbs_p), "\n",
                        "total SEM precip: ", round(total_met_p)))

