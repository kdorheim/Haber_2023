# Compare SEM inputs with the UMBS data
library(ggplot2)
# data <- read.csv(here::here("US-UMB_data/AMF_US-UMB_BASE_HR_10-1.csv"), skip = 2) 
head(data)
summary(data$PPFD_IN)

data %>% 
  filter(PPFD_IN != -9999) -> 
  data

data %>% 
  ggplot(aes(TIMESTAMP_START, PPFD_IN)) + 
  geom_point()

data %>% 
  filter(PPFD_IN != -9999) %>% 
  mutate(PPFD_IN = if_else(PPFD_IN < 0, 0, PPFD_IN)) -> 
  dd

d <- dd[1:100, ]
plot(d$PPFD_IN, type = "l")

my_d <- read.csv("met/SEM/SEM_input_1985.csv")
summary(my_d$PAR)
summary(d$PPFD_IN)


# Okay I am not that certian about 
read.csv("ameriflux/AMF_US-UMd_FLUXNET_SUBSET_HH_2007-2021_3-5.csv") %>% 
  filter(PPFD_IN != -9999) %>% 
  mutate(PPFD_IN = ifelse(PPFD_IN < 0, 0, PPFD_IN)) -> 
  dat

data.frame(year = substr(x = dat$TIMESTAMP_START, start = 1, stop = 4), 
           month = substr(x = dat$TIMESTAMP_START, start = 5, stop = 6), 
           day = substr(x = dat$TIMESTAMP_START, start = 7, stop = 8)) %>% 
  mutate(time = as.Date(paste0(year, "-",  month, '-', day))) -> 
  time_df

full_df <- cbind(time_df, dat)

full_df %>%  
  filter(year == 2008) -> 
  FLUXNET_2007

my_d <- read.csv("met/SEM/SEM_input_2008.csv") 
data.frame(year = substr(x = my_d$time, start = 1, stop = 4), 
           month = substr(x = my_d$time, start = 5, stop = 6), 
           day = substr(x = my_d$time, start = 7, stop = 8)) %>% 
  mutate(time = as.Date(paste0(year, "-",  month, '-', day))) -> 
  time_df
my_d <- cbind(time_df, my_d)


ggplot() + 
  geom_point(data = FLUXNET_2007, aes(time, PPFD_IN, color = "obs")) + 
  geom_point(data = my_d, aes(time, PAR, color = "mine"))





  

