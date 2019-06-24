library(tidyr)
library(purrr)

tbl <-
  list.files(path = "./data/timeseries_ndvi/",
             pattern = "*.csv", 
             full.names = T) %>% 
  map_df(~read.csv(., )) 



# create a function that loads and combines the csv files:
ndvi_ts <- function(a_csv) {
  # open the data 
  the_data <- read.csv(a_csv, header = TRUE, stringsAsFactors = FALSE)
  # write the csv to a new file
  write.csv(the_data, file = paste0(the_dir, "/", basename(a_csv)),
            )
}

all_ndvi_files <- list.files("./data/timeseries_ndvi", pattern = "*.csv",
                               full.names = TRUE)

lapply(all_ndvi_files,
       FUN = ndvi_ts)


dune<- read.csv("./data/timeseries_ndvi/dune_by_chx.csv", header = TRUE)
dune <- dune %>% mutate(location = "dune")

chx<- read.csv("./data/timeseries_ndvi/checkerboards.csv", header = TRUE)
chx <- chx %>% mutate(location = "chx")

afforestation<- read.csv("./data/timeseries_ndvi/afforestation.csv", header = TRUE)
afforestation <- afforestation %>% mutate(location = "afforestation")

heavy<- read.csv("./data/timeseries_ndvi/hvy_trt.csv", header = TRUE)
heavy <- heavy %>% mutate(location = "heavy")

naiman<- read.csv("./data/timeseries_ndvi/naiman.csv", header = TRUE)
naiman <- naiman %>% mutate(location = "naiman")

mobdune<- read.csv("./data/timeseries_ndvi/mobiledune_ehq.csv", header = TRUE)
mobdune <- mobdune %>% mutate(location = "mobile_dune")

dxg<- read.csv("./data/timeseries_ndvi/dxg.csv", header = TRUE)
dxg <- dxg %>% mutate(location = "dxg")

slowlow<- read.csv("./data/timeseries_ndvi/slowandlow.csv", header = TRUE)
slowlow <- slowlow %>% mutate(location = "slowlow")

ag<- read.csv("./data/timeseries_ndvi/lt_ag_incr.csv", header = TRUE)
ag <- ag %>% mutate(location = "ag")

control<- read.csv("./data/timeseries_ndvi/control.csv", header = TRUE)
control <- control %>% mutate(location = "control")


ndvi_ts <- bind_rows(chx, 
                     dune, 
                     afforestation, 
                     heavy, 
                     naiman, 
                     #mobdune, 
                     #ag, 
                     #slowlow, 
                     dxg, 
                     control, 
                     .id= "Site")

ndvi_ts<- as.numeric(ndvi_ts$Year)

ggplot(ndvi_ts, aes(x= Year, y= Fitted, group= location)) +
  geom_point(aes(color = location))+
  geom_line(aes(color = location))+
  scale_y_continuous(breaks=seq(1984,2018,5))+
  theme_bw()

