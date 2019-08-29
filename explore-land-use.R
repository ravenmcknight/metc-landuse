## data & packages ----------------------------------------

library(data.table)
library(ggplot2)
library(sf)
library(dplyr)
library(tigris)
library(rgdal)
library(gridExtra)
# devtools::install_github("timelyportfolio/parsetR")
library(parsetR)
library(forcats)
library(stringr)

# block groups
counties <- c("Anoka", "Carver", "Dakota", "Hennepin", "Ramsey", "Scott", "Washington") 
options(tigris_class = 'sf')
bg_sff <- block_groups('MN', counties, year = 2016)

# read data from geospatial commons
url = 'ftp://ftp.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/plan_generl_lnduse_historical/shp_plan_generl_lnduse_historical.zip'
loc <- file.path(tempdir(), 'lu.zip')
download.file(url, loc)
unzip(loc, exdir = file.path(tempdir(), 'lu'), overwrite = TRUE)
file.remove(loc)
lu <- readOGR(file.path(tempdir(), 'lu'), layer = 'GeneralizedLandUseHistorical', stringsAsFactors = FALSE)
lusf <- st_as_sf(lu)

# easier to work with 
ludt <- setDT(lusf)

# summaries ---------------------------
unique(ludt$Dscrpt2010)
unique(ludt$Dscrpt2016)
ludt[LUSE2010 == 210]# 5463 undev in 2010, 4381 undev in 2016
switch <- ludt[LUSE2010 == 210 & LUSE2016 != 210] # 3185 of these (bgs that went from undeveloped to developed)

# what is undev land now? -------------
switch[, Descrpt2016 := as.factor(Dscrpt2016)]
summary(switch$Descrpt2016)

ggplot(switch) +
  geom_bar(aes(x=fct_infreq(Dscrpt2016)), stat = 'count') +
  theme(axis.text = element_text(angle = 90), plot.title = element_text(hjust = 0.5)) +
  theme_minimal() + coord_flip() +
  labs(x = '2016 use', title = '2016 uses of land classified as "undeveloped" in 2010')

# possible vis? (too complicated)
tmp <- na.omit(ludt[, .N, .(Origin = Dscrpt2010, Destination = Dscrpt2016)])
tmp[, Origin := factor(str_trim(Origin))]
tmp[, Destination := factor(Destination)]
tmp <- tmp[order(-N)]
parset(tmp, dimensions = c('Origin', 'Destination'),
       value = htmlwidgets::JS("function(d){return d.N}"),
       tension = 0.5)