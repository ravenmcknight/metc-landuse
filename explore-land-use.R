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
library(leaflet)
library(htmltools)

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

switch_sf <- lusf %>% filter(LUSE2010 == 210 & LUSE2016 != 210)

# easier to work with 
ludt <- data.table(lusf)

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

# Which land use types are changing overall? --------------

# get counts
c10 <- ludt[, .N, by = Dscrpt2010]
c10[, desc := factor(str_trim(Dscrpt2010))]
c10[, Dscrpt2010 := NULL]

c16 <- ludt[, .N, by = Dscrpt2016]
c16[, desc := factor(str_trim(Dscrpt2016))]
c16[, Dscrpt2016 := NULL]

c <- c16[c10, on = 'desc']
colnames(c) <- c('count10', 'desc', 'count16')
c[, change := count16-count10]

ggplot(c) +
  geom_bar(aes(x = desc, y = change, fill = change > 0), stat = 'identity') +
  theme_minimal() + coord_flip() +
  theme(legend.position = 'none', plot.title = element_text(hjust = 0.5)) +
  labs(x = 'land use', title = 'Change in land use 2010-2016')

# Spatially, where is undev becoming dev? -----------------
switch_sf <- st_transform(switch_sf, 4326) # leaflet does not like utm

#lusepal <- colorFactor(palette = 'Set3', levels = levels(switch_sf$Dscrpt2016))

labs <- sprintf(
  "Now categorized as %s",
  switch_sf$Dscrpt2016
) %>% lapply(htmltools::HTML)

leaflet(switch_sf) %>% addTiles() %>%
  addPolygons(weight = 4, color = 'gray', fillOpacity = 0.8, label = labs)

# different in urban core?
uc <- block_groups('MN', county = c('Hennepin', 'Ramsey'), year = 2016)
uc <- st_transform(uc, 4326)

uc_undev <- st_join(switch_sf, uc, st_within)
# sum(is.na(uc_undev$STATEFP))  # not all NA
uc_undev_dt <- data.table(uc_undev)
uc_undev_dt[is.na(STATEFP), urbancore := FALSE]
uc_undev_dt[!is.na(STATEFP), urbancore := TRUE]

ggplot(uc_undev_dt) +
  geom_bar(aes(x=fct_infreq(Dscrpt2016), fill = urbancore), stat = 'count') +
  theme(axis.text = element_text(angle = 90)) +
  theme_minimal() + coord_flip() +
  labs(x = '2016 use', title = '2016 uses of undeveloped 2010 land')

# was anything uncategorized? -----------------------------
