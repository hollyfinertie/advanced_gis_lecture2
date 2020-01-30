setwd("C:/Prop_val2/stuff")

### Lab goal is to create a county level map of Quality of Life Index Ranking from the 
### Robert Wood Johnson Foundation

install.packages("maps")
require(maps)

ny_cty <- map('county', 'new york', fill=TRUE, col = palette())
head(ny_cty)

list.names.ny <- strsplit(ny_cty$names,",")
head(list.names.ny, n=62)
View(list.names.ny)

map.IDs <- as.character(tolower(sapply(list.names.ny, function(x) x[2])))
head(map.IDs, n=62)
map.IDs <- gsub("st lawrence", "stlawrence", map.IDs)
head(map.IDs, n=62)

require(maptools)
ny_cty_sp <- map2SpatialPolygons(ny_cty, IDs = map.IDs, proj4string = CRS("+init=epsg:2261"))
head(map.IDs, n=62)




####Read data and create Spatial Polygons Dataframe

#install.packages("data.table")
require(data.table)

rwj <- fread("rwj_rank.csv", stringsAsFactors = F, data.table = F, colClasses=list(character=c("FIPS")))
head(rwj)
ny_rwj <- subset(rwj, State == "New York")
head(ny_rwj, n=62)
ny_rwj$County <- gsub("St. Lawrence", "stlawrence", ny_rwj$County)
head(ny_rwj$County, n=62)
row.names(ny_rwj) <- as.character(tolower(ny_rwj$County))

head(row.names(ny_rwj), n=62)
head(map.IDs, n=62)

ny_rwj_df <- SpatialPolygonsDataFrame(ny_cty_sp,ny_rwj)
summary(ny_rwj_df)




summary(ny_rwj_df$QL.Rank)
ny_rwj_df$QL.Rank <- as.numeric(ny_rwj_df$QL.Rank)
summary(ny_rwj_df$QL.Rank)
ny_rwj_df$QL.Rank <- 62 - ny_rwj_df$QL.Rank

library(RColorBrewer)
library(classInt)

plotvar <- ny_rwj_df$QL.Rank
nclr <- 5
class <- classIntervals(plotvar, nclr, style = "quantile")
plotclr <- brewer.pal(nclr, "Greens")
colcode <- findColours(class, plotclr, digits = 3)

plot(ny_rwj_df, col = colcode, border = "grey",axes = F)
title(main = "Quality of Life Rankings: NY State \n by Jeremy R. Porter", 
      sub = "Data Source: Robert Wood Johnson Foundation")
legend("bottomleft", legend = names(attr(colcode,"table")), 
       fill = attr(colcode, "palette"), cex=0.55)

require(rgdal)
writeOGR(ny_rwj_df, 
         dsn = "working_directory", 
         layer = "RWJ_NY", 
         driver = "ESRI Shapefile")


#install.packages("sf")
require(sf)
rwj_sf <- st_read(dsn = "working_directory",
                  layer = "RWJ_NY")

names(rwj_sf)
head(rwj_sf)

plot(rwj_sf, max.plot = 15)

plot(st_geometry(rwj_sf), axes=TRUE)

plot(rwj_sf[,"QL_Rank"], 
     graticule=st_crs(rwj_sf), axes=TRUE, las=1)

plot(st_transform(rwj_sf[,"QL_Rank"], 2263), 
     graticule=st_crs(rwj_sf), 
     axes=TRUE, las=1)

coords <- st_coordinates(rwj_sf)
plot(coords)

#### Data Visualization with GGplot

#install.packages("ggplot2")
require(ggplot2)

map1 <- ggplot(data = rwj_sf) +
  geom_sf()
  
map1

map1a <- ggplot(data = rwj_sf) +
  geom_sf() +
  aes(fill=cut_number(QL_Rank, 5)) +
  scale_fill_brewer()

map1a


map2 <- ggplot(data = rwj_sf) +
  geom_sf() +
  aes(fill=cut_number(QL_Rank, 5)) +
  scale_fill_brewer() +
  ggtitle("County Level Quality of Life Rank\nNew York State") +
  theme(line = element_blank(),                          
        axis.text=element_blank(),                       
        axis.title=element_blank(),                      
        panel.background = element_blank())  

map2

map3 <- ggplot(data = rwj_sf) +
  geom_sf() +
  aes(fill=cut_number(QL_Rank, 5)) +
  scale_fill_brewer() +
  ggtitle("County Level Quality of Life Rank\nNew York State") +
  theme(axis.text=element_text(size=8),                       
        axis.title=element_text(size=8,face="bold"),
        plot.title = element_text(hjust = 0.5))  

map3

map4 <- ggplot(data = rwj_sf) +
  geom_sf() +
  aes(fill=cut_number(QL_Rank, 5)) +
  scale_fill_brewer() +
  ggtitle("County Level Quality of Life Rank\nNew York State") +
  theme(axis.text=element_text(size=8),                       
        axis.title=element_text(size=8,face="bold"),
        plot.title = element_text(face="bold",size=10,hjust = 0.5))  

map4


map5 <- ggplot(data = rwj_sf) +
  geom_sf() +
  aes(fill=cut_number(QL_Rank, 5)) +
  scale_fill_brewer(name="Quantile", palette="Blues", 
                    labels=c("1st",
                             "2nd",
                             "3rd",
                             "4th",
                             "5th")) +
  ggtitle("County Level Quality of Life Rank New York State\n ") +
  theme(axis.text=element_text(size=8),                       
        axis.title=element_text(size=8),
        plot.title = element_text(face="bold",size=12,hjust = 0.5),
        legend.position="bottom")

map5

#install.packages("ggsn")
require(ggsn)


northSymbols()


map6 <- ggplot(data = rwj_sf) +
  geom_sf() +
  aes(fill=cut_number(QL_Rank, 5)) +
  scale_fill_brewer(name="Quantile", palette="Blues", 
                    labels=c("1st",
                             "2nd",
                             "3rd",
                             "4th",
                             "5th")) +
  labs(title = "County Level Quality of Life Rank New York State",
       subtitle = "Jeremy R. Porter\n",
       caption = "\nData source: Robert Wood Johnson Foundation") +
  theme(axis.text=element_text(size=6),                       
        axis.title=element_text(size=6),
        plot.title = element_text(face="bold",size=16,hjust = 0.5),
        plot.subtitle = element_text(size=12,hjust = 0.5),
        plot.caption = element_text(),
        legend.position=c(0.91,0.58)) +
  north(rwj_sf, scale = 0.15, symbol=1, location="bottomleft")
  

map6



rstudioapi::documentSave()




