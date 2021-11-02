library(tidyverse)
library(maps)
usamap<-function(){
  usa_map <- map_data("state")
  states<-c("California","Florida","Oregon","Washington")
  region<-tolower(states)
  tmp<-data.frame(cbind(states,region))
  gg <- ggplot()
  gg <- gg + geom_map(data=usa_map, map=usa_map,
                    aes(long, lat, map_id=region),
                    color="black", size=0.15, fill=NA)
  gg <- gg + geom_map(data=tmp, map=usa_map,
                    aes(fill=states, map_id=region),
                    color="black", size=0.15)
  gg
}
