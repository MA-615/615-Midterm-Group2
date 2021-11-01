library(tidyverse)
library(maps)

map<-function(statename){
  m<-map_data("county",statename)
  
  p<-ggplot(data=m,
            mapping=aes(x=long,y=lat,group=group,fill=region))
  p+geom_polygon()+guides(fill=FALSE)+ggtitle(statename)
  
}
#map("Florida")

#this function is to show the map of the state
##use map(state name),such as map("Florida")