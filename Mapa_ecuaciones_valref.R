library(ggmap)
library(ggrepel)
library(maps)


Ecuaciones <- read_excel("./Ecuaciones.xlsx")

mapWorld <- borders("world", colour="white", fill="lightgrey")

ggplot(data = Ecuaciones, fill = Country) + mapWorld + geom_point(aes(x=Longitude, y=Latitude) ,
                                                  color="black", size=1)+
  geom_label_repel(data = Ecuaciones, 
                   aes(x=Longitude,y= Latitude,
                       label= paste(paste(Author, ", ", Year, sep =""), 
                                    paste("n =", n), sep = "\n") , 
                       fill = Country),
                   direction = "both",
                   force = 25) + 
  theme_void() + labs(title = 
                        (expression(paste("Predictive equations for ",
                                          dot(V), "O", 2[max], 
                                          " around the world"))))


