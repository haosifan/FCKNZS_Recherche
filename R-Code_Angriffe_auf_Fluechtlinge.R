library(sp)
require(rgdal)
require(ggmap)
library(maptools)
require(png)
require(grid)


map <- readRDS("DEU_adm1.rds")
dat <- read.csv2("data_Angriffe.csv", header=T)

map_df <- fortify(map)
map_dat <- merge(map_df,dat,by="id")
dat <- cbind(dat,geocode(as.character(dat$name), source = "google"))

refs <- "Anzahl der Angriffe: http://www.mut-gegen-rechte-gewalt.de \n Ausländische Bevölkerung: Statistisches Bundesamt \n Anzahl der Asylanträge: Bundesamt für Migration und Flüchtlinge"


p <- ggplot() + 
  geom_polygon(data = map_dat,aes(x=long,y=lat,group=group, fill=Anteil_ausländischer_Bevölkerung))+
  labs(fill="Anteil ausländischer Bevölkerung") +
  geom_path(data=map_dat, aes(x=long,y=lat, group=group), size=0.01)+
  scale_fill_gradient(low="lightskyblue1", high="deepskyblue2") +
  geom_point(data = dat, aes(x=lon, y=lat, size=Angriffe.pro.EW, alpha=Angriffe.pro.Antrag)) +
  scale_size(range=c(5,20)) +
  scale_alpha(range = c(0.25, 1.0)) +
  labs(size="Angriffe pro 100000 Einwohner", alpha="Angriffe pro 1000 Anträge")+
  ggtitle("Angriffe auf Flüchtlinge und \n Flüchtlingsunterkünfte 2015") + 
  theme(plot.title = element_text(size=20, face="bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.line.y=element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.line.x=element_blank())

plot(p)

#Quellen
makeFootnote <- function(footnoteText="Quellen",
                         size= .7, color= grey(.5))
{
  require(grid)
  pushViewport(viewport())
  grid.text(label= footnoteText ,
            x = unit(1,"npc") - unit(2, "mm"),
            y= unit(2, "mm"),
            just=c("right", "bottom"),
            gp=gpar(cex= size, col=color))
  popViewport()
}


pdf("Karte.pdf", width = 9, height = 9)
plot(p)
makeFootnote(refs)
dev.off()

png("Karte.png", width = 9, height = 9, units = "in", res = 150)
plot(p)
makeFootnote(refs)
dev.off()