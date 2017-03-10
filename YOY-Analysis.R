TerristsGDPData=read.csv(file.choose())
yearWiseData$cumTotal_kill=cumsum(yearWiseData$Total_Kill)
yearWiseData$cumTotal_Wound=cumsum(yearWiseData$Total_Wound)
yearWiseData$cumTotal_Success=cumsum(yearWiseData$Total_Success)
yearWiseData$cumTotal_Incidents=cumsum(yearWiseData$Total_Incidents)
yearWiseData$cumPerc_kill= signif(yearWiseData$cumTotal_kill[1:length(yearWiseData$cumTotal_kill)]/sum(as.numeric(yearWiseData$Total_Kill))*100,4)
yearWiseData$cumPerc_Wound= signif(yearWiseData$cumTotal_Wound[1:length(yearWiseData$cumTotal_Wound)]/sum(as.numeric(yearWiseData$Total_Wound))*100,4)
yearWiseData$cumPerc_Success= signif(yearWiseData$cumTotal_Success[1:length(yearWiseData$cumTotal_Success)]/sum(as.numeric(yearWiseData$Total_Success))*100,4)
yearWiseData$cumPerc_Incidents= signif(yearWiseData$cumTotal_Incidents[1:length(yearWiseData$cumTotal_Incidents)]/sum(as.numeric(yearWiseData$Total_Incidents))*100,4)


library(dplyr)
library(plyr)
library(plotly)
yearWiseData=ddply(TerristsGDPData,.(iyear),summarise,Total_Kill=sum(na.omit(nkill)), Total_Wound=sum(na.omit(nwound)), Total_Success=sum(na.omit(success)), Total_Incidents=length(na.omit(eventid)), Total_Kill_Avg=mean(na.omit(nkill)), Total_Wound_Avg=mean(na.omit(nwound)))
plot_ly(yearWiseData) %>%
  add_lines(alpha = 1, x = ~iyear, y = ~Total_Incidents, name = "Total Incidents in a Year") %>%
  add_lines(alpha = 1,x = ~iyear, y = ~Total_Kill, name = "Total Killed in a Year", line=list (color='Red')) %>%
  add_lines(alpha = .50,x = ~iyear, y = ~Total_Wound, name = "Total Wounded in a Year") %>%
  add_lines(alpha = .50, x = ~iyear, y = ~Total_Success, name = "Total Successful attacks in a Year") %>%
  layout(title = "Terrorism Incidents Over Time",
         xaxis = list(title = "Year"),
         yaxis = list(title = "Count"))

s <- 
  "A       B        C       G       Xax
0.451   0.333   0.034   0.173   0.22        
0.491   0.270   0.033   0.207   0.34    
0.389   0.249   0.084   0.271   0.54    
0.425   0.819   0.077   0.281   0.34
0.457   0.429   0.053   0.386   0.53    
0.436   0.524   0.049   0.249   0.12    
0.423   0.270   0.093   0.279   0.61    
0.463   0.315   0.019   0.204   0.23
"
d <- read.delim(textConnection(s), sep="")
summary(d)
library(plyr)
library(reshape2)

library(ggplot2)
library(reshape2)
d <- melt(d, id.vars="Xax")

# Everything on the same plot
ggplot(d, aes(Xax,value, col=variable)) + 
  geom_point() + 
  stat_smooth() 

# Separate plots
ggplot(d, aes(Xax,value)) + 
  geom_point() + 
  stat_smooth() +
  facet_wrap(~variable)


USPersonalExpenditure <- data.frame("Categorie"=rownames(USPersonalExpenditure), USPersonalExpenditure)
summary(USPersonalExpenditure)
head()



install.packages("leaflet")
library(leaflet)
map<-leaflet()
map
map<- leaflet()%>%addTiles()
map7<-leaflet(GTD)%>%addTiles()%>%
  addMarkers(clusterOptions=markerClusterOptions())
addCircleMarkers(lng=GTD$latitude,lat=GTD$longitude)
map7
saveWidget(map7,file="map7.html",selfcontained=F)
display_html(paste("<iframe src=",'map7.html'," ',width='100%',"/>))



 attachWiseYearly=ddply(TerristsGDPData,.(iyear, attacktype1_txt),summarise,Total_Kill=sum(na.omit(nkill)), Total_Wound=sum(na.omit(nwound)), Total_Success=sum(na.omit(success)), Total_Incidents=length(na.omit(eventid)), Total_Kill_Avg=mean(na.omit(nkill)), Total_Wound_Avg=mean(na.omit(nwound)))
 attachWiseYearly$cumTotal_kill=cumsum(attachWiseYearly$Total_Kill)
 attachWiseYearly$cumTotal_Wound=cumsum(attachWiseYearly$Total_Wound)
 attachWiseYearly$cumTotal_Success=cumsum(attachWiseYearly$Total_Success)
 attachWiseYearly$cumTotal_Incidents=cumsum(attachWiseYearly$Total_Incidents)
 attachWiseYearly$cumPerc_kill= signif(attachWiseYearly$cumTotal_kill[1:length(attachWiseYearly$cumTotal_kill)]/sum(as.numeric(attachWiseYearly$Total_Kill))*100,4)
 attachWiseYearly$cumPerc_Wound= signif(attachWiseYearly$cumTotal_Wound[1:length(attachWiseYearly$cumTotal_Wound)]/sum(as.numeric(attachWiseYearly$Total_Wound))*100,4)
 attachWiseYearly$cumPerc_Success= signif(attachWiseYearly$cumTotal_Success[1:length(attachWiseYearly$cumTotal_Success)]/sum(as.numeric(attachWiseYearly$Total_Success))*100,4)
 attachWiseYearly$cumPerc_Incidents= signif(attachWiseYearly$cumTotal_Incidents[1:length(attachWiseYearly$cumTotal_Incidents)]/sum(as.numeric(attachWiseYearly$Total_Incidents))*100,4)
dim(attachWiseYearlyData)
names(attachWiseDataPivot)
edit(attachWiseDataPivot)
 attachWiseDataPivot=tidyr::spread(attachWiseYearly, attacktype1_txt, Total_Kill, fill=0)
 colnames(attachWiseDataPivot)[7:15]=c("Armed_Assault", "Assassination", "Bombing_Explosion", "Facility_Infrastructure_Attack", "Hijacking", "Hostage_Taking_Barricade_Incident", "Hostage_Taking_Kidnapping", "Unarmed_Assault", "UnKnown")

library(dplyr)
 library(plotly)
 library(ggplot2)
 plot_ly(attachWiseDataPivot) %>%
   add_bars(alpha = 2, x = ~iyear, y = ~Armed_Assault, name = "Victims of Armed Assault") %>%
   add_bars(alpha = 3, x = ~iyear, y = ~Bombing_Explosion, name = "Victims of Bomb Explosions") %>%   
   add_bars(alpha = 1, x = ~iyear, y = ~Assassination, name = "Victims of Assassination") %>%   
   add_bars(alpha = 4, x = ~iyear, y = ~Facility_Infrastructure_Attack, name = "Victims of Facility Infrastructure Attack") %>%   
   add_bars(alpha = 1, x = ~iyear, y = ~Hijacking, name = "Victims of Hijacking") %>%   
   add_bars(alpha = 1, x = ~iyear, y = ~Hostage_Taking_Barricade_Incident, name = "Victims of Hostage Taking Barricade") %>%   
   add_bars(alpha = 1, x = ~iyear, y = ~Hostage_Taking_Kidnapping, name = "Victims of Hostage Taking Kidnapping") %>%   
   add_bars(alpha = 1, x = ~iyear, y = ~Unarmed_Assault, name = "Victims of Unarmed Assault") %>%
   add_bars(alpha = 1, x = ~iyear, y = ~UnKnown, name = "Victims of UnKnown") %>%      
   layout(title = "Year on Year Attack Wise Victims",
          xaxis = list(title = "Year"),
          yaxis = list(title = "Attack Victims"))

 plot_ly(x = ~attachWiseDataPivot$iyear, y = ~attachWiseDataPivot$Armed_Assault, type = 'scatter', mode = 'lines', name = 'Armed Assault', fill = 'tozeroy') %>%
   add_trace(y = ~attachWiseDataPivot$Bombing_Explosion, name = 'Bombing Explosion', fill = 'tozeroy') %>%
   add_trace(y = ~attachWiseDataPivot$Assassination, name = 'Assassination', fill = 'tozeroy') %>%
   add_trace(y = ~attachWiseDataPivot$Facility_Infrastructure_Attack, name = 'Facility Infrastructure Attack', fill = 'tozeroy') %>%
   add_trace(y = ~attachWiseDataPivot$Hijacking, name = 'Hijacking', fill = 'tozeroy') %>%
   add_trace(y = ~attachWiseDataPivot$Hostage_Taking_Barricade_Incident, name = 'Hostage Taking Barricade Incident', fill = 'tozeroy') %>%
   add_trace(y = ~attachWiseDataPivot$Hostage_Taking_Kidnapping, name = 'Hostage Taking Kidnapping', fill = 'tozeroy') %>%
   add_trace(y = ~attachWiseDataPivot$UnKnown, name = 'UnKnown', fill = 'tozeroy') %>%
   layout(title = "High and Low Temperatures in New York",
          paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
          xaxis = list(title = "Years",
                       gridcolor = 'rgb(255,255,255)',
                       showgrid = TRUE,
                       showline = FALSE,
                       showticklabels = TRUE,
                       tickcolor = 'rgb(127,127,127)',
                       ticks = 'outside',
                       zeroline = FALSE),
          yaxis = list(title = "Attack Victims",
                       gridcolor = 'rgb(255,255,255)',
                       showgrid = TRUE,
                       showline = FALSE,
                       showticklabels = TRUE,
                       tickcolor = 'rgb(127,127,127)',
                       ticks = 'outside',
                       zeroline = FALSE))
 
# AttackWise Summary of number of Kills in terms of percentage and its frequency
 unique(unlist(TerristsData$attacktype2_txt))
 level(unlist(TerristsData$attacktype2_txt))
 attachWiseData=ddply(TerristsData,.(attacktype1_txt),summarise,Total_Kill=sum(na.omit(nkill))
#                      , Total_Wound=sum(na.omit(nwound)), Total_Success=sum(na.omit(success)), Total_Incidents=length(na.omit(eventid)), Total_Kill_Avg=mean(na.omit(nkill)), Total_Wound_Avg=mean(na.omit(nwound))
                      )
 attachWiseData$cumTotal_kill=cumsum(attachWiseData$Total_Kill)
 attachWiseData$cumTotal_Wound=cumsum(attachWiseData$Total_Wound)
 attachWiseData$cumTotal_Success=cumsum(attachWiseData$Total_Success)
 attachWiseData$cumTotal_Incidents=cumsum(attachWiseData$Total_Incidents)
 attachWiseData$cumPerc_kill= signif(attachWiseData$cumTotal_kill[1:length(attachWiseData$cumTotal_kill)]/sum(as.numeric(attachWiseData$Total_Kill))*100,4)
 attachWiseData$cumPerc_Wound= signif(attachWiseData$cumTotal_Wound[1:length(attachWiseData$cumTotal_Wound)]/sum(as.numeric(attachWiseData$Total_Wound))*100,4)
 attachWiseData$cumPerc_Success= signif(attachWiseData$cumTotal_Success[1:length(attachWiseData$cumTotal_Success)]/sum(as.numeric(attachWiseData$Total_Success))*100,4)
 attachWiseData$cumPerc_Incidents= signif(attachWiseData$cumTotal_Incidents[1:length(attachWiseData$cumTotal_Incidents)]/sum(as.numeric(attachWiseData$Total_Incidents))*100,4)

 attachWiseData$Perc_kill= attachWiseData$Total_Kill[1:length(attachWiseData$Total_Kill)]/ sum(as.numeric(attachWiseData$Total_Kill))  * 100
                              

library("plotly") 

plot_ly(attachWiseData, labels = ~attacktype1_txt, values = ~Perc_kill, type = 'pie') %>%
  layout(title = 'Attack Type % Killings',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


countryWiseData=ddply(TerristsData,.(iyear, country_txt),summarise,Total_Kill=sum(na.omit(nkill)), Total_Wound=sum(na.omit(nwound)), Total_Success=sum(na.omit(success)), Total_Incidents=length(na.omit(eventid)), Total_Kill_Avg=mean(na.omit(nkill)), Total_Wound_Avg=mean(na.omit(nwound)))
countryWiseData=countryWiseData[with(countryWiseData, order(iyear,-Total_Kill, -Total_Wound)), ]
countryWiseData=ddply(countryWiseData, "iyear", function(x) head(x, 10))
countryWiseData$cumTotal_kill=cumsum(countryWiseData$Total_Kill)
countryWiseData$cumTotal_Wound=cumsum(countryWiseData$Total_Wound)
countryWiseData$cumTotal_Success=cumsum(countryWiseData$Total_Success)
countryWiseData$cumTotal_Incidents=cumsum(countryWiseData$Total_Incidents)
countryWiseData$cumPerc_kill= signif(countryWiseData$cumTotal_kill[1:length(countryWiseData$cumTotal_kill)]/sum(as.numeric(countryWiseData$Total_Kill))*100,4)
countryWiseData$cumPerc_Wound= signif(countryWiseData$cumTotal_Wound[1:length(countryWiseData$cumTotal_Wound)]/sum(as.numeric(countryWiseData$Total_Wound))*100,4)
countryWiseData$cumPerc_Success= signif(countryWiseData$cumTotal_Success[1:length(countryWiseData$cumTotal_Success)]/sum(as.numeric(countryWiseData$Total_Success))*100,4)
countryWiseData$cumPerc_Incidents= signif(countryWiseData$cumTotal_Incidents[1:length(countryWiseData$cumTotal_Incidents)]/sum(as.numeric(countryWiseData$Total_Incidents))*100,4)
countryWiseData=countryWiseData[with(countryWiseData, order(-Total_Kill, -Total_Wound)), ]
head(countryWiseData, 50)
edit(countryWiseData)

plot_ly(countryWiseData, x = ~iyear, y = ~country_txt, type = 'bar', name = 'Year' , text=country_txt) %>%
  add_trace(y = ~Total_Wound, name = 'Wound Count', text = country_txt) %>%
  layout(yaxis = list(title = 'Count'), barmode = 'group')

plot_ly(countryWiseData, x = ~iyear, y = ~country_txt, type = 'bar', name = 'SF Zoo') %>%
  #add_trace(y = ~Total_Kill, name = 'LA Zoo') %>%
  layout(yaxis = list(title = 'Count'), barmode = 'stack')


region_attack <- data.frame(table(countryWiseData$iyear,countryWiseData$country_txt, countryWiseData$))
head(region_attack)

plot_ly(region_attack,x=~Var2,y=~Freq,color = ~Var1,type = "bar") %>%
  layout(title = "Attack Types by Region",
         xaxis = list(title = "Region"),
         yaxis = list(title = "Count"),
         margin = list(b = 160,
                       t = 100,
                       pad = 10),
         barmode = "stack",
         legend = list(x = 0.02, y = 0.98))

edit(TerristsData$summary)

attachWiseDataPivot=tidyr::spread(attachWiseYearlyData, attacktype1_txt, Total_Kill, fill=0)
colnames(attachWiseDataPivot)[7:15]=c("Armed_Assault", "Assassination", "Bombing_Explosion", "Facility_Infrastructure_Attack", "Hijacking", "Hostage_Taking_Barricade_Incident", "Hostage_Taking_Kidnapping", "Unarmed_Assault", "UnKnown")

install.packages("tm")
install.packages("SnowballC")
install.packages("wordcloud")
install.packages("RColorBrewer")
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

wordcloud(TerristsData$summary,max.words=1000,random.order=FALSE,colors=brewer.pal(8, "Dark2"))

TerristsData$property
TerristsData$nkill
TerristsData$nsuccess
summary(TerristsData$suicide)

  
  # Frequnecy Table
  # Bi-Variate Analysis (nKill, nSuccess)
  # Bi-Variate Analysis (nKill, nwound, nSuccess, nfailure)  
  # k-means cluster - High Risk, Low Risk, Moderate Risk
  # 

  
library(dplyr)
library(plyr)
cityWiseData = ddply(TerristsData,.(iyear, city), summarise, Total_Kill=sum(na.omit(nkill)), Total_Wound=sum(na.omit(nwound)), Total_Success=sum(na.omit(success)), Total_Incidents=length(na.omit(eventid)), Total_Kill_Avg=mean(na.omit(nkill)), Total_Wound_Avg=mean(na.omit(nwound)))
cityWiseData$cumTotal_kill=cumsum(cityWiseData$Total_Kill)
cityWiseData$cumTotal_Wound=cumsum(cityWiseData$Total_Wound)
cityWiseData$cumTotal_Success=cumsum(cityWiseData$Total_Success)
cityWiseData$cumTotal_Incidents=cumsum(cilongitudetyWiseData$Total_Incidents)
cityWiseData$cumPerc_kill= signif(cityWiseData$cumTotal_kill[1:length(cityWiseData$cumTotal_kill)]/sum(as.numeric(cityWiseData$Total_Kill))*100,4)
cityWiseData$cumPerc_Wound= signif(cityWiseData$cumTotal_Wound[1:length(cityWiseData$cumTotal_Wound)]/sum(as.numeric(cityWiseData$Total_Wound))*100,4)
cityWiseData$cumPerc_Success= signif(cityWiseData$cumTotal_Success[1:length(cityWiseData$cumTotal_Success)]/sum(as.numeric(cityWiseData$Total_Success))*100,4)
cityWiseData$cumPerc_Incidents= signif(cityWiseData$cumTotal_Incidents[1:length(cityWiseData$cumTotal_Incidents)]/sum(as.numeric(cityWiseData$Total_Incidents))*100,4)

length(str(TerristsGDPData))

GT01= TerristsGDPData[,c("iyear", "city", "country_txt", "latitude","longitude", "attacktype1_txt", "targtype1_txt", "targsubtype1_txt", "target1", "weaptype1_txt","weapsubtype1_txt", "gname", "nkill", "nwound")]
head(TerristsGDPData)

GT01[GT01==""] <- NA
GT01 = na.omit(GT01)
mymap <- 
  leaflet() %>% 
  addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png') %>%
  setView(15, 40, zoom= 2)


mymap %>% addCircles (data=GT01, lat= ~latitude, lng = ~longitude, 
                      popup=paste( "<strong>Year: </strong>", GT01$iyear, "<br><strong>City: </strong>", GT01$city, "<br><strong>Country: </strong>", GT01$country_txt, "<br><strong>Attack type: </strong>", GT01$attacktype1_txt, 
                                   "<br><strong>Target: </strong>", GT01$targtype1_txt, " | ", GT01$targsubtype1_txt, " | ", GT01$target1, "<br><strong>Weapon: </strong>", GT01$weaptype1_txt, "<br><strong>Group: </strong>", GT01$gname, 
                                   "<br><strong>No of Victims: </strong>", GT01$nkill, "<br><strong>No of Wounds: </strong>", GT01$nwound), 
                      weight = 0.4, color="#8B1A1A", stroke = TRUE, fillOpacity = 0.6)


GT02= TerristsGDPData[,c("iyear", "city", "country_txt", "latitude","longitude", "attacktype1_txt", "targtype1_txt", "targsubtype1_txt", "target1", "weaptype1_txt","weapsubtype1_txt", "gname", "nkill", "nwound", "City_GDP", "City_Population")]
mymap  %>%  addCircles (data=GT02, lat= ~latitude, lng = ~longitude, 
                        popup=paste( "<strong>Year: </strong>", GT02$iyear, "<br><strong>City: </strong>", GT02$city, "<br><strong>Country: </strong>", GT01$country_txt, "<br><strong>Attack type: </strong>", GT02$attacktype1_txt, 
                                     "<br><strong>Target: </strong>", GT02$targtype1_txt, " | ", GT02$targsubtype1_txt, " | ", GT02$target1, "<br><strong>Weapon: </strong>", GT02$weaptype1_txt, "<br><strong>Group: </strong>", GT02$gname, 
                                     "<br><strong>Population: </strong>", GT02$City_Population, "<br><strong>GDP: </strong>", GT02$City_GDP),
                        weight = 0.4, color="#8B1A1A", stroke = TRUE, fillOpacity = 0.6)

TerristsGDPData=read.table(file.choose(), header=T, sep=",")
TerristsGDPData=read.csv(file.choose())
library(dplyr)
countryWiseData=ddply(TerristsGDPData,.(country_txt), summarise, Total_Incidents=length(na.omit(eventid)), Total_Kills=length(na.omit(nkill)))
countryWiseData=countryWiseData[with(countryWiseData, order(-Total_Incidents)), ]
target3=ddply(TerristsGDPData,.(target3), summarise, Total_Incidents=length(na.omit(eventid)))
unique(unlist(TerristsGDPData$city))
write.csv(motive, file.choose())
countryWiseData


library(rgdal)
library(rgeos)
library(ggplot2)
library(httr)

url <- "https://gist.githubusercontent.com/hrbrmstr/91ea5cc9474286c72838/raw/f3fde312c9b816dff3994f39f2bcda03209eff8f/continents.json"
stop_for_status(GET(url, write_disk("d://continents.json")))
continents <- readOGR("d://continents.json", "OGRGeoJSON")
continents_map <- fortify(continents, region="CONTINENT")

world <- map_data('world') %>% data.table()
world <- world[region!='Antarctica',]
str(world)
world
str(continents_map)
subset(continents_map, id %% "America")

unique(unlist(continents_map$id))

regionWiseData = ddply(TerristsGDPData,.(region_txt), summarise, Total_Kill=sum(na.omit(nkill)))

str(regionWiseData)
head(regionWiseData)
worldRegion=function(x) { if (grepl( "Australasia & Oceania", x)) {  "Australia"} else if (grepl("Central Asia", x) | grepl("East Asia", x)) {  "Asia" } 
}

regionWiseData=edit (regionWiseData)
regionWiseData
worldRegionWiseData=ddply(regionWiseData,.(region_txt), summarise, Total_Kill=sum(na.omit(Total_Kill)))

str(worldRegionWiseData)

worldRegionWiseData

data <- read.table(text="id value
                   Europe 340
                   Africa 300
                   Oceania 700
                   Asia 100
                   Australia 130", header=TRUE, stringsAsFactors=FALSE)
str(continents_map)
str(data)
data
edit(data)
write.table(data, "d://dd.csv", sep=",")
data=read.table("d://dd.csv", sep=",",  header = TRUE)
gg <- ggplot()
gg <- gg + geom_map(data=continents_map,
                    map=continents_map,
                    aes(x=long, y=lat, map_id=id),
                    color="red")
gg <- gg + geom_map(data=worldRegionWiseData,
                    map=continents_map,
                    aes(map_id=id, fill=Total_Kill),
                    color="green")
gg <- gg + scale_fill_distiller("PuBu") # needs latest ggplot2
gg <- gg + coord_equal()
gg <- gg + theme_bw()
gg <- gg + labs(x=NULL, y=NULL)
gg <- gg + theme(panel.border=element_blank())
gg <- gg + theme(panel.grid=element_blank())
gg


plot_ly(worldRegionWiseData, labels = ~id, values = ~Total_Kill, type = 'pie') %>%
  layout(title = 'Region Wise Total Killings',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))




regionAttackType=ddply(TerristsGDPData,.(region_txt, attacktype1_txt), summarise, Total_Kill=sum(na.omit(nkill)))
regionAttackTypePivot=tidyr::spread(regionAttackType, attacktype1_txt, Total_Kill, fill=0)
regionAttackTypePivot
names(regionAttackTypePivot)
colnames(regionAttackTypePivot)[2:10]=c("Armed_Assault", "Assassination", "Bombing_Explosion", "Facility_Infrastructure_Attack", "Hijacking", "Hostage_Taking_Barricade_Incident", "Hostage_Taking_Kidnapping", "Unarmed_Assault", "UnKnown")
p <- plot_ly(regionAttackTypePivot, x = ~region_txt, y = ~Armed_Assault, type = 'bar', name = 'Armed Assault') %>%
  add_trace(y = ~Bombing_Explosion, name = 'Bombing_Explosion') %>%
  add_trace(y = ~Facility_Infrastructure_Attack, name = 'Facility_Infrastructure_Attack') %>%
  add_trace(y = ~Hijacking, name = 'Hijacking') %>%
  add_trace(y = ~Hostage_Taking_Barricade_Incident, name = 'Hostage_Taking_Barricade_Incident') %>%
  add_trace(y = ~Unarmed_Assault, name = 'Unarmed_Assault') %>%
  add_trace(y = ~Hostage_Taking_Kidnapping, name = 'Hostage_Taking_Kidnapping') %>%
  add_trace(y = ~UnKnown, name = 'UnKnown') %>%
  layout(yaxis = list(title = 'Count'), barmode = 'stack')
p

regionAttackTypePivot




countryWiseOverTimeData=ddply(TerristsGDPData,.(country_txt, iyear),summarise,Total_Kills=sum(na.omit(nkill)),  Total_Success=sum(na.omit(success)),  Total_Incidents=length(na.omit(eventid)), Total_Wounds=sum(na.omit(nwound)))
countryWiseData=ddply(countryWiseOverTimeData,.(country_txt),summarise,Total_Kills=sum(na.omit(Total_Kills)),  Total_Success=sum(na.omit(Total_Success)),  Total_Incidents=sum(na.omit(Total_Incidents)), Total_Wounds=sum(na.omit(Total_Wounds)))
countryWiseData=countryWiseData[with(countryWiseData, order(-Total_Kills)), ]
head(countryWiseData)
top6MostAffectedCountries=subset(countryWiseOverTimeData, country_txt %in% c("Iraq", "Pakistan", "India", "Afganistan", "Nigeria", "Sri Lanka"))
dim(top6MostAffectedCountries)
unique(unlist(subset(countryWiseOverTimeData, country_txt %in% c("Iraq", "Pakistan", "India", "Afganistan", "Nigeria", "Sri Lanka"))$country_txt))
str(top6MostAffectedCountries)
length(top6MostAffectedCountries$iyear)
head(top6MostAffectedCountries)

p1 <- plot_ly(subset(top6MostAffectedCountries, country_txt %in% c("Iraq"))) %>%
    add_lines(alpha = 1, x = ~iyear, y = ~Total_Incidents, name = "Incidents in Iraq") %>%
    add_lines(alpha = 1,x = ~iyear, y = ~Total_Kills, name = "Killed in Iraq", line=list (color='Red')) %>%
    add_lines(alpha = .50,x = ~iyear, y = ~Total_Wounds, name = "Wounded in Iraq") %>%
    add_lines(alpha = .50, x = ~iyear, y = ~Total_Success, name = "Successful attacks in Iraq") %>%
    layout(title = "Terrorism Incidents Over Time",
           xaxis = list(title = "Year"),
           yaxis = list(title = "Iraq"))
  
p2 <- plot_ly(subset(top6MostAffectedCountries, country_txt %in% c("Pakistan"))) %>%
  add_lines(alpha = 1, x = ~iyear, y = ~Total_Incidents, name = "Incidents in Pakistan") %>%
  add_lines(alpha = 1,x = ~iyear, y = ~Total_Kills, name = "Killed in Pakistan", line=list (color='Red')) %>%
  add_lines(alpha = .50,x = ~iyear, y = ~Total_Wounds, name = "Wounded in Pakistan") %>%
  add_lines(alpha = .50, x = ~iyear, y = ~Total_Success, name = "Successful attacks in Pakistan") %>%
  layout(yaxis = list(title = "Pakistan"))

p3 <- plot_ly(subset(top6MostAffectedCountries, country_txt %in% c("India"))) %>%
  add_lines(alpha = 1, x = ~iyear, y = ~Total_Incidents, name = "Incidents in India") %>%
  add_lines(alpha = 1,x = ~iyear, y = ~Total_Kills, name = "Killed in India", line=list (color='Red')) %>%
  add_lines(alpha = .50,x = ~iyear, y = ~Total_Wounds, name = "Wounded in India") %>%
  add_lines(alpha = .50, x = ~iyear, y = ~Total_Success, name = "Successful attacks in India") %>%
  layout(yaxis = list(title = "India"))

  
p4 <- plot_ly(subset(top6MostAffectedCountries, country_txt %in% c("Nigeria"))) %>%
  add_lines(alpha = 1, x = ~iyear, y = ~Total_Incidents, name = "Incidents in Nigeria") %>%
  add_lines(alpha = 1,x = ~iyear, y = ~Total_Kills, name = "Killed in Nigeria", line=list (color='Red')) %>%
  add_lines(alpha = .50,x = ~iyear, y = ~Total_Wounds, name = "Wounded in Nigeria") %>%
  add_lines(alpha = .50, x = ~iyear, y = ~Total_Success, name = "Successful attacks in Nigeria") %>%
  layout(yaxis = list(title = "Nigeria"))

  
p5 <- plot_ly(subset(top6MostAffectedCountries, country_txt %in% c("Sri Lanka"))) %>%
  add_lines(alpha = 1, x = ~iyear, y = ~Total_Incidents, name = "Incidents in Sri Lanka") %>%
  add_lines(alpha = 1,x = ~iyear, y = ~Total_Kills, name = "Killed in Sri Lanka", line=list (color='Red')) %>%
  add_lines(alpha = .50,x = ~iyear, y = ~Total_Wounds, name = "Wounded in Sri Lanka") %>%
  add_lines(alpha = .50, x = ~iyear, y = ~Total_Success, name = "Successful attacks in Sri Lanka") %>%
  layout(yaxis = list(title = "Sri Lanka"))


p <- subplot(p1, p2, p3, p4, p5, nrows=3, margin=0.05)
p
?subplot

TerristsGDPData$targtype1
targetType=ddply(TerristsGDPData,.(target1),summarise,Total_Kills=sum(na.omit(nkill)),  Total_Success=sum(na.omit(success)),  Total_Incidents=length(na.omit(eventid)), Total_Wounds=sum(na.omit(nwound)))
summary(TerristsGDPData$target1)
top10targetType=targetType[with(target1, order(-Total_Kills)), ]
head(top10targetType)



## Clustering
cityWiseData = ddply(TerristsGDPData,.(city),
                     summarise, Total_Kill=round(sum(na.omit(nkill),0)),
                     Total_Wound=round(sum(na.omit(nwound),0)),
                     Total_Success=round(sum(na.omit(success),0)),
                     Total_Incidents=length(na.omit(eventid)),
                     Total_PropertyDamage=sum(na.omit(propvalue))
)
perData <- read.csv("Viewpoints on Terrorism.csv",header = T,stringsAsFactors = F)

head(perData)
colnames(perData) <- c("Timestamp","Gender","Age" ,"City","Profession","Education","Spread","Emotion_Unbalance","Places","Holiday","Counter","Combat","Equiped","Safe")
colnames(perData)
perData$Age <- factor(perData$Age)
perData$Gender <- factor(perData$Gender)
perData$City <- factor(perData$City)
perData$Profession <- factor(perData$Profession)
perData$Education <- factor(perData$Education)
summary(perData)
summary(percGraphData)

Age_per<-data.frame(table(perData$Age))
table(perData$Gender)
table(perData$City)
table(perData$Profession)
table(perData$Education)
table(perData$Spread)
table(perData$Emotion_Unbalance)
table(perData$Places)
table(perData$Holiday)
table(perData$Counter)
table(perData$Combat)
table(perData$Equiped)
table(perData$Safe)


library("stringi")
length(dataSummary)

dataSummary
dataSummary=capture.output(str(TerristsGDPData))
x = head(dataSummary, 1)
stri_locate(dataSummary, regex="\r", mode=c("first"))
x
head(dataSummary, 1)
dd = stri_locate(x, regex="obs. of", mode=c("first"))[[2]]
dd

substr(x, dd+3, 100)
install.packages("stringi")
library(stringi)

?substr
substr

# Commands to find total variables in a dataset
dataSummary=capture.output(str(TerristsGDPData))
x = head(dataSummary, 1)
dd = stri_locate(x, regex="obs. of", mode=c("first"))[[2]]
substr(x, dd+1, 100)
library(dplyr)
library(plyr)
library(car)
  yearWiseData=ddply(TerristsData,.(iyear),summarise,Total_Kill=sum(na.omit(nkill))
                     , Total_Wound=sum(na.omit(nwound)), 
                     Total_Success=sum(na.omit(success))
                     , Total_Incidents=length(na.omit(eventid))
                     , 
                     Total_nwoundus=sum(na.omit(nwoundus)),  
                     Total_nkillus=sum(na.omit(nkillus))
                     , Total_nwoundus=sum(na.omit(nwoundus)), 
                     Total_nkillter=sum(na.omit(nkillter))
                     , Total_nwoundte=sum(na.omit(nwoundte))
                     , Total_propvalue=sum(na.omit(nkillter)) 
                     )

cor(yearWiseData$Total_Kill, yearWiseData$Total_Wound
)

countryWiseData=ddply(TerristsData,.(country_txt),summarise,Total_Kill=sum(na.omit(nkill))
                   , Total_Wound=sum(na.omit(nwound)), 
                   Total_Success=sum(na.omit(success))
                   , Total_Incidents=length(na.omit(eventid))
                   , 
                   Total_nwoundus=sum(na.omit(nwoundus)),  
                   Total_nkillus=sum(na.omit(nkillus))
                   , Total_nwoundus=sum(na.omit(nwoundus)), 
                   Total_nkillter=sum(na.omit(nkillter))
                   , Total_nwoundte=sum(na.omit(nwoundte))
                   , Total_propvalue=sum(na.omit(nkillter)) 
)


CountryCityGDP = read.csv(file.choose())
CountryGDPPopulation = ddply(CountryCityGDP,.(country),summarise,Total_GDP=sum(na.omit(GDP)), Total_pop=sum(na.omit(pop)))
str(CountryCityGDP)
str(CountryGDPPopulation)
write.csv(CountryGDPPopulation, file.choose())
yearWiseData$cumTotal_kill=cumsum(yearWiseData$Total_Kill)
yearWiseData$cumTotal_Wound=cumsum(yearWiseData$Total_Wound)
yearWiseData$cumTotal_Success=cumsum(yearWiseData$Total_Success)
yearWiseData$cumTotal_Incidents=cumsum(yearWiseData$Total_Incidents)
yearWiseData$cumPerc_kill= signif(yearWiseData$cumTotal_kill[1:length(yearWiseData$cumTotal_kill)]/sum(as.numeric(yearWiseData$Total_Kill))*100,4)
yearWiseData$cumPerc_Wound= signif(yearWiseData$cumTotal_Wound[1:length(yearWiseData$cumTotal_Wound)]/sum(as.numeric(yearWiseData$Total_Wound))*100,4)
yearWiseData$cumPerc_Success= signif(yearWiseData$cumTotal_Success[1:length(yearWiseData$cumTotal_Success)]/sum(as.numeric(yearWiseData$Total_Success))*100,4)
yearWiseData$cumPerc_Incidents= signif(yearWiseData$cumTotal_Incidents[1:length(yearWiseData$cumTotal_Incidents)]/sum(as.numeric(yearWiseData$Total_Incidents))*100,4)


anova(yearWiseData)
dataSummary
is.na(0)

removeNa=function (x)
{
  if (is.na(x))
  {
     0;
  }
  else
  {
    x;
  }
}

removeNa(10)
# All related variables with respect to damage
ifelse(is.na(TerristsGDPData$nkill)==1, 0, TerristsGDPData$nkill)
TerristsGDPDataPCA1=subset(TerristsGDPData,select=c(ifelse(is.na(nkill)=="", 0, nkill) , nwound, nkillus, nwoundus, nkillter, nwoundte, propvalue))
plot(TerristsGDPDataPCA1)
pc=princomp(formula=~ ., data = TerristsGDPDataPCA1, cor=TRUE)
pc
head(TerristsGDPDataPCA1)
?if
)#Call:
#  princomp(formula = ~., data = TerristsGDPDataPCA1, cor = TRUE)
#
#Standard deviations:
#   Comp.1    Comp.2    Comp.3    Comp.4    Comp.5    Comp.6    Comp.7 
#1.6258738 1.3010887 1.0487125 0.7918670 0.7054970 0.6484697 0.1364278 
#
#
#7  variables and  19847 observations.

pc$loadings
#Loadings:
#          Comp.1 Comp.2 Comp.3 Comp.4 Comp.5 Comp.6 Comp.7
#nkill     -0.247  0.369  0.606        -0.494  0.430       
#nwound    -0.373  0.224  0.498  0.145  0.660 -0.325       
#nkillus   -0.557 -0.184 -0.188 -0.341                0.709
#nwoundus  -0.556 -0.185 -0.187 -0.352               -0.705
#nkillter          0.628 -0.272        -0.363 -0.626       
#nwoundte          0.574 -0.455         0.386  0.558       
#propvalue -0.410 -0.150 -0.180  0.856 -0.193              

#Comp.1 Comp.2 Comp.3 Comp.4 Comp.5 Comp.6 Comp.7
#SS loadings     1.000  1.000  1.000  1.000  1.000  1.000  1.000
#Proportion Var  0.143  0.143  0.143  0.143  0.143  0.143  0.143
#Cumulative Var  0.143  0.286  0.429  0.571  0.714  0.857  1.000

summary(pc)
#Importance of components:
#  Comp.1    Comp.2   Comp.3     Comp.4     Comp.5     Comp.6      Comp.7
#Standard deviation     1.6258738 1.3010887 1.048712 0.79186699 0.70549703 0.64846969 0.136427804
#Proportion of Variance 0.3776379 0.2418331 0.157114 0.08957905 0.07110372 0.06007328 0.002658935
#Cumulative Proportion  0.3776379 0.6194710 0.776585 0.86616407 0.93726779 0.99734106 1.000000000


# All related variables with hostage kind of situation, excluding nhours, ndays, 
TerristsGDPDataPCA2=subset(TerristsGDPData,select=c(nhostkid, nhostkidus, ransomamt, ransomamtus, ransompaid, ransompaidus, nreleased))
pc=princomp(formula=~ ., data = TerristsGDPDataPCA2, cor=TRUE)
pc
plot(pc)
#Call:
#  princomp(formula = ~., data = TerristsGDPDataPCA2, cor = TRUE)
#
#Standard deviations:
#  Comp.1    Comp.2    Comp.3    Comp.4    Comp.5    Comp.6    Comp.7 
#1.1771013 1.1220029 1.0130886 1.0003644 0.9906991 0.8490646 0.7912455 
#
#7  variables and  385 observations.
pc$loadings
#Loadings:
#  Comp.1 Comp.2 Comp.3 Comp.4 Comp.5 Comp.6 Comp.7
#nhostkid     -0.162  0.683  0.169               -0.662  0.156
#nhostkidus                 -0.669        -0.737              
#ransomamt    -0.676 -0.199                       0.124  0.698
#ransomamtus  -0.659 -0.241                      -0.252 -0.664
#ransompaid           0.114 -0.478  0.748  0.419 -0.141       
#ransompaidus               -0.535 -0.657  0.520              
#nreleased    -0.285  0.649                       0.667 -0.208
#
#Comp.1 Comp.2 Comp.3 Comp.4 Comp.5 Comp.6 Comp.7
#SS loadings     1.000  1.000  1.000  1.000  1.000  1.000  1.000
#Proportion Var  0.143  0.143  0.143  0.143  0.143  0.143  0.143
#Cumulative Var  0.143  0.286  0.429  0.571  0.714  0.857  1.000

summary(pc)
#Importance of components:
#  Comp.1    Comp.2    Comp.3    Comp.4    Comp.5    Comp.6     Comp.7
#Standard deviation     1.1771013 1.1220029 1.0130886 1.0003644 0.9906991 0.8490646 0.79124547
#Proportion of Variance 0.1979382 0.1798415 0.1466212 0.1429613 0.1402121 0.1029872 0.08943848
#Cumulative Proportion  0.1979382 0.3777797 0.5244009 0.6673622 0.8075743 0.9105615 1.00000000




# All related variables with hostage kind of situation, including nhours, ndays, 
unique(unlist(TerristsGDPData$ndays))
TerristsGDPDataPCA2=subset(TerristsGDPData,select=c(nhostkid, nhostkidus, ransomamt, ransomamtus, ransompaid, ransompaidus, nreleased, nhours, ndays))
pc=princomp(formula=~ ., data = TerristsGDPDataPCA2)
pc

#Call:
#  princomp(formula = ~., data = TerristsGDPDataPCA2, cor = TRUE)
#
#Standard deviations:
#  Comp.1    Comp.2    Comp.3    Comp.4    Comp.5    Comp.6    Comp.7 
#1.1771013 1.1220029 1.0130886 1.0003644 0.9906991 0.8490646 0.7912455 
#
#7  variables and  385 observations.
pc$loadings
#Loadings:
#  Comp.1 Comp.2 Comp.3 Comp.4 Comp.5 Comp.6 Comp.7
#nhostkid     -0.162  0.683  0.169               -0.662  0.156
#nhostkidus                 -0.669        -0.737              
#ransomamt    -0.676 -0.199                       0.124  0.698
#ransomamtus  -0.659 -0.241                      -0.252 -0.664
#ransompaid           0.114 -0.478  0.748  0.419 -0.141       
#ransompaidus               -0.535 -0.657  0.520              
#nreleased    -0.285  0.649                       0.667 -0.208
#
#Comp.1 Comp.2 Comp.3 Comp.4 Comp.5 Comp.6 Comp.7
#SS loadings     1.000  1.000  1.000  1.000  1.000  1.000  1.000
#Proportion Var  0.143  0.143  0.143  0.143  0.143  0.143  0.143
#Cumulative Var  0.143  0.286  0.429  0.571  0.714  0.857  1.000

summary(pc)
#Importance of components:
#  Comp.1    Comp.2    Comp.3    Comp.4    Comp.5    Comp.6     Comp.7
#Standard deviation     1.1771013 1.1220029 1.0130886 1.0003644 0.9906991 0.8490646 0.79124547
#Proportion of Variance 0.1979382 0.1798415 0.1466212 0.1429613 0.1402121 0.1029872 0.08943848
#Cumulative Proportion  0.1979382 0.3777797 0.5244009 0.6673622 0.8075743 0.9105615 1.00000000

plot(pc)

library(devtools)
install.packages("ggbiplot")
library(devtools)
install_github("ggbiplot", "vqv/ggbiplot")
library(ggbiplot)

g <- ggbiplot(pc, obs.scale = 1, var.scale = 1, 
              groups = ir.species, ellipse = TRUE, 
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)

biplot(pc)
plot(pc)

head(pc,3)
head(pc$scores)
  
pc

PCbiplot <- function(PC, x="PC1", y="PC2") {
  # PC being a prcomp object
  data <- data.frame(obsnames=row.names(PC$x), PC$x)
  plot <- ggplot(data, aes_string(x=x, y=y)) + geom_text(alpha=.4, size=3, aes(label=obsnames))
  plot <- plot + geom_hline(aes(0), size=.2) + geom_vline(aes(0), size=.2)
  datapc <- data.frame(varnames=rownames(PC$rotation), PC$rotation)
  mult <- min(
    (max(data[,y]) - min(data[,y])/(max(datapc[,y])-min(datapc[,y]))),
    (max(data[,x]) - min(data[,x])/(max(datapc[,x])-min(datapc[,x])))
  )
  datapc <- transform(datapc,
                      v1 = .7 * mult * (get(x)),
                      v2 = .7 * mult * (get(y))
  )
  plot <- plot + coord_equal() + geom_text(data=datapc, aes(x=v1, y=v2, label=varnames), size = 5, vjust=1, color="red")
  plot <- plot + geom_segment(data=datapc, aes(x=0, y=0, xend=v1, yend=v2), arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, color="red")
  plot
}

library("ggplot2")
PCbiplot(pc)
colnames(pc)
str(pc)
rownames(pc$scores)

fit.df <- as.data.frame(pc$scores)
fit.df$state <- rownames(fit.df)
ggplot(data=fit.df,aes(x=Comp.1,y=Comp.2))+
  geom_text(aes(label=state,size=1,hjust=0,vjust=0))


unique(unlist(TerristsData$attacktype3_txt))

unique(unlist(TerristsData$subAttacktype2_txt))
str(TerristsGDPData)

CountryGDPPopulation

levels(TerristsGDPData$gsu)
subset(TerristsGDPData,select=c(ifelse(is.na(attacktype1_txt)=="", 0, nkill) , nwound, nkillus, nwoundus, nkillter, nwoundte, propvalue))

data(iris)
dat <- as.matrix(iris[,-5])
dat

subset(TerristsGDPData, select=c(attacktype1_txt + attacktype2_txt))


ddply(TerristsGDPData,.(iyear), , )
library("dplyr")
library("plyr")
library("data.table")
library("tidyr")
TerristsGDPDataTable=as.data.table(TerristsGDPData)
TerristsGDPDataTable[,iyear]
TerristsGDPDataTable[attacktype1_txt=="Hostage Taking (Barricade Incident)" & attacktype2_txt=="Hostage Taking (Barricade Incident)" & attacktype3_txt=="Hostage Taking (Barricade Incident)", ]
TerristsGDPDataTable[attacktype1_txt=="Assassination" & attacktype2_txt=="Assassination" & attacktype3_txt=="Assassination", ]
TerristsGDPDataTable[attacktype1_txt=="Hostage Taking (Kidnapping)" & attacktype2_txt=="Hostage Taking (Kidnapping)" & attacktype3_txt=="Hostage Taking (Kidnapping)", ]
TerristsGDPDataTable[attacktype1_txt=="Hostage Taking (Barricade Incident)" & attacktype2_txt=="Hostage Taking (Barricade Incident)" & attacktype3_txt=="Hostage Taking (Barricade Incident)", ]
TerristsGDPDataTable[attacktype1_txt=="Hostage Taking (Barricade Incident)" & attacktype2_txt=="Hostage Taking (Barricade Incident)" & attacktype3_txt=="Hostage Taking (Barricade Incident)", ]
TerristsGDPDataTable[attacktype1_txt=="Hostage Taking (Barricade Incident)" & attacktype2_txt=="Hostage Taking (Barricade Incident)" & attacktype3_txt=="Hostage Taking (Barricade Incident)", ]
TerristsGDPDataTable[attacktype1_txt=="Hostage Taking (Barricade Incident)" & attacktype2_txt=="Hostage Taking (Barricade Incident)" & attacktype3_txt=="Hostage Taking (Barricade Incident)", ]
TerristsGDPDataTable[attacktype1_txt=="Hostage Taking (Barricade Incident)" & attacktype2_txt=="Hostage Taking (Barricade Incident)" & attacktype3_txt=="Hostage Taking (Barricade Incident)", ]
TerristsGDPDataAttackType=spread(TerristsGDPData, attacktype1_txt, rowno)
unique(unlist(TerristsData$attacktype2_txt))
TerristsGDPData$rowno = 1:nrow(TerristsGDPData)
colnames(TerristsGDPData)
TerristsGDPData[2924, c("rowno", "attacktype1_txt", "attacktype2_txt")]
nrow(TerristsGDPData)

dd=data.frame(TerristsGDPData[, c("rowno", "attacktype1_txt", "attacktype2_txt")])

TerristsGDPData=reshape(TerristsGDPData, direction="wide", idvar=c("attacktype1_txt"), timevar="rowno")



summary(TerristsGDPData$nkill)
summary(TerristsGDPData$nwound)
summary(TerristsGDPData$propvalue)

# PCA - Final Round
countryWiseData=ddply(TerristsGDPData,.(country_txt),summarise,Total_Incidents=length(na.omit(eventid)), Total_Kill=sum(na.omit(nkill)), Total_Wound=sum(na.omit(nwound)),  Total_Propvalue=sum(na.omit(propvalue)))
colnames(countryWiseData)
countryWiseDataPCA=subset(countryWiseData, select=c(Total_Incidents, Total_Kill, Total_Wound, Total_Propvalue))
pcGTD = princomp(formula=~ ., data = countryWiseDataPCA, cor=TRUE)
pcGTD
pcGTD$loadings
summary(pcGTD)
pcGTD$loadings
plot(pcGTD)
biplot(pcGTD)
pcGTD$scores
length(pcGTD$scores[,4])
unique(unlist(TerristsGDPData$country_txt))

countryWiseData$Damage_Score=pcGTD$scores[,1]
countryWiseData$Property_Score=pcGTD$scores[,2]
dim(countryWiseDataPCA)
colnames(countryWiseData)
countryWiseData= countryWiseData[with(countryWiseData, order(-Damage_Score)), ]
head(countryWiseData)
countryWiseData$GTI_Cluster= ifelse(countryWiseData$GTI_Score>=5, "High", ifelse(countryWiseData$GTI_Score>=3 & countryWiseData$GTI_Score <5, "Medium", "Low" ))
head(countryWiseData$GTI_Cluster)

countryWiseData=countryWiseData[with(countryWiseData, order(-GTI_Score)), ]
head(countryWiseData)
write.csv(countryWiseData, file.choose())
library("dplyr")
library("plyr")
library("data.table")
library("tidyr")

regionWiseData = ddply(TerristsGDPData,.(iyear, region_txt), summarise, Total_Incidents=length(eventid), Total_Kill=sum(na.omit(nkill)))
regionWiseDataPivot=tidyr::spread(regionWiseData, region_txt, Total_Incidents, fill=0)
colnames(regionWiseDataPivot)
head(regionWiseDataPivot)
colnames(regionWiseDataPivot)=c("iyear", "Total_Kill", "Australasia_Oceania", "Central_America_Caribbean", "Central_Asia", "East_Asia", "Eastern_Europe", "Middle_East_North_Africa", "North_America", "South America", "South_Asia" , "Southeast_Asia", "Sub_Saharan_Africa", "Western Europe")

# IGNORE  -VE VALUES
# COUNTIES WHICH HAS VALUES FOR PROP Damage_Score
#   DIFFERERNT YEAR - INDIA
#     1975 - 2000 - 2015 HAS VALUE
#     AIM: 1975 * 1.4 AT THE LEVEL OF 2015
#     COUNTRY = indiA
#     YEAR = 1975, 2000, 2015
#     POPVALUE = X1, X2, X3
#     CURRENT 2015
#     BASE YEAR: 1975 - 2015 - 1.4 * X1
#     BASE YEAR: 2000 - 2015 - 1.1 * X2
#     2015 = X3
    
    # UNIQUE COUNTRY, YEAR, PROPVALUES
    
    
    
    
    install.packages("WDI")
    library(WDI)
    population=WDI(indicator='SP.POP.TOTL', country="all",start=1960, end=2016)
    str(population)
    poverty= WDI(indicator='SI.POV.2DAY', country="all",start=1960, end=2016)
    str(poverty)
    unique(unlist(poverty$country))
    wdi_data=WDI_data
    str(wdi_data)
    countries=wdi_data[[2]]
    str(countries)
    df = as.data.frame(countries)
    aa <- df$region != "Aggregates"
    countries_df <- df[aa,]
    str(countries_df)
    unique(unlist(countries_df$region))
    unique(unlist(TerristsGDPData$region_txt))
    
    
    
    library("dplyr")
    library("plyr")
    library("data.table")
    library("tidyr")
    #install.packages("GoogleMotionChart")
    library(googleVis)
    #library(GoogleMotionChart)
    TerroristGDPDataContinent=read.csv(file.choose())
    colnames(TerroristGDPDataContinent)
    TerroristGDPDataContinent$name
    TerroristGDPDataContinent$nkill<-as.numeric(TerroristGDPDataContinent$nkill)
    continentwisedata<-ddply(TerroristGDPDataContinent,.(Name,  iyear),summarise,Total_Incidents=length(eventid),Total_Kill=sum(na.omit(nkill)))
    head(continentwisedata)
    unique(colnames(continentwisedata))
    unique(continentwisedata$Name)
    
    gg<- gvisMotionChart(continentwisedata,
                         idvar = "Name",
                         timevar = "iyear")
    plot(gg)

    
    df=data.frame(country=c("US", "GB", "BR"), 
                  val1=c(10,13,14), 
                  val2=c(23,12,32))
    
    Line <- gvisLineChart(df)
    plot(Line)
    
    head(Fruits)
    
    Motion=gvisMotionChart(Fruits, 
                           idvar="Fruit", 
                           timevar="Year")
    plot(Motion)
    
    countryWiseData=ddply(TerristsGDPData,.(country_txt),summarise,Total_Incidents=length(na.omit(eventid)), Total_Kill=sum(na.omit(nkill)), Total_Wound=sum(na.omit(nwound)))
    colnames(countryWiseData)
    countryWiseDataPCA=subset(countryWiseData, select=c(Total_Incidents, Total_Kill, Total_Wound))
    pcGTD = princomp(formula=~ ., data = countryWiseDataPCA, cor=TRUE)
    pcGTD
    countryWiseData$GTI_Score=pcGTD$scores[,1]
    
    pcGTD$loadings
    
    colnames(TerroristGDPDataContinent)
    dim(TerroristGDPDataContinent)
    countryWiseData=countryWiseData[with(countryWiseData, order(-GTI_Score)), ]
    head(countryWiseData)
    
    TerroristGDPDataContinent
    
    unique(unlist(TerroristGDPDataContinent$Assassination))
    unique(unlist(TerroristGDPDataContinent$attacktype3_txt))
    
    
    TerroristGDPDataContinent$Armed_Assault=ifelse(is.na( TerroristGDPDataContinent$attacktype1_txt), 0,  ifelse(TerroristGDPDataContinent$attacktype1_txt=="Armed Assault", 1, 0))
    TerroristGDPDataContinent$Facility_Infrastructure_Attack=ifelse(is.na(TerroristGDPDataContinent$attacktype1_txt), 0,  ifelse(TerroristGDPDataContinent$attacktype1_txt=="Facility/Infrastructure Attack", 1, 0))
    TerroristGDPDataContinent$Bombing_Explosion=ifelse(is.na( TerroristGDPDataContinent$attacktype1_txt), 0,  ifelse(TerroristGDPDataContinent$attacktype1_txt=="Bombing/Explosion", 1, 0))
    TerroristGDPDataContinent$Hostage_Taking_Kidnapping=ifelse(is.na( TerroristGDPDataContinent$attacktype1_txt), 0,  ifelse(TerroristGDPDataContinent$attacktype1_txt=="Hostage Taking (Kidnapping)", 1, 0))
    TerroristGDPDataContinent$Assassination=ifelse(is.na( TerroristGDPDataContinent$attacktype1_txt), 0,  ifelse(TerroristGDPDataContinent$attacktype1_txt=="Assassination", 1, 0))
    TerroristGDPDataContinent$Hijacking=ifelse(is.na( TerroristGDPDataContinent$attacktype1_txt), 0,  ifelse(TerroristGDPDataContinent$attacktype1_txt=="Hijacking", 1, 0))
    TerroristGDPDataContinent$Hostage_Taking_Barricade_Incident=ifelse(is.na( TerroristGDPDataContinent$attacktype1_txt), 0,  ifelse(TerroristGDPDataContinent$attacktype1_txt=="Hostage Taking (Barricade Incident)", 1, 0))
    TerroristGDPDataContinent$Unknown=ifelse(is.na( TerroristGDPDataContinent$attacktype1_txt), 0,  ifelse(TerroristGDPDataContinent$attacktype1_txt=="Unknown", 1, 0))
    TerroristGDPDataContinent$Unarmed_Assault=ifelse(is.na( TerroristGDPDataContinent$attacktype1_txt), 0,  ifelse(TerroristGDPDataContinent$attacktype1_txt=="Unarmed Assault", 1, 0))
    
    TerroristGDPDataContinent$Armed_Assault=ifelse(is.na( TerroristGDPDataContinent$attacktype2_txt), TerroristGDPDataContinent$Armed_Assault,  ifelse(TerroristGDPDataContinent$attacktype2_txt=="Armed Assault", 1, TerroristGDPDataContinent$Armed_Assault))
    TerroristGDPDataContinent$Facility_Infrastructure_Attack=ifelse(is.na(TerroristGDPDataContinent$attacktype2_txt), TerroristGDPDataContinent$Facility_Infrastructure_Attack,  ifelse(TerroristGDPDataContinent$attacktype2_txt=="Facility/Infrastructure Attack", 1, TerroristGDPDataContinent$Facility_Infrastructure_Attack))
    TerroristGDPDataContinent$Bombing_Explosion=ifelse(is.na( TerroristGDPDataContinent$attacktype2_txt), TerroristGDPDataContinent$Bombing_Explosion,  ifelse(TerroristGDPDataContinent$attacktype2_txt=="Bombing/Explosion", 1, TerroristGDPDataContinent$Bombing_Explosion))
    TerroristGDPDataContinent$Hostage_Taking_Kidnapping=ifelse(is.na( TerroristGDPDataContinent$attacktype2_txt), TerroristGDPDataContinent$Hostage_Taking_Kidnapping,  ifelse(TerroristGDPDataContinent$attacktype2_txt=="Hostage Taking (Kidnapping)", 1, TerroristGDPDataContinent$Hostage_Taking_Kidnapping))
    TerroristGDPDataContinent$Assassination=ifelse(is.na( TerroristGDPDataContinent$attacktype2_txt), TerroristGDPDataContinent$Assassination,  ifelse(TerroristGDPDataContinent$attacktype2_txt=="Assassination", 1, TerroristGDPDataContinent$Assassination))
    TerroristGDPDataContinent$Hijacking=ifelse(is.na( TerroristGDPDataContinent$attacktype2_txt), TerroristGDPDataContinent$Hijacking,  ifelse(TerroristGDPDataContinent$attacktype2_txt=="Hijacking", 1, TerroristGDPDataContinent$Hijacking))
    TerroristGDPDataContinent$Hostage_Taking_Barricade_Incident=ifelse(is.na( TerroristGDPDataContinent$attacktype2_txt), TerroristGDPDataContinent$Hostage_Taking_Barricade_Incident,  ifelse(TerroristGDPDataContinent$attacktype2_txt=="Hostage Taking (Barricade Incident)", 1, TerroristGDPDataContinent$Hostage_Taking_Barricade_Incident))
    TerroristGDPDataContinent$Unknown=ifelse(is.na( TerroristGDPDataContinent$attacktype2_txt), TerroristGDPDataContinent$Unknown,  ifelse(TerroristGDPDataContinent$attacktype2_txt=="Unknown", 1, TerroristGDPDataContinent$Unknown))
    TerroristGDPDataContinent$Unarmed_Assault=ifelse(is.na( TerroristGDPDataContinent$attacktype2_txt), TerroristGDPDataContinent$Unarmed_Assault,  ifelse(TerroristGDPDataContinent$attacktype2_txt=="Unarmed Assault", 1, TerroristGDPDataContinent$Unarmed_Assault))
    
    TerroristGDPDataContinent$Armed_Assault=ifelse(is.na( TerroristGDPDataContinent$attacktype3_txt), TerroristGDPDataContinent$Armed_Assault,  ifelse(TerroristGDPDataContinent$attacktype3_txt=="Armed Assault", 1, TerroristGDPDataContinent$Armed_Assault))
    TerroristGDPDataContinent$Facility_Infrastructure_Attack=ifelse(is.na(TerroristGDPDataContinent$attacktype3_txt), TerroristGDPDataContinent$Facility_Infrastructure_Attack,  ifelse(TerroristGDPDataContinent$attacktype3_txt=="Facility/Infrastructure Attack", 1, TerroristGDPDataContinent$Facility_Infrastructure_Attack))
    TerroristGDPDataContinent$Bombing_Explosion=ifelse(is.na( TerroristGDPDataContinent$attacktype3_txt), TerroristGDPDataContinent$Bombing_Explosion,  ifelse(TerroristGDPDataContinent$attacktype3_txt=="Bombing/Explosion", 1, TerroristGDPDataContinent$Bombing_Explosion))
    TerroristGDPDataContinent$Hostage_Taking_Kidnapping=ifelse(is.na( TerroristGDPDataContinent$attacktype3_txt), TerroristGDPDataContinent$Hostage_Taking_Kidnapping,  ifelse(TerroristGDPDataContinent$attacktype3_txt=="Hostage Taking (Kidnapping)", 1, TerroristGDPDataContinent$Hostage_Taking_Kidnapping))
    TerroristGDPDataContinent$Assassination=ifelse(is.na( TerroristGDPDataContinent$attacktype3_txt), TerroristGDPDataContinent$Assassination,  ifelse(TerroristGDPDataContinent$attacktype3_txt=="Assassination", 1, TerroristGDPDataContinent$Assassination))
    TerroristGDPDataContinent$Hijacking=ifelse(is.na( TerroristGDPDataContinent$attacktype3_txt), TerroristGDPDataContinent$Hijacking,  ifelse(TerroristGDPDataContinent$attacktype3_txt=="Hijacking", 1, TerroristGDPDataContinent$Hijacking))
    TerroristGDPDataContinent$Hostage_Taking_Barricade_Incident=ifelse(is.na( TerroristGDPDataContinent$attacktype3_txt), TerroristGDPDataContinent$Hostage_Taking_Barricade_Incident,  ifelse(TerroristGDPDataContinent$attacktype3_txt=="Hostage Taking (Barricade Incident)", 1, TerroristGDPDataContinent$Hostage_Taking_Barricade_Incident))
    TerroristGDPDataContinent$Unknown=ifelse(is.na( TerroristGDPDataContinent$attacktype3_txt), TerroristGDPDataContinent$Unknown,  ifelse(TerroristGDPDataContinent$attacktype3_txt=="Unknown", 1, TerroristGDPDataContinent$Unknown))
    TerroristGDPDataContinent$Unarmed_Assault=ifelse(is.na( TerroristGDPDataContinent$attacktype3_txt), TerroristGDPDataContinent$Unarmed_Assault,  ifelse(TerroristGDPDataContinent$attacktype3_txt=="Unarmed Assault", 1, TerroristGDPDataContinent$Unarmed_Assault))
    
    TerroristGDPDataContinent$Total_Attacks_In_One=TerroristGDPDataContinent$Armed_Assault+TerroristGDPDataContinent$Facility_Infrastructure_Attack+TerroristGDPDataContinent$Bombing_Explosion+TerroristGDPDataContinent$Hostage_Taking_Kidnapping
    +TerroristGDPDataContinent$Assassination+TerroristGDPDataContinent$Hijacking+TerroristGDPDataContinent$Hostage_Taking_Barricade_Incident
    +TerroristGDPDataContinent$Unknown+TerroristGDPDataContinent$Unarmed_Assault
    
    
    TerroristGDPDataContinent$bitValuesAttackTypes=paste(paste(paste(paste(paste(paste(paste(paste(TerroristGDPDataContinent$Armed_Assault, TerroristGDPDataContinent$Facility_Infrastructure_Attack)
                                                         ,TerroristGDPDataContinent$Bombing_Explosion)
                                                         ,TerroristGDPDataContinent$Hostage_Taking_Kidnapping)
                                                         ,TerroristGDPDataContinent$Assassination)
                                                         ,TerroristGDPDataContinent$Hijacking)
                                                         ,TerroristGDPDataContinent$Hostage_Taking_Barricade_Incident)
                                                         ,TerroristGDPDataContinent$Unknown)
                                                         ,TerroristGDPDataContinent$Unarmed_Assault)
    TerroristGDPDataContinent$bitValuesAttackTypes=stringi::stri_replace_all_fixed(TerroristGDPDataContinent$bitValuesAttackTypes, " ", "")
    TerroristGDPDataContinent$bitValuesAttackTypes=as.factor(TerroristGDPDataContinent$bitValuesAttackTypes)
    attacktype_combination=read.csv(file.choose())
    summary(TerroristGDPDataContinent$bitValuesAttackTypes)
    
    
    combinationAttackWiseSummary = ddply(TerroristGDPDataContinent,.(bitValuesAttackTypes),summarise,Total_Incidents=length(na.omit(eventid)), Total_Kill=sum(na.omit(nkill)), Total_Wound=sum(na.omit(nwound)))
    attacktype_combination = ddply(TerroristGDPDataContinent,.(bitValuesAttackTypes, attacktype1_txt,attacktype2_txt,attacktype3_txt),summarise,Total_Incidents=length(na.omit(eventid)))
    write.csv(attacktype_combination, file.choose())
    colnames(combinationAttackWiseSummary)
    combinationAttackWiseSummaryDetails = merge(combinationAttackWiseSummary, attacktype_combination, by="bitValuesAttackTypes");
    write.csv(combinationAttackWiseSummaryDetails, file.choose())
    subset(head(combinationAttackWiseSummaryDetails), select=c(bitValuesAttackTypes, attacktype1_txt, attacktype2_txt, attacktype3_txt, Total_Incidents))
    
    summary(attacktype_combination)
    str(attacktype_combination)
    
    
    BinToDec <- function(x) 
      sum(2^(which(rev(unlist(strsplit(as.character(x), "")) == 1))-1))
    
    
    TerroristGDPDataContinent$numberBitValuesAttackTypes=BinToDec(TerroristGDPDataContinent$bitValuesAttackTypes)
    summary(TerroristGDPDataContinent$numberBitValuesAttackTypesTerroristGDPDataContinent$numberBitValuesAttackTypes
    
    
    write.csv(attacktype_combination, file.choose())
    colnames(TerroristGDPDataContinent)
    
    binStr <- "100100000"
    BinToDec(binStr)
    
    
    summary(TerroristGDPDataContinent$Armed_Assault)
    summary(TerroristGDPDataContinent$Facility_Infrastructure_Attack)
    summary(TerroristGDPDataContinent$Bombing_Explosion)
    summary(TerroristGDPDataContinent$Hostage_Taking_Kidnapping)
    summary(TerroristGDPDataContinent$Assassination)
    summary(TerroristGDPDataContinent$Hijacking)
    summary(TerroristGDPDataContinent$Hostage_Taking_Barricade_Incident)
    summary(TerroristGDPDataContinent$Unknown)
    summary(TerroristGDPDataContinent$Unarmed_Assault)
    summary(TerroristGDPDataContinent$Total_Attacks_In_One)
    
    
    
    allThreeAttacks=subset(TerroristGDPDataContinent, Total_Attacks_In_One==2, select=c(eventid,attacktype1_txt,attacktype2_txt,attacktype3_txt,Armed_Assault, Facility_Infrastructure_Attack, Bombing_Explosion, Hostage_Taking_Kidnapping, Assassination, Hijacking, Hostage_Taking_Barricade_Incident, Unknown, Unarmed_Assault, nkill))
    head(allThreeAttacks)
    max(na.omit(allThreeAttacks$nkill))
    allThreeAttacks=allThreeAttacks[with(allThreeAttacks, order(-nkill)), ]
    TerroristGDPDataContinentNKILLWise=TerroristGDPDataContinent[with(TerroristGDPDataContinent, order(-nkill)), ]
    subset(allThreeAttacks, nkill == 310, select = c(eventid, nkill, attacktype1_txt,attacktype2_txt,attacktype3_txt))
    # Property Damage Analysis
    countryWithPropertyDamage=subset(TerroristGDPDataContinent, propvalue>0)
    dim(countryWithPropertyDamage)
    head(subset(countryWithPropertyDamage, select=c(country_txt, propvalue)))
    summaryCountryWithPropertyDamage=ddply(countryWithPropertyDamage,.(country_txt),summarise,Total_PropValue=sum(na.omit(propvalue)))
    unique(unlist(summaryCountryWithPropertyDamage$country_txt))
    
    continentsMissing=subset(TerroristGDPDataContinent, is.na(Continent))
    TerroristGDPDataContinent$Continent1=ifelse(TerroristGDPDataContinent$country_txt=="East Germany (GDR)", "EU", TerroristGDPDataContinent$Continent)
    TerroristGDPDataContinent$Name = ifelse(TerroristGDPDataContinent$country_txt=="East Germany (GDR)", "Europe", TerroristGDPDataContinent$Name)
    
    unique(unlist(TerroristGDPDataContinent$Continent))
    TerroristGDPDataContinent_ALL_NAs=subset(TerroristGDPDataContinent, is.na(TerroristGDPDataContinent$Continent))
    unique(unlist(TerroristGDPDataContinent_ALL_NAs$country_txt))
    TerroristGDPDataContinent_ALL_NAs$Continent1 = ifelse(TerroristGDPDataContinent_ALL_NAs$country_txt=="East Germany (GDR)", "EU", TerroristGDPDataContinent_ALL_NAs$Continent)
    unique(unlist(TerroristGDPDataContinent$Continent1))
    


        
    TerroristGDPDataContinent_ALL_NAs_ONLY_EVENTID=subset(TerroristGDPDataContinent_ALL_NAs, select=c(eventid, Continent1))
    head(TerroristGDPDataContinent_ALL_NAs_ONLY_EVENTID)
    
    
    
    merge(TerroristGDPDataContinent, TerroristGDPDataContinent_ALL_NAs_ONLY_EVENTID, by="eventid", all.x=TRUE)
          
          unique(unlist(TerroristGDPDataContinent$Continent))
          
          TerroristGDPDataContinent$
          TerroristGDPDataContinent$Continent1 = ifelse(TerroristGDPDataContinent$country_txt=="East Germany (GDR)",
                                                         "EU", 
                                                        ifelse(is.na(TerroristGDPDataContinent$Continent1), 
                                                                " ", 
                                                               TerroristGDPDataContinent$Continent1))
          
          drops=c("Name", "Continent")
          TerroristGDPDataContinent=TerroristGDPDataContinent[, !(names(TerroristGDPDataContinent) %in%  drops)]
          dim(TerroristGDPDataContinent)
          colnames(TerroristGDPDataContinent)
          TerroristGDPDataContinent=read.csv(file.choose())
          country_continent_mapping = read.csv(file.choose())
          colnames(country_continent_mapping)=c("country_txt" , "Continent_Code" ,   "Continent_Name", "country")
          colnames(country_continent_mapping)
          TerroristGDPDataContinent=merge(TerroristGDPDataContinent, country_continent_mapping, by="country", all.x=TRUE)
          write.csv(TerroristGDPDataContinent, file.choose())
          colnames(TerroristGDPDataContinent)
          
          allNAConti = subset(TerroristGDPDataContinent, is.na(TerroristGDPDataContinent$Continent))
          dim(allNAConti)
          unique(unlist(paste(paste(TerroristGDPDataContinent$country,"~"), TerroristGDPDataContinent$country_txt))
          unique(unlist(TerroristGDPDataContinent$Continent_Code))
          
          subset(TerroristGDPDataContinent, is.na(TerroristGDPDataContinent$Content_Name)==TRUE, select = c(country_txt, region_txt, Content_Name))
          subset(allThreeAttacks, nkill == 310, select = c(eventid, nkill, attacktype1_txt,attacktype2_txt,attacktype3_txt))
          pmatch(c("med", "mod"), c("mean", "median", "mode"))
          subset(TerroristGDPDataContinent, country==217, select = c(country_txt, region_txt, Continent_Name, Continent_Code))
          
          subset(country_continent_mapping, country_txt=="Grenada");
          library(stringi)
          library(stringr)
          str(country_continent_mapping)
          ??stringdist_inner_join
          
          write.csv(TerroristGDPDataContinent, file.choose());
          country_continent_mapping

          
