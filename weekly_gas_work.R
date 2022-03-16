#make sure the working directory is set here
setwd(dirname(rstudioapi::getSourceEditorContext()$path))


library(tidyverse)
library(lemon)
library(openxlsx)
library(lubridate)
library(scales)
library(viridis)
library(ggpubr)
library(ggthemes)
library(janitor)
library(readxl)

library(grid)
library(gridExtra)
library(timeDate)



## @knitr gas_prelims

library(maps)
library(mapdata)
library(ggthemes)

kent_download_html<-function(downloads_data){
  #testing
  #url_sent<-kent_url(test_type$product_id,test_type$types_id,test_type$freq_id,test_type$year_id,test_type$format_name)
  #downloads_data<-kent_filter[3,]
  #get the url
  url_sent<-downloads_data$url
  
  temp <- tempfile()
  download.file(url_sent,temp,mode="wb")
  #read the excel
  tables <- readHTMLTable(temp,stringsasFactors=FALSE)
  
  tables <- getNodeSet(htmlParse(temp), "//table")
  gas_data <- readHTMLTable(tables[[1]],
                            #header = c("peak","trough","contraction",
                            #           "expansion","trough2trough","peak2peak"),
                            #colClasses = c("character","character","character",
                            #               "character","character","character"),
                            trim = TRUE, stringsAsFactors = FALSE
  )
  #get the id values from the downloads matrix
  read_year<-as.character(downloads_data$year_name)
  read_fuel<-as.character(downloads_data$product_name)
  read_data<-as.character(downloads_data$types_name)
  read_freq<-as.character(downloads_data$freq_name)
  gas_data<-gas_data[-1,]
  #remove duplicated dates which seem to happen with non-leap-years
  gas_data<-gas_data[,!duplicated(as.character(gas_data[1,]))]
  #add year to dates
  gas_data[1,]<-paste(read_year,gas_data[1,],sep="/")
  #set header to city
  gas_data[1,1]<-"City"
  
  #use first row as headers
  names(gas_data)<-gas_data[1,]
  #remove row used as names
  gas_data<-gas_data[-1,]
  
  #fix City Names
  gas_data$City[grep("TROIS RIV",gas_data$City)]<-"TROIS RIVIERES"
  gas_data$City[grep("MONTR",gas_data$City)]<-"MONTREAL"
  gas_data$City<-gsub("GASPÃ???","GASPE",gas_data$City)
  gas_data$City<-gsub("QUÃ???BEC","QUEBEC",gas_data$City)
  gas_data$City<-gsub("QuÃ©bec","QUEBEC",gas_data$City)
  gas_data$City[grep("WINNIPEG",gas_data$City)]<-"WINNIPEG"
  gas_data$City[grep("TORONTO",gas_data$City)]<-"TORONTO"
  #make dataframe
  gas_data$fuel<-read_fuel
  gas_data$data_field<-read_data
  gas_data$year<-read_year
  gas_data$freq<-read_freq
  gas_data<-gas_data %>% melt(id=c("City","fuel","year","freq","data_field"),value.name = "value",variable.name = "Date")
  #gas_data$Date_store<-gas_data$Date
  #gas_data$Date<-ymd(as.character(gas_data$Date))
  gas_data$Date<-as.Date(as.character(gas_data$Date), format = "%Y/%m/%d")
  gas_data$value<-as.numeric(gas_data$value)
  gas_data
}



kent_download_excel<-function(downloads_data){
  #testing
  #downloads_data<-downloads[1,]
  #get the url
  url_sent<-downloads_data$url
  #check the file type
  temp <- tempfile()
  download.file(url_sent,temp,mode="wb")
  #read the excel
  gas_data <- read_excel(temp,col_names = FALSE)
  #get the id values from the downloads matrix
  read_year<-as.character(downloads_data$year_name)
  read_fuel<-as.character(downloads_data$product_name)
  read_data<-as.character(downloads_data$types_name)
  read_freq<-as.character(downloads_data$freq_name)
  if(as.numeric(read_year)>2016){
    #remove first two rows with data tags
    gas_data<-gas_data[-c(1,2),]
    #get rid of the last two rows
    gas_data<-gas_data[-c(NROW(gas_data),NROW(gas_data)-1),]
  }
  if(as.numeric(read_year)<=2016)
    gas_data<-gas_data[,-grep("Average",gas_data[1,])]
  #change dates to have year on them
  gas_data[1,]<-paste(read_year,gas_data[1,],sep="/")
  
  #remove duplicated dates which seem to happen with non-leap-years
  gas_data<-gas_data[,!duplicated(as.character(gas_data[1,]))]
  #set header to city
  gas_data[1,1]<-"City"
  #use first row as headers
  names(gas_data)<-gas_data[1,]
  #remove row used as names
  gas_data<-gas_data[-1,]
  
  #fix cities
  gas_data$City[grep("TORONTO",gas_data$City)]<-"TORONTO"
  
  gas_data$fuel<-read_fuel
  gas_data$data_field<-read_data
  gas_data$year<-read_year
  gas_data$freq<-read_freq
  gas_data<-gas_data %>% melt(id=c("City","fuel","year","freq","data_field"),value.name = "value",variable.name = "Date")
  gas_data$Date<-ymd(as.character(gas_data$Date))
  #gas_data$Date_store<-gas_data$Date
  gas_data$value<-as.numeric(gas_data$value)
  gas_data
}




kent_download<-function(downloads_data){
  if(downloads_data$format_name=="Excel")
    new_data<-kent_download_excel(downloads_data)
  if(downloads_data$format_name=="HTML")
    new_data<-kent_download_html(downloads_data)
  new_data$City<-gsub("ST JOHNS","ST. JOHN'S",new_data$City)
  new_data$City<-str_to_title(new_data$City)
  
  new_data
}






#kent_data<-types


#kent_data<-get_kent_urls()
#save(kent_data,file="kent_data.R")

load(file="kent_data.RData")
load(file="gas_cities.R")


products_of_interest<-c("regular gasoline","diesel")
#get me all the archived files
downloads<-filter(kent_data,freq_name=="Weekly",format_name=="Excel",product_name %in% products_of_interest)


#downloads$xlsx<-grepl(".xlsx",downloads$url)

#download_url<-paste("https://charting.kentgroupltd.com/Charting/DownloadExcel?file=",kent_url,sep="")
#downloads_data<-downloads[4,]

weekly_gas_data<-bind_rows(kent_download(downloads[1,]),kent_download(downloads[2,]),kent_download(downloads[3,]))%>%
  na.omit()

weekly_diesel_data<-bind_rows(kent_download(downloads[16,]),kent_download(downloads[17,]),kent_download(downloads[18,]))%>%
  na.omit()

gasoline_daily<-kent_data %>% filter(product_name=="regular gasoline",freq_name=="Daily",format_name=="Excel",types_name != "Wholesale by Marketer") %>%
  filter(year_name %in% c("2018","2019"))

daily_gasoline<-kent_download(gasoline_daily[1,])%>%  na.omit()
for(index in seq(2,NROW(gasoline_daily))){
  print(paste("working on row",index))
  new_data<-kent_download(gasoline_daily[index,])%>% na.omit()
  daily_gasoline<-rbind(daily_gasoline,new_data)
}
daily_gasoline$data_field<-as.factor(daily_gasoline$data_field)
daily_gasoline$City<-gsub("\\*","",daily_gasoline$City)

diesel_daily<-kent_data %>% filter(product_name=="diesel",freq_name=="Daily",format_name=="Excel",types_name != "Wholesale by Marketer") %>%
  filter(year_name %in% c("2018","2019"))

daily_diesel<-kent_download(diesel_daily[1,])%>%  na.omit()
for(index in seq(2,NROW(diesel_daily))){
  print(paste("working on row",index))
  new_data<-kent_download(diesel_daily[index,])%>% na.omit()
  daily_diesel<-rbind(daily_diesel,new_data)
}
daily_diesel$data_field<-as.factor(daily_diesel$data_field)
daily_diesel$City<-gsub("\\*","",daily_diesel$City)




## @knitr gas_graphs

gasoline<-ggplot(weekly_gas_data)+
  geom_line(data=filter(weekly_gas_data,City!="Edmonton"),aes(Date,value,group=City),color="grey80")+
  geom_line(data=filter(weekly_gas_data,City=="Edmonton"),aes(Date,value,group=City,color=City),size=2)+
  geom_line(data=filter(weekly_gas_data,City=="Calgary"),aes(Date,value,group=City,color=City),size=2)+
  geom_line(data=filter(weekly_gas_data,City=="Toronto"),aes(Date,value,group=City,color=City),size=2)+
  scale_color_manual("",values=colors_ua10()[-3])+
  scale_y_continuous(limits = c(80,175))+
  scale_x_date(date_breaks = "3 months",date_labels = "%b %d\n%Y")+
  labs(y="Retail Price, cents per liter",x="Date",
       title="Retail Gasoline (top) and Diesel (bottom) Prices",
       subtitle="All other cities tracked by Kent Group in grey",
       caption="Data via Kent Group and NRCan")+
  weekly_graphs()


diesel<-ggplot(weekly_diesel_data)+
  geom_line(data=filter(weekly_diesel_data,City!="EDMONTON"),aes(Date,value,group=City),color="grey80")+
  geom_line(data=filter(weekly_diesel_data,City=="Edmonton"),aes(Date,value,group=City,color=City),size=2)+
  geom_line(data=filter(weekly_diesel_data,City=="Calgary"),aes(Date,value,group=City,color=City),size=2)+
  geom_line(data=filter(weekly_diesel_data,City=="Toronto"),aes(Date,value,group=City,color=City),size=2)+
  scale_color_manual("",values=colors_ua10()[-3])+
  scale_y_continuous(limits = c(80,160))+
  scale_x_date(date_breaks = "3 months",date_labels = "%b %d\n%Y")+
  labs(y="Retail Price, cents per liter",x="",
       #title="Retail Diesel Prices",
       #subtitle="All other cities tracked by Kent Group in grey",
       caption="Data via Kent Group and NRCan")+
  weekly_graphs()


#set_png("all_prices.png")
weekly_prices<-arrangeGrob(gasoline+theme(legend.position="none",
                                        legend.margin=margin(c(0,0,0,0),unit="cm"),
                                        legend.text = element_text(colour="black", size = 12, face = "bold"),
                                        plot.caption = element_blank(),
                               axis.text = element_text(colour="black", size = 10, face = "bold"),
                               axis.title = element_text(colour="black", size = 10, face = "bold"),
                               axis.text.x = element_blank(),
                               axis.title.x = element_blank()),
                diesel+theme(legend.position="bottom",
                             legend.margin=margin(c(0,0,0,0),unit="cm"),
                             legend.text = element_text(colour="black", size = 12, face = "bold"),
                             axis.text = element_text(colour="black", size = 10, face = "bold"),
                             axis.title = element_text(colour="black", size = 10, face = "bold"),
                             plot.subtitle = element_blank()),ncol=1,heights=c(3,3.5))



#grid.arrange(ap)


#https://charting.kentgroupltd.com/filemanager.ashx/ListFolder
#https://charting.kentgroupltd.com/filemanager.ashx/Download/WPPS_Report2%20June28.htm?stateId=6d85a9a57b1c8d70ad9c&path=%5BWPPS+Archive%5D%3A%5CWPPS+2011&openInBrowser=false&fileName=WPPS_Report2+June28.htm
#dev.off()


gas_plot<-ggplot(daily_gasoline)+
  geom_line(data=filter(daily_gasoline,City!="Edmonton",data_field=="Retail"),aes(Date,value,group=City),color="grey80")+
  geom_line(data=filter(daily_gasoline,City=="Edmonton",data_field=="Retail"),aes(Date,value,group=City,color=City),size=2)+
  geom_line(data=filter(daily_gasoline,City=="Calgary",data_field=="Retail"),aes(Date,value,group=City,color=City),size=2)+
  geom_line(data=filter(daily_gasoline,City=="Toronto",data_field=="Retail"),aes(Date,value,group=City,color=City),size=2)+
  geom_line(data=filter(daily_gasoline,City=="Vancouver",data_field=="Retail"),aes(Date,value,group=City,color=City),size=2)+
  scale_color_manual("",values=colors_ua10()[-3])+
  scale_x_date(limits=c(min(daily_gasoline$Date),max(daily_gasoline$Date)), date_breaks = "1 months",date_labels = "%b %d\n%Y",expand=c(0,0))+
  scale_y_continuous(limits = c(75,175))+
  labs(y="Retail Price, cents per liter",x="",
       title="Retail Gasoline (top) and Diesel (bottom) Prices",
       subtitle="All other cities tracked by Kent Group in grey",
       caption="Data via Kent Group and NRCan")+
  weekly_small()
#gas_plot


diesel_plot<-ggplot(daily_diesel)+
  geom_line(data=filter(daily_diesel,City!="Edmonton",data_field=="Retail"),aes(Date,value,group=City),color="grey80")+
  geom_line(data=filter(daily_diesel,City=="Edmonton",data_field=="Retail"),aes(Date,value,group=City,color=City),size=2)+
  geom_line(data=filter(daily_diesel,City=="Calgary",data_field=="Retail"),aes(Date,value,group=City,color=City),size=2)+
  geom_line(data=filter(daily_diesel,City=="Toronto",data_field=="Retail"),aes(Date,value,group=City,color=City),size=2)+
  geom_line(data=filter(daily_diesel,City=="Vancouver",data_field=="Retail"),aes(Date,value,group=City,color=City),size=2)+
  scale_y_continuous(limits = c(75,175))+
  scale_color_manual("",values=colors_ua10()[-3])+
  scale_x_date(limits=c(min(daily_diesel$Date),max(daily_diesel$Date)), date_breaks = "1 months",date_labels = "%b %d\n%Y",expand=c(0,0))+
  labs(y="Retail Price, cents per liter",x="",
       title="Retail Diesel Prices",
       subtitle="All other cities tracked by Kent Group in grey",
       caption="Data via Kent Group and NRCan")+
  weekly_small()
#diesel_plot

daily_prices<-arrangeGrob(gas_plot+theme(legend.position="none",
                                                      legend.margin=margin(c(0,0,0,0),unit="cm"),
                                                      legend.text = element_text(colour="black", size = 12, face = "bold"),
                                                      plot.caption = element_blank(),
                                                      axis.text = element_text(colour="black", size = 10, face = "bold"),
                                                      axis.title = element_text(colour="black", size = 10, face = "bold"),
                                                      axis.text.x = element_blank(),
                                                      axis.title.x = element_blank()),
                                       diesel_plot+theme(legend.position="bottom",
                                                         legend.margin=margin(c(0,0,0,0),unit="cm"),
                                                         legend.text = element_text(colour="black", size = 12, face = "bold"),
                                                         axis.text = element_text(colour="black", size = 10, face = "bold"),
                                                         axis.title = element_text(colour="black", size = 10, face = "bold"),
                                                         plot.subtitle = element_blank(),
                                                         plot.title = element_blank()),
                                       ncol=1,heights=c(3.5,3.5))
#https://charting.kentgroupltd.com/filemanager.ashx/ListFolder
#https://charting.kentgroupltd.com/filemanager.ashx/Download/WPPS_Report2%20June28.htm?stateId=6d85a9a57b1c8d70ad9c&path=%5BWPPS+Archive%5D%3A%5CWPPS+2011&openInBrowser=false&fileName=WPPS_Report2+June28.htm
#set_png("all_prices_daily.png")
#grid.arrange(daily_prices)
#dev.off()

## @knitr gas_map

#grid.arrange(daily_prices)

# carbon_tax_impact<-daily_gasoline%>% filter(City %in% c("Calgary","Vancouver","Edmonton"),data_field=="Retail")
# 
# 
# carbon_tax_impact <-daily_gasoline%>% 
#   filter(City %in% c("Calgary","Vancouver","Edmonton"),data_field=="Retail") %>% 
#   arrange(Date) %>% group_by(City) %>% 
#   mutate(mean_30=rollapply(value,30,mean,align='right',fill=NA),
#          change_30=value-mean_30,
#          may_29_value=max(value*(Date==ymd("2019-05-29"))),
#          change_may_29=value-may_29_value)
#set_png("carbon_tax_cut.png")
# ggplot(filter(carbon_tax_impact,Date>=ymd("2019-05-29")))+
#   #geom_line(aes(Date,change_30,group=City,color=City))+
#   geom_line(aes(Date,change_may_29,group=City,color=City),size=2)+
#   #geom_vline(xintercept = ymd("2019-06-04"))+
# scale_color_manual("",values=colors_ua10()[-3])+
#   scale_x_date(date_breaks = "2 day",date_labels = "%b\n%d",expand=c(0,0))+
#   #scale_y_continuous(limits = c(75,175))+
#   labs(y="Retail Price Change, cents per liter",x="",
#        title="Retail Gasoline Prices Relative to Price on May 29, 2019",
#        #subtitle="All other cities tracked by Kent Group in grey",
#        caption="Data via Kent Group and NRCan")+
#   ajl_line()
#dev.off()
#mapping
map_prices <- daily_gasoline %>% left_join(cdn_cities,by=c("City"="name")) %>% arrange(Date) %>%
  group_by(City,data_field,fuel,lat,long) %>%
  summarise(last=last(value),date=last(Date)) %>% na.omit() %>% filter(data_field=="Retail")%>%
  mutate(label=paste(City,"\n",round(last,1),sep=""))

usa <- map_data("worldLores","usa")
canada <- map_data("worldLores","canada")
mexico <- map_data("worldLores", "Mexico")

Canada_map<-ggplot() + geom_polygon(data = canada, aes(x=long, y = lat, group = group), 
                                    fill = "white", color="black") + 
  coord_fixed(xlim = c(-140, -50),  ylim = c(40, 70), ratio = 1.2)+
  theme_map()+theme(plot.title = element_text(size = 18,face = "bold"),
                    plot.subtitle = element_text(size = 16, face = "italic"))+
  labs(title=paste("Gasoline Prices in Canadian Cities",sep=""),
       caption="Source: Kent Marketing")

#Canada_map


weekly_map<-Canada_map+geom_point(data= map_prices, aes(x=long, y=lat), color=colors_ua10()[1],size=2)+
  geom_label_repel(data= map_prices,aes(x=long, y=lat,label = label),color=colors_ua10()[1],
                   fontface = 'bold',
                   size = 3,
                   #box.padding = unit(0, "lines"),
                   #point.padding = unit(0, "lines"),
                   label.size=1,
                   segment.color = colors_ua10()[1],
                   segment.size = 1.5,
                   force = 10,
                   min.segment.length = unit(0, 'lines'))+
  labs(subtitle=paste("Prices in Canadian cents per litre from Kent Marketing survey data for ",format(max(map_prices$date),"%b %d, %Y"),sep=""))

#set_png("weekly_map.png")
#print(weekly_map)
#dev.off()

