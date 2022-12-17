## @knitr part0
#make sure the working directory is set here
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
old_wd<-getwd()
library(tidyverse)
library(grid)
library(gridExtra)
library(lemon)
library(openxlsx)
library(lubridate)
library(scales)
library(viridis)
library(ggpubr)
library(ggthemes)
library(janitor)
library(readxl)

## @knitr prelims

bb_file<-"C:/Users/aleach/Google Drive/BB Stuff/Leach_BB_R.xlsx"
nrg_folder<-"C:/Users/aleach/Google Drive/NRGStream"


## Make breaks from a starting date at a given hour, occuring by interval,
## length.out is days


#extract legend
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

load_bb_daily<-function()
{ #load BB data
  daily_data <- read.xlsx(xlsxFile = bb_file, sheet = "DailyData", startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
  headers<-c("Date",daily_data[1,seq(2,ncol(daily_data),by=3)])
  units<-c(NA,unlist(daily_data[3,seq(2,ncol(daily_data),by=3)], use.names=FALSE))
  curr<-c(NA,unlist(daily_data[3,seq(1,ncol(daily_data),by=3)],use.names = FALSE))
  codes<-c(NA,unlist(daily_data[4,seq(1,ncol(daily_data),by=3)],use.names = FALSE))
  #check here if you have #NA requesting codes in first row
  data<-daily_data[-(1:5),c(1,seq(2,ncol(daily_data),by=3))]
  if(any(grep("#N/A Requesting",data[1,])))
    data<-data[-1,] #take off the first row if you didn't let the refresh finish
  data[,1]<-as.Date(data[,1])
  data[1,1]<-data[2,1]-days(1)
  names(data)<-headers
  data <- data.frame(lapply(data, function(x) {
    gsub("#N/A N/A", "", x)
  }))
  data[,-1] = apply(data[,-1], 2, function(x) as.numeric(as.character(x)))
  data[,1]<-as.Date(data[,1])
  
  attributes(data)$curr<-curr
  attributes(data)$units<-units
  attributes(data)$bbcode<-codes
  return(data)
}

forwards_data_pull<-function(comdty){
  #testing
  #comdty<-"WTI"
  forwards_read <- read.xlsx(xlsxFile = bb_file, sheet = comdty, startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
  forwards_data<-forwards_read[-seq(1,4),-1]
  forwards_data<-forwards_data[,c(1,seq(2,ncol(forwards_data),by=2))]
  forwards_data <- data.frame(lapply(forwards_data, function(x) {
    gsub("#N/A N/A", "", x)
  }),stringsAsFactors = F)
  units<-forwards_read[1,1]
  name_list<-c("Date",forwards_read[2,seq(2,ncol(forwards_read),by=2)])
  names(forwards_data)<-name_list
  forwards_data[,-1] = apply(forwards_data[,-1], 2, function(x) as.numeric(as.character(x)))
  forwards_data[,1] = as.character(forwards_data[,1])
  if(any(grepl("#N/A Requesting",forwards_data[1,])))
    forwards_data<-forwards_data[-1,] #take off the first row if you didn't let the refresh finish
  forwards_data[,1]<-as_date(forwards_data[,1])
  #need to return both the data and the units
  return_list<-list(forwards_data,units)
  #forwards_data
  return_list
}


diffs_chart<-function(data_sent=data,names=c("LLS","Maya"),years=10,curr="USD",break_set="12 months")
{
  #send a data set and two variable names
  #build differentials
  #names<-c("LLS","Maya")
  #data_sent<-data
  data_sent<-data_sent%>% filter(Date>=max(Date)-years(years))%>%
    select(Date,USDCAD,all_of(names))%>%mutate(diff=eval(parse(text =names[1]))-eval(parse(text =names[2])))%>%
    pivot_longer(-c(Date,USDCAD,diff),names_to="variable")%>%
    mutate(variable=factor(variable,levels = names))
  if(curr=="USD")
    p <- ggplot(data_sent) +
    geom_ribbon(aes(Date,ymax=diff,ymin=0,fill="Differential"),size=1.25)+
    geom_line(aes(Date,value,group = variable,colour=variable),size=1.25) +
    #geom_area(data=filter(df1,variable=="diff"),aes(Date,value,group = variable,fill=variable))+
    #geom_point(size=1) +
    scale_colour_manual(NULL,values=colors_ua10())+
    scale_fill_manual(NULL,values=colors_ua10(),labels=paste(names[1],"Premium or\nDiscount to",names[2],sep=" "))+
    scale_x_date(name=NULL, date_labels =  "%b\n%Y",expand=c(0,0),breaks=pretty_breaks()) +
    scale_y_continuous(expand = c(0, 0)) +
    expand_limits(y=0)+
    labs(y="Spot Prices and Differentials ($US/bbl)",x="Year",
         title=paste(names[1],"and",names[2],"Prices",sep=" "),
         caption="Data via Bloomberg")+
    weekly_graphs()
  
  if(curr=="CAD")
    p <- ggplot(df1,aes(Date,value*USDCAD,group = variable,colour=variable,fill=variable)) +
    geom_area(data=filter(df1,variable=="diff"),size=1.25)+
    geom_line(data=df1,size=1.25) +
    #geom_point(size=1) +
    scale_colour_brewer(NULL,labels=c(names,paste(names[1],"Premium or\nDiscount to",names[2],sep=" ")),type = "seq", palette = "Paired", direction = -1)+
    scale_fill_brewer(NULL,labels=c(names,paste(names[1],"Premium or\nDiscount to",names[2],sep=" ")),type = "seq", palette = "Paired", direction = -1)+
    scale_x_date(name=NULL,breaks = pretty_breaks(), date_labels =  "%b\n%Y",expand=c(0,0)) +
    scale_y_continuous(expand = c(0, 0),breaks=pretty_breaks()) +
    #geom_hline(yintercept = 0) + 
    labs(y="Spot Prices and Differentials ($CAD/bbl)",x="Year",
         title=paste(names[1],"and",names[2],"Prices",sep=" "),
         caption="Data via Bloomberg")+
    weekly_graphs()
  return(p)
}

#diffs_chart()

ref_margin_charts<-function(data_sent,curr="USD",years=5,break_set="12 months")
{
  #send a data set and two variable names
  #build differentials
  #data_sent<-data
  #years=5
  data_sent<-data_sent %>% filter(Date>=max(Date)-years(years))
  #ggplot(data_sent)+geom_line(aes(Date,`ULSD Edmonton`/100*168/USDCAD))+geom_line(aes(Date,`Gulf ULSD`/100*42),colour="dodgerblue")+geom_line(aes(Date,WTI),colour="red")
  #MAKE THE EDMONTON MARGINS into USD/bbl
  data_sent$edm_yield<-1/data_sent$USDCAD*168/100*.88*(2/3*data_sent$`Edmton 87 Oct Gas Rack Con`+1/3*data_sent$`ULSD Edmonton`)
  #data_sent$edm_yield<-1/data_sent$USDCAD*.88/100*168*(2/3*data_sent$`Mid Grd Gas Edm`+1/3*data_sent$`ULSD Edmonton`)
  #gulf yield in USD/bbl
  data_sent$gulf_yield<-.88*(2/3*data_sent$`Gulf Gasoline`+1/3*data_sent$`Gulf ULSD`)/100*42
  data_sent$nymex_yield<-.88*(2/3*data_sent$`NY RBOB`+1/3*data_sent$`NY Harbor ULSD Prompt`)/100*42
  
  data_sent$bitumen_margin<-data_sent$edm_yield-data_sent$`Implied Bitumen`
  data_sent$bitumen_margin_false<-data_sent$edm_yield-data_sent$Maya+7.50
  
  data_sent$bitumen_gulf_margin<-data_sent$gulf_yield-data_sent$`Implied Bitumen`-7.50
  data_sent$bitumen_nymex_margin<-data_sent$nymex_yield-data_sent$`Implied Bitumen`-5
  p <- ggplot(data_sent) +
    geom_area(aes(Date,bitumen_margin_false,fill="Maya-based margin"),size=1.25)+
    geom_area(aes(Date,bitumen_margin,fill="WCS-based margin"),alpha=0.5)+
    geom_line(aes(Date,`WCS`,color="WCS spot price"),size=1.25)+
    geom_line(aes(Date,`Maya`-7.50,color="Maya net toll"),size=1.25)+
    geom_line(aes(Date,edm_yield-7.50,color="Edmonton refined products\n(coking refinery yield)"),size=1.5)+
    #geom_point(size=1) +
    scale_colour_manual(NULL,values=colors_ua10())+
    scale_fill_manual(NULL,values=colors_ua10()[c(2,3)])+
    scale_x_date(name=NULL,breaks=date_breaks(width = break_set), date_labels ="%b\n%Y",expand=c(0,0)) +
    scale_y_continuous(expand = c(0, 0),breaks = pretty_breaks()) +
    #geom_hline(yintercept = 0) + 
    labs(y="Spot Prices and Margins ($US/bbl)",x="Year",
         title="Refinery Margins Based on WCS and Maya Pricing",
         caption="Data via Bloomberg")+
    weekly_graphs()+guides(colour=guide_legend(nrow=1),fill=guide_legend(nrow=3))
  p
}




cad_net_back_area_chart<-function(data_sent,years=10,break_set="12 months",bw=F)
{
  #send a data set and two variable names
  #for testing
  #years<-1
  #data_sent<-data
  
  #build differentials
  data_sent<-data_sent %>% filter(Date>=max(Date)-years(years))
  top_line<-"Brent"
  #Brent #Maya #Maya net tolls to Alberta / WCS #WCS net toll from site  #WCS net cost of diluent amortized over barrels
  
  
  measures<-c("Brent","Maya","WCS","Edmonton Condensate")
  df1<-reshape2::melt(data_sent,id=c("Date","USDCAD"),measure.vars = measures)
  df1$week<-week(df1$Date)
  df1$year<-year(df1$Date)
  df1<-df1 %>% group_by(year,week,variable) %>% summarize(Date=max(Date),value=mean(value),USDCAD=mean(USDCAD))
  df1<-reshape2::dcast(df1,Date+USDCAD ~ variable)
  df1$time_gap<-difftime(df1$Date,ymd("2019-04-01"),units = "days")/365
  df1$toll<-8.75*1.02^(as.numeric(df1$time_gap))#based on 2019-04-01 IJ tolls to Houston of 8.75
  df1$local_toll<-1.02^(as.numeric(df1$time_gap))#based on 2019-04-01 IJ tolls to Houston of 8.75
  df1$bit_site<-((df1$WCS*df1$USDCAD-1.5*df1$local_toll-.3*(df1$`Edmonton Condensate`*df1$USDCAD+df1$local_toll))/.7)
  lims<-c(max(df1$Date)-years(years),max(df1$Date)+months(1))
  lims_y<-c(-2,max(df1$Brent*df1$USDCAD)+10)
  lims_y<-round(lims_y/5)*5 #round to nearest multiple of 5
  labs_discounts=c("Global light (Brent) vs heavy sour (Maya) crude differential",
                   "Transportation cost, Alberta to U.S. Gulf Coast",
                   "Implied export transportation constraint cost",
                   "Alberta local transportation and diluents cost",
                   "Bitumen value at site") 
  p <- ggplot(df1) +
    #geom_line(data=filter(df1,variable!="diff"),aes(Date,value,group = variable,colour=variable),size=1.7) +
    geom_line(aes(Date,Brent*USDCAD,colour = "Global Light Oil"),size=1.25)+
    geom_area(aes(Date,Brent*USDCAD,fill="A"))+
    geom_area(aes(Date,Maya*USDCAD,fill="B"))+  
    geom_area(aes(Date,(Maya*USDCAD-toll),fill="C"))+  
    geom_area(aes(Date,WCS*USDCAD,fill="D")) +
    geom_area(aes(Date,bit_site,fill="E"))+  
    #guide = guide_legend(reverse = TRUE)+
    #geom_point(size=1) +
    scale_colour_manual(NULL,values=c("black"))+
    #scale_fill_manual(NULL,values=colors_ua10())+
    scale_x_date(name=NULL,breaks = break_set, date_labels =  "%b %d\n%Y",expand=c(0,0)) +
    #scale_y_continuous(expand = c(0, 0),breaks=c(-5,seq(0,lims_y[2],10))) +
    #geom_hline(yintercept = 0) + 
    labs(y="Oil prices ($CAD/bbl)",x="Year",
         title="Global Prices, Alberta Netbacks",
         caption="Data via Bloomberg, graph by Andrew Leach")+
    weekly_graphs()+
    guides(colour=guide_legend(nrow=1),fill=guide_legend(nrow=3))
  if(bw==F)
    p<-p+scale_fill_viridis("",discrete=TRUE,option="D",labels=labs_discounts)
  if(bw==T)
    p<-p+scale_fill_grey("", start = .8, end = 0, na.value = "red",labels=labs_discounts)
  #print(p)  
  p
}


ab_constraint_area_chart<-function(data_sent,years=10,break_set="12 months",bw=F)
{
  #testing
  #years<-15
  #data_sent<-data
  #bw<-T
  data_sent<-filter(data_sent,Date>=max(Date)-years(years))
  measures<-c("Maya","WCS")
  df1<-reshape2::melt(data_sent,id=c("Date","USDCAD"),measure.vars = measures)
  df1$week<-week(df1$Date)
  df1$year<-year(df1$Date)
  df1<-df1 %>% group_by(year,week,variable) %>% summarize(Date=max(Date),value=mean(value),USDCAD=mean(USDCAD))
  df1<-reshape2::dcast(df1,Date+USDCAD ~ variable)
  df1$time_gap<-difftime(df1$Date,ymd("2019-04-01"),units = "days")/365
  df1$toll<-8.75*1.02^(as.numeric(df1$time_gap))#based on 2019-04-01 IJ tolls to Houston of 8.75
  df1$AB_discount<-(df1$Maya*df1$USDCAD-df1$toll-df1$WCS*df1$USDCAD) 
  p <- ggplot(df1) +
    geom_area(aes(Date,AB_discount,fill="WCS discount to Maya, net approximate tolls"))+  
    scale_x_date(name=NULL,date_breaks =break_set, date_labels =  "%b %d\n%Y",expand=c(0,0)) +
    labs(y="WCS Discount to Maya ($CAD/bbl)",x="Year",
         caption="Data via Bloomberg, graph by Andrew Leach")+
    weekly_graphs()+
    guides(colour=guide_legend(nrow=1),fill=guide_legend(nrow=3))
  if(bw==F)
    p<-p+scale_fill_viridis("",discrete=TRUE,option="D")
  if(bw==T)
    p<-p+scale_fill_grey("", start = 0.6, end = 0.6, na.value = "red")
  #print(p)  
  p
}

diffs_area_chart<-function(data,names,years,curr="USD",break_set="12 months")
{
  #send a data set and two variable names
  #build differentials
  data_sent<-data %>% select(Date,USDCAD,names)
  data_sent<-filter(data_sent,Date>=max(Date)-years(years))
  diff_name<-paste(names[1],"_",names[2],"_diff",sep="")
  data_sent[,"diff"]<-data_sent[,names[1]]-data_sent[,names[2]]
  measures<-c("diff")
  
  df1<-reshape2::melt(data_sent,id=c("Date","USDCAD"),measure.vars = measures)
  df1<-na.omit(df1)
  df1<-reshape2::dcast(df1,Date+USDCAD ~ variable)
  df1<-df1 %>% dplyr::mutate(polarity = ifelse(diff < 0, "negative", "positive"))
  df1$polarity<-as.factor(df1$polarity)
  df1<-reshape2::melt(df1,id=c("Date","USDCAD","polarity"))
  df1<-na.omit(df1)
  lims<-c(max(df1$Date)-years(years),max(df1$Date)+months(1))
  lims_y<-c(min(0,min(df1$value)),max(df1$value)+10)
  lims_y<-round(lims_y/10)*10 #round to nearest multiple of 10
  levels(df1$variable)<-c("diff")
  if(curr=="USD")
    p <- ggplot(df1) +
    #geom_line(data=filter(df1,variable!="diff"),aes(Date,value,group = variable,colour=variable),size=1.7) +
    geom_area(data=filter(df1,variable=="diff"),aes(Date,value,group = variable,fill=variable))+
    #geom_point(size=1) +
    #scale_colour_manual(NULL,values=colors_ua10(), labels=names)+
    scale_fill_manual(NULL,values=colors_ua10(),labels=paste(names[1],"Premium or\nDiscount to",names[2],sep=" "))+
    scale_x_date(name=NULL,date_breaks = break_set, date_labels =  "%b\n%Y",limits=lims,expand=c(0,0)) +
    scale_y_continuous(expand = c(0, 0),limits=lims_y,breaks=c(-5,seq(0,lims_y[2],10))) +
    #geom_hline(yintercept = 0) + 
    labs(y="Differentials ($US/bbl)",x="Year",
         #title=paste(names[1],"and",names[2],"Prices",sep=" "),
         caption="Data via Bloomberg")+
    weekly_graphs()
  
  if(curr=="CAD")
    p <- ggplot(df1,aes(Date,value*USDCAD,group = variable,fill=variable)) +
    #geom_line(data=df1,size=1.7) +
    geom_area(data=filter(df1,variable=="diff"),aes(Date,value,group = variable,fill=variable))+
    #geom_point(size=1) +
    scale_fill_manual(NULL,values=colors_ua10(),labels=paste(names[1],"Premium or Discount to",names[2],sep=" "))+
    scale_x_date(name=NULL,date_breaks = break_set, date_labels =  "%b\n%Y",limits=lims,expand=c(0,0)) +
    scale_y_continuous(expand = c(0, 0),limits=lims_y,breaks=c(-5,seq(0,lims_y[2],10))) +
    #geom_hline(yintercept = 0) + 
    labs(y="Differentials ($CAD/bbl)",x="Year",
         #title=paste(names[1],"and",names[2],"Prices",sep=" "),
         caption="Data via Bloomberg")+
    weekly_graphs()
  return(p)
}



getSeason <- function(DATES) {
  WS <- as.Date("2012-12-15", format = "%Y-%m-%d") # Winter Solstice
  SE <- as.Date("2012-3-15",  format = "%Y-%m-%d") # Spring Equinox
  SS <- as.Date("2012-6-15",  format = "%Y-%m-%d") # Summer Solstice
  FE <- as.Date("2012-9-15",  format = "%Y-%m-%d") # Fall Equinox
  
  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(DATES, format="2012-%m-%d"))
  
  ifelse (d >= WS | d < SE, "Winter",
          ifelse (d >= SE & d < SS, "Spring",
                  ifelse (d >= SS & d < FE, "Summer", "Fall")))
}


levels_chart<-function(data_sent,names,years,curr="USD",title_sent="Benchmark Oil Prices",break_set="12 months")
{
  #send a data set and two variable names
  #build differentials
  #testing
  #data_sent<-data
  #names<-c("Syncrude Sweet Blend","WCS")
  #curr<-"USD"
  #break_set="12 months"
  #title_sent="Benchmark Oil Prices"
  #years<-5
  rows_legend<-max(1,round(sum(nchar(names))/50))
  data_sent<-filter(data_sent,Date>=max(Date)-years(years))
  df1<-reshape2::melt(data_sent,id=c("Date","USDCAD"),measure.vars = names)
  df1<-na.omit(df1)
  lims<-c(max(df1$Date)-years(years),max(df1$Date)+months(1))
  lims_y<-c(min(0,min(df1$value)-5),max(df1$value)+10)
  if(curr=="CAD")
    lims_y<-c(min(0,min(df1$value*df1$USDCAD)-5),max(df1$value*df1$USDCAD)+10)
  lims_y<-round(lims_y/5)*5 #round to nearest multiple of 5
  if(curr=="USD")
    p<-ggplot(df1) +
    geom_line(data=filter(df1,variable!="diff"),aes(Date,value,group = variable,colour=variable),size=1.25) +
    #geom_point(size=1) +
    scale_colour_manual(NULL,values=colors_ua10(), labels=names)+
    scale_x_date(name=NULL,date_breaks = break_set, date_labels =  "%b\n%Y",limits=lims,expand=c(0,0)) +
    scale_y_continuous(expand = c(0, 0),limits=lims_y,breaks=c(-10,seq(0,lims_y[2],10))) +
    guides(colour=guide_legend(nrow=rows_legend))+
    labs(y="Spot Prices ($US/bbl)",x="Year",
         title=title_sent,
         caption="Data via Bloomberg")+
    weekly_graphs()
  if(curr=="CAD")
    p <- ggplot(df1,aes(Date,value*USDCAD,group = variable,colour=variable,fill=variable)) +
    geom_line(data=df1,size=1.25) +
    scale_colour_manual(NULL,values=colors_ua10(), labels=names)+
    scale_x_date(name=NULL,date_breaks = break_set, date_labels =  "%b\n%Y",limits=lims,expand=c(0,0)) +
    scale_y_continuous(expand = c(0, 0),limits=lims_y,breaks=c(-10,seq(0,lims_y[2],10))) +
    guides(colour=guide_legend(nrow=rows_legend))+
    #geom_hline(yintercept = 0) + 
    labs(y="Spot Prices ($CAD/bbl)",x="Year",
         title=title_sent,
         caption="Data via Bloomberg")+
    weekly_graphs()
  p
}

levels_chart_gas<-function(data_sent,names,years,curr="USD",title_sent="Benchmark Natural Gas Prices",break_set="12 months")
{
  #send a data set and two variable names
  #build differentials
  #data_sent<-data
  #curr<-"USD"
  #break_set<-"12 months"
  #title_sent<-"Benchmark Natural Prices"
  #years<-5
  #curr<-"CAD"
  rows_legend<-max(1,round(sum(nchar(names))/50))
  data_sent<-filter(data_sent,Date>=max(Date)-years(years))
  df1<-reshape2::melt(data_sent,id=c("Date","USDCAD"),measure.vars = names)
  df1<-na.omit(df1)
  #trim the data
  df1<-df1 %>% filter(Date>=max(df1$Date)-years(years))
  lims<-c(max(df1$Date)-years(years),max(df1$Date)+months(1))
  lims_y<-c(min(0,min(df1$value)),max(df1$value)*1.1)
  if(curr=="CAD")
    lims_y<-c(min(0,min(df1$value*df1$USDCAD)),max(df1$value*df1$USDCAD)+10)
  #lims_y<-round(lims_y/5)*5 #round to nearest multiple of 5
  if(curr=="USD")
    p<-ggplot(df1) +
    geom_line(data=filter(df1,variable!="diff"),aes(Date,value,group = variable,colour=variable),size=1.25) +
    #geom_point(size=1) +
    scale_colour_manual(NULL,values=colors_ua10(), labels=names)+
    scale_x_date(name=NULL,date_breaks = break_set, date_labels =  "%b\n%Y",limits=lims,expand=c(0,0)) +
    scale_y_continuous(expand = c(0, 0),limits=lims_y) +
    guides(colour=guide_legend(nrow=rows_legend))+
    labs(y="Spot Prices ($US/MMBTU)",x="Year",
         title=title_sent,
         caption="Data via Bloomberg")+
    weekly_graphs()
  if(curr=="CAD")
    p <- ggplot(df1,aes(Date,value*USDCAD,group = variable,colour=variable,fill=variable)) +
    geom_line(data=df1,size=1.7) +
    scale_colour_manual(NULL,values=colors_ua10(), labels=names)+
    scale_x_date(name=NULL,date_breaks = break_set, date_labels =  "%b\n%Y",limits=lims,expand=c(0,0)) +
    scale_y_continuous(expand = c(0, 0),limits=lims_y,breaks=c(-5,seq(0,lims_y[2],10))) +
    guides(colour=guide_legend(nrow=rows_legend))+
    #geom_hline(yintercept = 0) + 
    labs(y="Spot Prices ($CAD/GJ)",x="Year",
         title=title_sent,
         caption="Data via Bloomberg")+
    weekly_graphs()
  return(p)
}


data<-load_bb_daily()
names(data)<-gsub("USD.CAD","USDCAD",names(data))
names(data)<-gsub("\\."," ",names(data))
names(data)<-gsub("  "," ",names(data))
names(data)<-trimws(names(data))
names(data)<-gsub("So Cal Gas Co CityGate","SoCal Citygate",names(data))



#fix Edmonton condensate

data<-data %>% mutate(`Edmonton Condensate`=case_when(is.na(`Edmonton Condensate Old Stream`)==TRUE ~ `Edmonton Condensate`,
                                                      TRUE~`Edmonton Condensate Old Stream`))


#names(data)<-gsub("Implied.Bitumen","Implied Bitumen",names(data))

#build refining margins


#build differentials
data$WTI_Brent_diff<-data$Brent-data$WTI

data$`Implied Bitumen`<-(data$WCS-.3*data$`Edmonton Condensate`)/.7

names(data)[grep("Syncrude Sweet Blend",names(data))]<-"Syncrude Sweet Synthetic"
names(data)[grep("ANS West Coast",names(data))]<-"Alaska North Slope"


#measures<-grep(paste(c("Brent","WTI","WTI_Brent_diff"),collapse="|"), 
#               names(data), value=TRUE)

#data$`NWR_margin`<-data$WCS+.5*data$`ULSD`+.35*data$`Edmonton Condensate`+.02*data$EdmontonButane/100*168+.02*data$`Edmoton Propane`/100*168

## @knitr ref_margins

set_png("ref_margins.png")
ref_margin_charts(data,break_set = "6 months")
dev.off()

set_png("ref_margins_long.png")
ref_margin_charts(data,years = 10, break_set = "12 months")
dev.off()

## @knitr bitumen_netback


top_panel<-cad_net_back_area_chart(data = data,years = 12,bw=F)+theme(legend.text = element_text(colour="black", size = 10, face = "bold"))
bottom_panel<-ab_constraint_area_chart(data = data,years = 12,bw=T)
mylegend<-arrangeGrob(g_legend(top_panel))
set_png("cdn_bitumen_net.png")
grid.arrange(arrangeGrob(top_panel + labs(y="Oil or bitumen prices ($CAD/bbl)",title="Decomposed Brent Crude to Implied Athabasca Bitumen Value Differential") +
                           theme(legend.position="none",
                                 legend.margin=margin(c(0,0,0,0),unit="cm"),
                                 legend.text = element_text(colour="black", size = 14, face = "bold"),
                                 plot.caption = element_blank(),
                                 plot.title = element_text(size = 12,face="plain"),
                                 plot.subtitle = element_text(size = 14, face = "italic"),
                                 panel.grid.minor = element_blank(),
                                 text = element_text(size = 10,face = "bold"),
                                 axis.text = element_text(size = 10,face = "bold", colour="black"),
                                 axis.text.x = element_blank()
                           ),
                         bottom_panel +labs(subtitle="Transportation-cost-adjusted difference between Maya and Western Canada Select prices")+
                           theme(legend.position="none",
                                 plot.title = element_text(size = 14,face = "bold"),
                                 plot.subtitle = element_text(size = 11,face="plain"),
                                 plot.caption = element_text(size = 8, face = "italic"),
                                 panel.grid.minor = element_blank(),
                                 text = element_text(size = 10,face = "bold"),
                                 axis.text = element_text(size = 10,face = "bold", colour="black"),
                                 axis.title.y = element_text(margin = margin(t = 0, r = 8, b = 0, l = 0))),
                         ncol=1,heights=c(3,2.5)),
             mylegend, 
             nrow=2,heights=c(10,1.5)
             #bottom =text_grob(
             #  "Source: Data via Bloomberg, graph by Andrew Leach",
             #  face = "italic", color = "black",size=9,just="center",lineheight = 1
             #),
             #top =text_grob(
             #  "Crude and Implied Bitumen Price Constituents",
             #  face = "bold", color = "black",size=12,just="center",lineheight = 1
             #)
             
)

dev.off()



top_panel<-cad_net_back_area_chart(data = data,years = 1,bw=F,break_set = "2 months")+theme(legend.text = element_text(colour="black", size = 10, face = "bold"))
bottom_panel<-ab_constraint_area_chart(data = data,years = 1,bw=T,break_set = "2 months")
mylegend<-arrangeGrob(g_legend(top_panel))
set_png("cdn_bitumen_net_short.png")
grid.arrange(arrangeGrob(top_panel + labs(y="Oil or bitumen prices ($CAD/bbl)",title="Decomposed Brent Crude to Implied Athabasca Bitumen Value Differential") +
                           theme(legend.position="none",
                                 legend.margin=margin(c(0,0,0,0),unit="cm"),
                                 legend.text = element_text(colour="black", size = 14, face = "bold"),
                                 plot.caption = element_blank(),
                                 plot.title = element_text(size = 12,face="plain"),
                                 plot.subtitle = element_text(size = 14, face = "italic"),
                                 panel.grid.minor = element_blank(),
                                 text = element_text(size = 10,face = "bold"),
                                 axis.text = element_text(size = 10,face = "bold", colour="black"),
                                 axis.text.x = element_blank()
                           ),
                         bottom_panel +labs(subtitle="Transportation-cost-adjusted difference between Maya and Western Canada Select prices")+
                           theme(legend.position="none",
                                 plot.title = element_text(size = 14,face = "bold"),
                                 plot.subtitle = element_text(size = 11,face="plain"),
                                 plot.caption = element_text(size = 8, face = "italic"),
                                 panel.grid.minor = element_blank(),
                                 text = element_text(size = 10,face = "bold"),
                                 axis.text = element_text(size = 10,face = "bold", colour="black"),
                                 axis.title.y = element_text(margin = margin(t = 0, r = 7, b = 0, l = 0))),
                         ncol=1,heights=c(3,2.5)),
             mylegend, 
             nrow=2,heights=c(10,1.5)
             #bottom =text_grob(
             #  "Source: Data via Bloomberg, graph by Andrew Leach",
             #  face = "italic", color = "black",size=9,just="center",lineheight = 1
             #),
             #top =text_grob(
             #  "Crude and Implied Bitumen Price Constituents",
             #  face = "bold", color = "black",size=12,just="center",lineheight = 1
             #)
             
)

dev.off()


## @knitr oil_gas_graphs


names<-c("WTI")
test<-levels_chart(data=data,names,15,"USD")
set_png("wti_long.png")
print(test)
dev.off()


names<-c("Brent","WTI","Maya","WCS")
test<-levels_chart(data=data,names,5,"USD")
set_png("global_crude.png")
print(test)
dev.off()

names<-c("Syncrude Sweet Synthetic","WTI","Edmonton Mixed Sweet","WCS","Implied Bitumen")
test<-levels_chart(data=data,names,5,"USD")
set_png("cdn_crude.png")
print(test)
dev.off()

names<-c("Syncrude Sweet Synthetic","WTI","Edmonton Mixed Sweet","WCS","Implied Bitumen")
test<-levels_chart(data=data,names,5,"CAD")
set_png("cdn_crude_cad.png")
print(test)
dev.off()

names<-c("Syncrude Sweet Synthetic","WTI","LLS","WTI Midland","Bakken Clearbrook","Alaska North Slope")
test<-levels_chart(data=data,names,12,"USD")
set_png("na_light_crude_usd.png")
print(test)
dev.off()


names<-c("Brent","WTI","LLS","Bakken Clearbrook")
test<-levels_chart(data=data,names,10,"USD")
set_png("na_light_crude2_usd.png")
print(test)
dev.off()

data<-data %>% mutate(`NBP UK`=`NBP Gas`*`GBP USD`/10)

names<-c("AECO NIT","Henry Hub","Japan LNG JCC","NBP UK")
test<-levels_chart_gas(data=data,names,15,"USD")
set_png("global_gas.png")
print(test)
dev.off()


names<-c("Henry Hub","Dawn","Station 2")

names<-c("AECO NIT","PGE Citygate","Empress","Station 2","Chicago Citygate","Algonquin Citygate")

set_png("regional_gas.png")
levels_chart_gas(data=data,names,5,"USD",break_set="6 months")
dev.off()

set_png("regional_gas_short.png")
levels_chart_gas(data=data,names,2,"USD",break_set="3 months")
dev.off()



names<-c("Dawn","AECO NIT","Station 2")
test<-levels_chart_gas(data=data,names,5,"USD")
set_png("cdn_gas.png")
print(test)
dev.off()

names<-c("AECO NIT","Henry Hub","Japan LNG JCC","NBP UK")
test<-levels_chart_gas(data=data,names,5,"USD")
set_png("global_gas.png")
print(test)
dev.off()

data<-data %>% mutate(`Edmonton Wholesale Refined Products`=`ULSD Edmonton`/3+2*`Mid Grd Gas Edm`/3,
                      `Edmonton Retail Refined Products ex Tax`=`Diesel Retail Excl Tax Edmton`/3+
                        `Reg Gas Rtl Excl Tax Edmton`*2/3,
                      `Edmonton Retail Refined Products incl Tax`=`Diesel Retail Incl Tax Edmton Abta`/3+
                        `Reg Gas Retail Edmonton Incl Tax`*2/3,
                      `AECO/NIT Natural Gas (BOE)`=`AECO NIT`*5.5513652248856 )

names<-c("Edmonton Wholesale Refined Products","Edmonton Mixed Sweet",
         "Implied Bitumen","AECO/NIT Natural Gas (BOE)") 
test<-levels_chart(data=data,names,10,"CAD",title_sent="Edmonton Oil and Natural Gas Prices")
set_png("oil_vs_gas.png")
print(test)
dev.off()




names<-c("Edmonton Mixed Sweet","Edmonton Wholesale Refined Products",
         "Edmonton Retail Refined Products ex Tax","Edmonton Retail Refined Products incl Tax") 
test<-levels_chart(data=data,names,5,"CAD",title_sent="Edmonton Oil and Refined Product Prices")
set_png("edm_ref.png")
print(test)
dev.off()

png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("maya_wcs.png")
names<-c("Maya","WCS")
test<-diffs_chart(data=data,names,10,"USD")
print(test)
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()

png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("wti_maya.png")
names<-c("WTI","Maya")
test<-diffs_chart(data=data,names,10,"USD")
print(test)
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()


png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("lls_maya.png")
names<-c("LLS","Maya")
test<-diffs_chart(data=data,names,5,"USD")
print(test)
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()


png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("wti_wcs.png")
names<-c("WTI","WCS")
test<-diffs_chart(data=data,names,10,"USD")
print(test)
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()

png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("brent_wcs.png")
names<-c("Brent","WCS")
test<-diffs_chart(data=data,names,12,"USD")
print(test)
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()

png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("brent_maya.png")
names<-c("Brent","Maya")
test<-diffs_chart(data=data,names,10)
print(test)
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()



png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("Brent_WTI.png")
names<-c("Brent","WTI")
test<-diffs_chart(data=data,names,5,"USD")
print(test)
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()


#getting some diff data to work with
diffs_data<-filter(data,Date>=max(Date)-years(20))
diff_name<-paste(names[1],"_",names[2],"_diff",sep="")
diffs_data[,"diff"]<-data[,names[1]]-data[,names[2]]
measures<-c(names,"diff")
df1<-reshape2::melt(diffs_data,id=c("Date","USDCAD"),measure.vars = measures)
df1<-na.omit(df1)
df1<-reshape2::dcast(df1,Date+USDCAD ~ variable)
df1<-df1 %>% mutate(polarity = ifelse(diff < 0, "negative", "positive"))
df1$polarity<-as.factor(df1$polarity)
df1<-reshape2::melt(df1,id=c("Date","USDCAD","polarity"))
df1<-na.omit(df1)

df1<-df1 %>% mutate(year=year(Date),month=month(Date)) %>% group_by(year, month,variable) %>% summarize(value=mean(value))%>%
  ungroup()%>% filter(variable=="diff")




## @knitr ngl_graphs



ngl_levels_chart<-function(data_sent,names,labels_sent,years,curr="USD",title_sent="NGL Prices",break_set="12 months",exempt_set=NULL)
{
  rows_legend<-max(1,round(sum(nchar(labels_sent))/80))
  data_sent<-filter(data_sent,Date>=max(Date)-years(years))
  df1<-reshape2::melt(data_sent,id=c("Date","USDCAD"),measure.vars = names)
  df1<-na.omit(df1)
  #adjust commodities to $/bbl
  df1<-df1 %>% mutate(value=ifelse(variable%in% exempt_set,value,value*42/100))
  lims<-c(max(df1$Date)-years(years),max(df1$Date)+months(1))
  lims_y<-c(min(0,min(df1$value)-5),max(df1$value)+5)
  lims_y<-round(lims_y/5)*5 #round to nearest multiple of 5
  p<-ggplot(df1) +
    geom_line(data=df1,aes(Date,value,group = variable,colour=variable),size=1.25)
  p<-p+  
    #geom_point(size=1) +
    scale_fill_manual(NULL,values=rev(colors_ua10()))+
    scale_colour_manual(NULL,values=colors_ua10(),labels=labels_sent)+
    scale_x_date(name=NULL,date_breaks = break_set, date_labels =  "%b\n%Y",limits=lims,expand=c(0,0)) +
    scale_y_continuous(expand = c(0, 0),limits=lims_y,breaks=c(-10,seq(0,lims_y[2],10))) +
    guides(colour=guide_legend(nrow=rows_legend),fill=guide_legend(nrow=1))+
    labs(y="Spot Prices ($US/bbl)",x="Year",
         title=title_sent,
         caption="Data via Bloomberg")+
    weekly_graphs()
  return(p)
}



#fixed to here

ngl_fractions<-function(){
  
  #CANADIAN DATA
  #Edmonton and Mont Belvieu Propane USd/gallon
  #Edmonton and Mont Belvieu Butane USd/gallon
  #Mont Belvieu Ethane is UDd/gallon
  #mont Belvieu Natural Gasoline is USd/gallon (do we convert it?)
  #Edmonton Condensate is USD/bbl
  #AECO NIT is CAD/gigajoule
  
  #send me prices for NGLs and the share of each - will force add to one with condensate share-  
  #if(!exists(names))
  names<-c("Mont Belvieu Ethane","Edmonton Propane",
           "Edmonton Butane","Edmonton Condensate","AECO NIT","Henry Hub",
           "Mont Belvieu Ethane","Mont Belvieu Propane",
           "Mont Belvieu Butane","Mont Belvieu Natural Gasoline")
  
  #testing data should be in $/bbl or boe
  frac_data<-data %>% select(c("Date","USDCAD",names))
  
  #force shares to sum to one
  #if(shares==FALSE)
  shares<-c(0.26,0.13,0.06)
  cond_share<-1-sum(shares[1:3])
  shares<-c(shares[1:3],cond_share)
  #btu/gal times 42 divided by 10^6 to get mmbtu per barrel
  mmbtu_gal<- c(66500,90900,102930,110100)/10^6 #ethane, propane, butane, condensate
  
  #set Edmonton Ethane to MBV*0.66
  frac_data<-frac_data %>% mutate(  #commodity into $/mmbtu
    ethane_value=`Mont Belvieu Ethane`*.66/mmbtu_gal[1]/100, #ngl in c/gal, so divide by 100 to get $/gal
    propane_value=`Edmonton Propane`/mmbtu_gal[2]/100,
    butane_value=`Edmonton Butane`/mmbtu_gal[3]/100,
    cond_value=`Edmonton Condensate`/mmbtu_gal[4]/42,#condensate was in $/bbl, so divide by 42 to get $/gal
    frac_value=ethane_value*shares[1]+propane_value*shares[2]+butane_value*shares[3]+cond_value*shares[4],
    #one GJ is 0.947817 MMBTU
    `AECO Gas (US_MMBTU)`=`AECO NIT`/USDCAD/0.947817,
    can_spread=frac_value-`AECO Gas (US_MMBTU)`,
    can_dry_gas=`AECO Gas (US_MMBTU)`,
    can_dry_gas_BOE=`AECO Gas (US_MMBTU)`*5.799,
    can_spread_BOE=can_spread*5.799,
    us_ethane_value=`Mont Belvieu Ethane`/mmbtu_gal[1]/100, #ngl in c/gal, so divide by 100 to get $/gal
    us_propane_value=`Mont Belvieu Propane`/mmbtu_gal[2]/100,
    us_butane_value=`Mont Belvieu Butane`/mmbtu_gal[3]/100,
    us_cond_value=`Mont Belvieu Natural Gasoline`/mmbtu_gal[4]/100,#condensate was in $/bbl, so divide by 42 to get $/gal
    us_frac_value=us_ethane_value*shares[1]+us_propane_value*shares[2]+us_butane_value*shares[3]+us_cond_value*shares[4],
    us_spread=us_frac_value-`Henry Hub`,
    us_spread_BOE=us_spread*5.799,
    us_dry_gas=`Henry Hub`,
    us_dry_gas_BOE=`Henry Hub`*5.799
  )
  frac_data%>%select(Date,us_spread,can_spread,us_spread_BOE,can_spread_BOE,can_dry_gas,us_dry_gas,can_dry_gas_BOE,us_dry_gas_BOE)
}

`%ni%` <- Negate(`%in%`)
gas_names<-c("us_spread","can_spread","us_spread_BOE","can_spread_BOE","can_dry_gas","us_dry_gas","can_dry_gas_BOE","us_dry_gas_BOE")
data<-subset(data,select = names(data) %ni% gas_names)
data<-data %>%left_join(ngl_fractions(),by="Date")

names<-c("Mont Belvieu Ethane","Mont Belvieu Propane",
         "Mont Belvieu Butane","Mont Belvieu Natural Gasoline")
labels<-names
test<-ngl_levels_chart(data=data,names,labels,5,"USD",title_sent="Mt Belvieu NGL Prices")
set_png("mbv_ngl.png")
print(test)
dev.off()

names<-c("Edmonton Propane",
         "Edmonton Butane","Edmonton Condensate")
labels<-names
test<-ngl_levels_chart(data=data,names,labels,5,"USD",title_sent="Edmonton NGL Prices",exempt_set = c("Edmonton Condensate"))
set_png("edm_ngl.png")
print(test)
dev.off()


#montney frac spread graph


ngl_spread_chart<-function(data_sent,names,labels_sent,years,curr="USD",title_sent="NGL Prices",break_set="12 months",exempt_set=NULL,spread_name="us_spread",file_name)
{
  #send a data set and two variable names
  #ngl data should be sent in $USd/gal unless names are in exempt_set
  #spread is $/mmbtu
  #print(exempt_set)
  top_panel<-ngl_levels_chart(data=data_sent,names,labels_sent,years,curr,title_sent,exempt_set=exempt_set)+
    theme(legend.text = element_text(colour="black", size = 11, face = "bold"))
  # now make the bottom panel
  data_sent<-filter(data_sent,Date>=max(Date)-years(years))
  df1<-reshape2::melt(data_sent,id=c("Date","USDCAD"),measure.vars = spread_name)
  df1<-na.omit(df1)
  lims<-c(max(df1$Date)-years(years),max(df1$Date)+months(1))
  bottom_panel<-ggplot(df1) +
    geom_area(data=filter(df1,variable==spread_name),aes(Date,value,group = variable,fill="Approximate Fractionation Spread"))+
    scale_fill_manual(NULL,values=colors_ua10()[5])+
    scale_colour_manual(NULL,values=colors_ua10(),labels=labels_sent)+
    scale_x_date(name=NULL,date_breaks = break_set, date_labels =  "%b\n%Y",expand=c(0,0),limits = lims) +
    scale_y_continuous(expand = c(0, 0)) +
    guides(colour=guide_legend(nrow=2),fill=guide_legend(nrow=1))+
    labs(y="Fractionation Spread ($US/MMBtu)",x="")+
    weekly_graphs()+
    theme(legend.text = element_text(colour="black", size = 11, face = "bold"))
  ifelse(grepl("can",spread_name),
         bottom_panel<-bottom_panel+labs(caption="Assumed NGL fractions are 26% ethane, 13% propane, 6% butane and 55% condensate. Ethane value approximated at 66% of Edmonton propane. Spread is relative to AECO/NIT gas."),
         bottom_panel<-bottom_panel+labs(caption="Assumed NGL fractions are 26% ethane, 13% propane, 6% butane and 55% condensate. Spread is relative to Henry Hub gas."))
  #extract legend
  #https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
  g_legend<-function(a.gplot){
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)}
  
  mylegend<-arrangeGrob(g_legend(top_panel),g_legend(bottom_panel), nrow=2)
  
  #grid_arrange_shared_legend(top_panel,gridExtra::arrangeGrob(bottom_panel, ncol=1), ncol=1, nrow=2)
  
  set_png(file=paste(file_name,sep=""))
  grid.arrange(arrangeGrob(top_panel + theme(legend.position="none",
                                             legend.margin=margin(c(0,0,0,0),unit="cm"),
                                             legend.text = element_text(colour="black", size = 14, face = "bold"),
                                             plot.caption = element_blank(),
                                             plot.title = element_blank(),
                                             plot.subtitle = element_text(size = 14, face = "italic"),
                                             panel.grid.minor = element_blank(),
                                             text = element_text(size = 10,face = "bold"),
                                             axis.text = element_text(size = 10,face = "bold", colour="black"),
                                             axis.text.x = element_blank()
  ),
  bottom_panel +
    theme(legend.position="none",
          plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(size = 16, face = "italic"),
          plot.caption = element_text(size = 5, face = "italic"),
          panel.grid.minor = element_blank(),
          text = element_text(size = 10,face = "bold"),
          axis.text = element_text(size = 10,face = "bold", colour="black"),
          axis.title.y = element_text(margin = margin(t = 0, r = 13, b = 0, l = 0))),
  ncol=1,heights=c(3,2.5)),
  mylegend, 
  nrow=2,heights=c(10, 1),
  bottom =text_grob(
    "Source: Data via Bloomberg, graph by Andrew Leach",
    face = "italic", color = "black",size=8,just="center",lineheight = 1
  ),
  top =text_grob(
    "NGL Prices and Implied Fractionation Spread",
    face = "bold", color = "black",size=8,just="center",lineheight = 1
  )
  
  )
  dev.off()  
  
}


names<-c("Edmonton Propane",
         "Edmonton Butane","Edmonton Condensate")
labels<-names
ngl_spread_chart(data=data,names,labels,5,"USD",title_sent="Edmonton NGL Prices",exempt_set = c("Edmonton Condensate"),spread_name = "can_spread",file_name = "can_ngls.png")


names<-c("Mont Belvieu Ethane","Mont Belvieu Propane",
         "Mont Belvieu Butane","Mont Belvieu Natural Gasoline")
labels<-names
ngl_spread_chart(data=data,names,labels,5,"USD",title_sent="Mont Belvieu NGL Prices",exempt_set = c("Edmonton Condensate"),spread_name = "us_spread",file_name = "us_ngls.png")











#gas charts
#UK gas is in pence per therm
#convert to USD/mmbtu
#1 therm is 0.10 MMBtu
data$`NBP Gas MMBTU`<-data$`NBP Gas`/.1*data$`GBP USD`/100


#seasonality

#gas price graphs
df1<-data %>% select(Date,`Henry Hub`,`AECO NIT`,`Japan LNG JCC`,`NBP Gas MMBTU`)%>%
  reshape2::melt(id=c("Date")) %>%
  mutate(year=year(Date),month=month(Date),week=week(Date))

png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("global_gas.png")
p<-ggplot(df1)+geom_line(aes(Date,value,group=variable,colour=variable),size=1.25)+
  scale_colour_manual(NULL,values=colors_ua10())+
  scale_x_date(name=NULL,date_breaks = "18 months", date_labels =  "%b\n%Y",expand=c(0,0)) +
  labs(y="Natural Gas Spot or Most Recent Price ($US/MMBtu)",x="Date",
       title="Global natural gas prices",
       caption="Data via Bloomberg, calculations and graph by Andrew Leach")+
  weekly_graphs()
print(p)
dev.off()


## @knitr forwards_prelims


cad_forwards<-function(){
  forwards_read <- read.xlsx(xlsxFile = bb_file, sheet = "cad_fwd_daily", startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
  forwards_data<-forwards_read[-seq(1,2),] #clip the column headers
  forwards_data<-forwards_data[,c(1,seq(2,ncol(forwards_data),by=3))]
  forwards_data <- data.frame(lapply(forwards_data, function(x) {
    gsub("#N/A N/A", "", x)
  }))
  forwards_data[,-1] = apply(forwards_data[,-1], 2, function(x) as.numeric(as.character(x)))
  forwards_data[,1] = ymd(as.character(forwards_data[,1]))
  #add premia to spot value (premia is 1/10000 basis)
  forwards_data[,-c(1,2)] = apply(forwards_data[,-c(1,2)], 2, function(x) x/10000)
  forwards_data[,-c(1,2)] <- forwards_data[,-c(1,2)]+forwards_data[,2]
  name_list<-c("Date","Spot",forwards_read[1,seq(4,ncol(forwards_read),by=3)])
  name_list<-gsub("CAD","",name_list)
  name_list<-gsub(" Curncy","",name_list)
  name_list<-gsub("M","",name_list)
  name_list<-gsub("2Y","24",name_list)
  name_list<-gsub("3Y","36",name_list)
  name_list<-gsub("4Y","48",name_list)
  name_list<-gsub("5Y","60",name_list)
  name_list<-gsub("6Y","72",name_list)
  name_list<-gsub("7Y","84",name_list)
  name_list<-gsub("8Y","96",name_list)
  name_list<-gsub("9Y","108",name_list)
  names(forwards_data)<-name_list
  if(any(grepl("#N/A Requesting",forwards_data[1,])))
    forwards_data<-forwards_data[-1,] #take off the first row if you didn't let the refresh finish
  forwards_data
}

forwards_graphs<-function(data_sent,units_sent,title_sent,single=1,relative_labels=1,lag=0){
  #assume last trading day is 25th calendar day of each month - still to fix
  #testing
  #comdty<-"WTI"
  #title_sent<-"WTI Forward Contract"
  #single<-1
  #relative_labels<-1
  #data_sent<-data_retrieve[[1]]
  #units_sent<-data_retrieve[[2]]
  # lag<-0
  forwards_data<-data_sent
  units<-units_sent
  df1<-reshape2::melt(forwards_data,id=c("Date"),variable.name = "Instrument")
  df1<-na.omit(df1)
  #operator %m+% in lubridate adds a month without exceeding days in month for target
  df1<-df1 %>% mutate(
    Inst_Date=Date%m+%months(as.numeric(gsub("Month ","", as.character(Instrument)))),
    Inst_Date=Inst_Date%m+%months(ifelse(day(Date)>25,1,0)), #add an extra month if we're past calendar day 25
    Inst_Year=year(Inst_Date),
    Inst_Month=month(Inst_Date),
    Inst_Date=ymd(paste(Inst_Year,Inst_Month,15,sep = "-")) #set instrument date to the 15th of each month
  )
  date_max<-max(df1$Date)-lag #drop back 1 day if you grabbed mid_day data.
  date_list<-rev(c(date_max,date_max-weeks(1),date_max-weeks(2)))
  #print(relative_labels)
  labs<-c(as.character(date_max),"1 week previous","2 weeks previous")
  label_list<-rev(paste("Forward strip, ",format(ymd(date_list), "%b %d, %Y"),sep = ""))
  if(single==1){
    date_list<-c(date_max)
    labs<-c(as.character(date_max))
    label_list<-rev(paste("Forward strip, ",format(ymd(date_list), "%b %d, %Y"),sep = ""))
  }
  if(relative_labels==0)
    labs<-label_list
  p<-ggplot(subset(df1,Date %in% date_list)) +
    geom_line(aes(Inst_Date,value,group=as.factor(Date),colour=as.factor(Date)),size=1.25)+
    #geom_vline(xintercept = as.Date("2017-04-19")+
    #scale_color_brewer("Calendar Strip Year",palette = "Set1")+
    #scale_y_continuous(limits=c(0,max(df1%>% filter(Date %in% date_list) %>% select(value))))+
    scale_color_manual("",values = c(colors_ua10()[1:5],"Black"),labels=rev(labs))+
    scale_x_date(breaks = date_breaks(width="12 months"),labels = date_format("%b\n%Y"))+
    guides(colour = guide_legend(reverse = TRUE))+
    weekly_graphs()+
    labs(y=paste("Settlement Price (",units,")",sep=""),x="Instrument Date",
         title=paste(title_sent,"Strips"),
         caption="Source: Data via Bloomberg")
  p
}



cad_hedged_forwards_graphs<-function(){
  #assume last trading day is 25th calendar day of each month - still to fix
  #setup
  title_sent<-"WTI forward"
  single<-0
  relative_labels<-1
  data_retrieve<-forwards_data_pull("WTI")
  data_sent<-data_retrieve[[1]]
  units_sent<-data_retrieve[[2]]
  lag<-0
  #end testing
  forwards_data<-data_sent
  units<-"$CA/bbl"
  df1<-reshape2::melt(forwards_data,id=c("Date"),variable.name = "Instrument")
  df1<-na.omit(df1)
  long_squeeze<-ymd("2020-04-20")
  
  #operator %m+% in lubridate adds a month without exceeding days in month for target
  df1<-df1 %>% mutate(
    Inst_Date=Date%m+%months(as.numeric(gsub("Month ","", as.character(Instrument)))),
    Inst_Date=Inst_Date%m+%months(ifelse(day(Date)>20,1,0)), #add an extra month if we're past calendar day 20
    Inst_Year=year(Inst_Date),
    Inst_Month=month(Inst_Date),
    Inst_Date=ymd(paste(Inst_Year,Inst_Month,20,sep = "-")) #set instrument date to the 15th of each month
  )
  
  #df1<-df1 %>% filter(Date!=long_squeeze)
  
  date_max<-max(df1$Date)-lag #drop back 1 day if you grabbed mid_day data.
  #get cads
  cad_df<-cad_forwards()
  names(cad_df)[2]<-"0"
  cad_df1<-reshape2::melt(cad_df,id=c("Date"),variable.name = "FX_Contract",value.name = "cad_fx")
  cad_df1<-na.omit(cad_df1)
  cad_df1<-cad_df1 %>% mutate(
    Inst_Date=Date%m+%months(as.numeric(gsub("Month ","", as.character(FX_Contract)))),
    Inst_Date=Inst_Date%m+%months(ifelse(day(Date)>20,1,0)), #add an extra month if we're past calendar day 20
    Inst_Year=year(Inst_Date),
    Inst_Month=month(Inst_Date),
    Inst_Date=ymd(paste(Inst_Year,Inst_Month,20,sep = "-")) #set instrument date to the 15th of each month
  )
  #join cads and commodity data
  df_test<-left_join(df1,cad_df1,by=c("Date","Inst_Date","Inst_Year","Inst_Month")) %>% filter(!is.na(cad_fx))
  budget_day<-ymd("2019-10-24")
  budget_day_2020<-ymd("2020-02-27")
  #date_max<-ymd("2020-01-01")
  date_list<-rev(c(date_max,budget_day,budget_day_2020))
  #print(relative_labels)
  #labs<-c(format(ymd(date_max), "%b %d, %Y"),"1 week previous","1 year previous","2 years previous","3 years previous","5 years previous")
  labs<-c(format(ymd(date_max), "%b %d, %Y"),"Budget Day, 2020","Budget Day, 2019")
  labs=c("Alberta Budget 2020 Outlook","Alberta Budget 2019 Outlook",labs)
  label_list<-rev(paste("Forward strip, ",format(ymd(date_list), "%b %d, %Y"),sep = ""))
  label_list=c("Alberta Budget 2020 Outlook","Alberta Budget 2019 Outlook","Front Month Prices",label_list)
  if(single==1){
    date_list<-c(date_max)
    labs<-c(as.character(date_max))
    label_list<-rev(paste("Forward strip, ",format(ymd(date_list), "%b %d, %Y"),sep = ""))
    labs=c("Alberta Budget 2020 Outlook","Alberta Budget 2019 Outlook",labs)
    label_list=c("Alberta Budget 2020 Outlook","Alberta Budget 2019 Outlook",label_list)
  }
  relative_labels<-1
  if(relative_labels==0)
    labs<-label_list
  
  p_usd<-ggplot(filter(df1,Date %in% date_list,Inst_Date<=Date+months(60))) +
    geom_line(data=filter(df1,Date>=budget_day,Date<=ymd("2020-02-27")),aes(Inst_Date,value,group=Date),color="grey70",size=.5)+
    geom_line(data=filter(df1,Date>=budget_day_2020),aes(Inst_Date,value,group=Date),color="firebrick",size=.5)+
    geom_line(aes(Inst_Date,value,group=factor(Date,levels=as.character(date_list)),colour=factor(Date,levels=as.character(date_list))),size=2)+
    geom_point(aes(Inst_Date,value,group=factor(Date,levels=as.character(date_list)),colour=factor(Date,levels=as.character(date_list))),shape=21,size=2,fill="white")+
    #geom_line(data=filter(df_test,Date>=budget_day,Instrument=="Month 1"),aes(Date,value,colour="ZZZ"),size=.75)+
    #geom_vline(xintercept = as.Date("2017-04-19")+
    #scale_color_brewer("Calendar Strip Year",palette = "Set1")+
    #scale_y_continuous(limits=c(0,max(df1%>% filter(Date %in% date_list) %>% select(value))))+
    scale_color_manual("",values = tail(c(colors_ua10()[c(1:4,6,7)],"Black","Red","Dodgerblue"),length(date_list)+2),labels=rev(labs))+
    scale_x_date(breaks = date_breaks(width="4 months"),labels = date_format("%b\n%Y"),limits=c(budget_day-months(3),date_max+months(60)))+
    scale_y_continuous(breaks = pretty_breaks(),limits = c(0,70))+
    guides(colour = guide_legend(reverse = TRUE,nrow = 2))+
    weekly_graphs()+
    labs(y=paste("Settlement Price ($US/bbl)",sep=""),x="Instrument Date",
         title=paste(title_sent,"strips"),
         subtitle="Forward strips for all other dates from Alberta Budget Day 2019 to Alberta Budget Day 2020 shown in grey. Days since shown in red.",
         caption="Source: Data via Bloomberg. April 20, 2020 forward curve omitted.")
  #p_usd  
  
  data_test<-filter(df_test,Date >= budget_day)
  max_inst=max(data_test$Inst_Date)
  
  
  #testing<-filter(df_test,Date %in% date_list,Inst_Date<=Date+months(60))
  
  p_cad<-ggplot(filter(df_test,Date %in% date_list,Inst_Date<=Date+months(60))) +
    #geom_line(data=subset(df_test,Date >= budget_day),aes(Inst_Date,value*cad_fx,group=Date),color="grey80",size=.15)+
    geom_line(data=filter(df_test,Date>=budget_day,Date<=ymd("2020-02-27")),aes(Inst_Date,value*cad_fx,group=Date),color="grey70",size=.5)+
    geom_line(data=filter(df_test,Date>=budget_day_2020),aes(Inst_Date,value*cad_fx,group=Date),color="firebrick",size=.5)+
    geom_line(aes(Inst_Date,value*cad_fx,group=factor(Date,levels=as.character(date_list)),colour=factor(Date,levels=as.character(date_list))),size=2)+
    geom_point(aes(Inst_Date,value*cad_fx,group=factor(Date,levels=as.character(date_list)),colour=factor(Date,levels=as.character(date_list))),shape=21,size=2,fill="white")+
    #geom_vline(xintercept = as.Date("2017-04-19")+
    #scale_color_brewer("Calendar Strip Year",palette = "Set1")+
    #scale_y_continuous(limits=c(0,max(df1%>% filter(Date %in% date_list) %>% select(value))))+
    scale_color_manual("",values = tail(c(colors_ua10()[c(1:4,6,7)],"Black","Red","Dodgerblue"),length(date_list)+2),labels=rev(labs))+
    scale_x_date(breaks = date_breaks(width="4 months"),labels = date_format("%b\n%Y"),limits=c(budget_day-months(3),date_max+months(37)),expand = c(0,0))+
    scale_y_continuous(breaks = pretty_breaks(),limits = c(0,90))+
    guides(colour = guide_legend(reverse = TRUE,nrow = 2))+
    weekly_graphs()+
    labs(y=paste("CAD-Hedged Settlement Price (",units,")",sep=""),x="Instrument Date",
         title=paste(title_sent,"strip interacted with Canadian dollar forward contracts"),
         subtitle="Forward strips for all other dates from Alberta Budget Day 2019 to Budget Day 2020 shown in grey. Days since shown in red.",
         
         caption="Source: Data via Bloomberg. April 20, 2020 forward curve omitted.")
  p_cad 
  add_budget<-1
  if(add_budget==1){
    
    budget <- data.frame("Date" = c("2017-10-1"	,"2018-10-01","2019-10-01","2020-10-01","	2021-10-01","	2022-10-01"), 
                         "WTI_CAD" = c(68.83,82.27,76.00,76.32,80.52,80.77),
                         "WTI" = c(53.69,62.77,57.00,58.00,62.00,63.00))
    budget$Date=ymd(budget$Date)
    budget_2020 <- data.frame("Date" = c("2017-10-1"	,"2018-10-01","2019-10-01","2020-10-01","	2021-10-01","	2022-10-01"), 
                              "WTI_CAD" = c(68.83,82.27,76.82,75.82,80.52,81.29),
                              "WTI" = c(53.69,62.77,58.00,58.00,62.00,63.00))
    
    budget_2020$Date=ymd(budget_2020$Date)
    p_cad<-p_cad+geom_line(data=budget,aes(Date,WTI_CAD,colour="AB_Budget"),size=2,linetype=2)+
      geom_point(data=budget,aes(Date,WTI_CAD,colour="AB_Budget"),shape=21,size=2,fill="white")+
      geom_line(data=budget_2020,aes(Date,WTI_CAD,colour="AB_Budget_2020"),size=2,linetype=2)+
      geom_point(data=budget_2020,aes(Date,WTI_CAD,colour="AB_Budget_2020"),shape=21,size=2,fill="white")
    
    p_usd<-p_usd+geom_line(data=budget,aes(Date,WTI,colour="AB_Budget"),size=2,linetype=2)+
      geom_point(data=budget,aes(Date,WTI,colour="AB_Budget"),shape=21,size=2,fill="white")+
      geom_line(data=budget_2020,aes(Date,WTI,colour="AB_Budget_2020"),size=1.5,linetype=2)+
      geom_point(data=budget_2020,aes(Date,WTI,colour="AB_Budget_2020"),shape=21,size=1.5,fill="white")
  }
  p_cad
  p_usd  
  ggsave("p_cad.png",plot=p_cad, width=14, height=7)  
  ggsave("p_usd.png",plot=p_usd,width=14, height=7)  
}


## @knitr hedged_forwards

cad_hedged_forwards_graphs()

## @knitr nwr_graphs




data$NWR<-(40250*data$`ULSD Edmonton`/100*168/data$USDCAD+(8790+28266)*data$`Edmonton Condensate`+3363*data$`Edmonton Propane`/1000*42)-78000*data$WCS
data$NWR<-data$NWR/78000
#	ULSD Avg Unbnd Edmton	CAd/liter	CRUMEDAG Index ULSD Edmonton		CAd
# 	N Am Propane Edmonton	USd/gallon	LPGSEDPP Index Edmoton Propane		USd	
#	Syncrude Sweet Blend Crude Oil	USD/barrel	USCRSYNC Index Syncrude Sweet Blend Crude Oil		USD	
# Western Canada Select Crude Px	USD/barrel	USCRWCAS Index WCS		USD	
#C5 Condensate Spot Px Edmonton Condensate		USD/barrel Edmonton Condensate		USD


#tolls: use 1.014 billion per year for 37500 barrels of bitumen processed. 
#pro-rated, that's 1.352 billion per year for the refinery
#47.48 dollars per barrel output.

ggplot(data=filter(data,Date>=ymd("2011-02-16")))+geom_line(aes(Date,NWR*USDCAD))+geom_hline(aes(yintercept=47.48))

#but that's a false way to look at it, because what the province has is bitumen, not WCS, from royalty in kind
#the cost of good sold, i.e. per barrel of bitumen processed, means you have to buy the diluent. To produce a barrel of WCS, you needed 
#.3 barrels of diluent


data$NWR_bitumen<-(40250*data$`ULSD Edmonton`/100*168/data$USDCAD+ #40250 barrels per day of diesel in Canadian cents per litre converted to USD/barrel
                     (8790+28266)*data$`Edmonton Condensate`+ #VGO valued at condensate price
                     3363*data$`Edmonton Propane`/100*42)- #butane and propane in USd per gallon scale to USD per barrel
  78000*data$WCS

data$NWR_maya<-(40250*data$`ULSD Edmonton`/100*168/data$USDCAD+ #40250 barrels per day of diesel in Canadian cents per litre converted to USD/barrel
                  (8790+28266)*data$`Edmonton Condensate`+ #VGO valued at condensate price
                  3363*data$`Edmonton Propane`/100*42)- #butane and propane in USd per gallon scale to USD per barrel
  78000*(data$Maya-7.50)
data$NWR_maya<-data$NWR_maya/50000*data$USDCAD


data$NWR_bitumen<-data$NWR_bitumen/50000*data$USDCAD


data$wcs_maya<-(data$Maya-data$WCS)*data$USDCAD

data$wcs_wti<-(data$WTI-data$WCS)*data$USDCAD

ggplot(data) +geom_line(aes(Date,`ULSD Edmonton`/100*168/USDCAD,colour="Diesel"))+
  geom_line(aes(Date,`Edmonton Condensate`,colour="Cond"))+
  geom_line(aes(Date,`Edmonton Propane`/100*42,colour="Propane"))+
  geom_line(aes(Date,WCS,colour="WCS"))+
  
  ggplot(data) +geom_line(aes(Date,NWR_bitumen,colour="gross margin, WCS basis"))+
  geom_line(aes(Date,NWR_maya,colour="gross margin, Maya net toll basis"))

scale_color_manual(values=colors_tableau10())

blake_theme<-function(){
  theme_hc(20)+
    theme(plot.subtitle = element_text(color="grey10",size=rel(.7)),
          plot.title = element_text(face="bold"),
          plot.caption = element_text(color="grey50",size=rel(.5),hjust=0),
          plot.caption.position =  "plot",
          legend.title = element_text(color="grey10",size=rel(.5)),
          legend.text = element_text(color="grey10",size=rel(.5)),
          axis.title = element_text(size=rel(.8)),
          axis.ticks = element_blank(),
          panel.spacing = unit(2,"lines"),
          legend.position = "none",
          plot.margin = margin(t = .75, r = 1, b = .75, l = 1,unit= "cm"),
          axis.text.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))
          
    )
}

blakes_blue<-"#4477AA"

graph_grey<-"grey20"
graph_black<-"black"
data<-data %>% mutate(
  nwr_wcs_7ma=roll::roll_mean(NWR_bitumen,30),
  nwr_maya_7ma=roll::roll_mean(NWR_maya,30),
  wcs_maya_7ma=roll::roll_mean(wcs_maya,30),
  wcs_wti_7ma=roll::roll_mean(wcs_wti,30),
  
)



ggplot(data%>%filter(Date>ymd("2015-09-01"))%>%
          select(Date,NWR_bitumen,NWR_maya,wcs_maya,wcs_wti)%>%
           mutate(
           nwr_wcs_7ma=roll::roll_mean(NWR_bitumen,30),
           nwr_maya_7ma=roll::roll_mean(NWR_maya,30),
           wcs_maya_7ma=roll::roll_mean(wcs_maya,30),
           wcs_wti_7ma=roll::roll_mean(wcs_wti,30)))+
  geom_line(aes(Date,nwr_wcs_7ma,color="Hypothetical NWR gross margin"),size=1.5)+
  geom_line(aes(Date,wcs_wti_7ma,color="WTI-WCS differential"),size=1.5)+
  #geom_line(aes(Date,73.059,color="2018 NWSR Toll Estimate"),size=1.5,linetype="dashed")+
  #geom_line(aes(Date,39.67,color="2012 NWSR Toll Estimate"),size=1.5,linetype="dashed")+
  geom_line(aes(Date,73.059),size=1.5,linetype="dashed",color="black")+
  geom_line(aes(Date,39.67),size=1.5,linetype="dashed",color="black")+
  annotate("text",x = ymd("2015-09-05"), y=73+5,label = "2018 NWSR Toll Estimate, $73.06/bbl",size=rel(2.5),hjust=0,vjust=.5)+

  annotate("text",x = ymd("2015-09-05"), y=39.67+5,label = " 2012 NWSR Toll Estimate, $39.67/bbl ",size=rel(2.5),hjust=0,vjust=.5)+
  
  theme_minimal()+weekly_graphs()+
  scale_x_date(date_labels = "%b\n%Y",date_breaks = "12 months",expand=c(0,0) )+
  #expand_limits(x = as.Date(c("2015-09-01", "2020-10-01")))+
  expand_limits(y = c(0,180))+
  scale_y_continuous(expand = c(0,0),breaks=pretty_breaks(8))+
  #scale_shape_manual("",values=c(16,16))+
  #scale_fill_manual("",values=colors_tableau10())+
  scale_color_manual("",values=c(colors_ua10()[c(1,3)]))+
  guides(colour=guide_legend(),shape="none")+
  theme(axis.text.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        legend.position = "bottom",
        legend.key.width = unit(2,"cm"))+
  labs(x="",y="Margin or spread (CA$ per barrel)",
       #title="Coal and Gas Generation and Carbon Prices (MW, 2007-2015)",
       #title="Adjusted Drops in Generation by Plant Type (MW, 2020)",
       caption="Source: Prices via Bloomberg, contract details via Alberta Energy Annual Reports, 2013-2014 and 2019-2020, calculations by Andrew Leach. Margins and spreads shown at 30 day moving averages.",
       NULL)+
  NULL  
ggsave("nwr.png",width = 14, height=7,dpi=600,bg="white")



## @knitr forwards_graphs

data_retrieve<-forwards_data_pull("WTI")
set_png("wti_single.png")
forwards_graphs(data_retrieve[[1]],data_retrieve[[2]],"WTI Forward Contract",single=1,relative_labels = 1,lag=1)
dev.off()
#forwards_graphs("WTI","WTI Forward Contract",single=1,relative_labels = 1)
set_png("wti_mult.png")
forwards_graphs(data_retrieve[[1]],data_retrieve[[2]],"WTI Forward Contract",single=0,relative_labels = 1,lag=1)
dev.off()




wcs_data_retrieve<-forwards_data_pull("WTI-WCS")
forwards_graphs(wcs_data_retrieve[[1]],wcs_data_retrieve[[2]],"Bloomberg WTI-WCS Fair Value Swaps",single=0,relative_labels = 1,lag=0)


data_retrieve<-forwards_data_pull("Brent")
forwards_graphs(data_retrieve[[1]],data_retrieve[[2]],"Brent Forward Contract",single=0,relative_labels = 1,lag=1)


#don't use lags with weekly data unless you know what you're doing
data_retrieve<-forwards_data_pull("Brent-WTI")
set_png("brent_wti_single.png")
forwards_graphs(data_retrieve[[1]],data_retrieve[[2]],"Brent-WTI Implied Forward Differential",single=1,relative_labels = 1,lag=0)
dev.off()

set_png("brent_wti_mult.png")
forwards_graphs(data_retrieve[[1]],data_retrieve[[2]],"Brent-WTI Implied Forward Differential",single=0,relative_labels = 1,lag=0)
dev.off()


#forwards_graphs("Brent","Brent Forward Contract")
#forwards_graphs("Henry Hub","Henry Hub Gas Forward Contract")


data_retrieve<-forwards_data_pull("AECO to Henry Hub")
set_png("nit_hh_mult.png")
forwards_graphs(data_retrieve[[1]],data_retrieve[[2]],"AECO-Henry Hub Implied Forward Differential",single=0,relative_labels = 1,lag=0)
dev.off()

set_png("nit_hh_single.png")
forwards_graphs(data_retrieve[[1]],data_retrieve[[2]],"AECO-Henry Hub Implied Forward Differential",single=1,relative_labels = 1,lag=0)
dev.off()


#forwards_graphs("WTI-WCS","WTI vs WCS Fair Value Basis Differential")

#forwards_graphs("Brent-WTI","WTI vs Brent Fair Value Basis Differential")






## @knitr NGX_forwards


#NGX forwards data

ngx_data_read<-function(file_name){
  #testing stuff
  #file_name<-"NGX_gas_forwards.csv"
  ngx_data <- read.csv(paste(nrg_folder,file_name,sep="/"),blank.lines.skip=T,stringsAsFactors=F,header=F)
  #figure out where individual series start NGX and NYMEX data
  series<-ngx_data[grep("Trade Date",ngx_data$V1)-1,1]
  series<-sapply(strsplit(series,"\\ - "), `[`, 1) #cut out the other crap in the series labels
  series<-gsub("NGX ","",series) #cut out the other crap in the series labels
  header_rows<-grep("Trade Date",ngx_data$V1)[] #find the headers rows
  units<-gsub("High ","",ngx_data[header_rows,5]) #extract units of measure from the headers
  volumes<-gsub("Volume ","",ngx_data[header_rows,7]) #extract units of measure from the headers
  series_rows<-grep("Trade Date",ngx_data$V1)-1
  start_rows<-header_rows+1
  end_rows<-c((grep("Trade Date",ngx_data$V1)-2)[-1],NROW(ngx_data)) #skip the header and series rows
  ngx_data$series<-""
  ngx_data$price_units<-""
  ngx_data$volume_units<-""
  for(sets in seq(1,NROW(start_rows))){
    ngx_data$series[start_rows[sets]:end_rows[sets]]<-series[sets]
    ngx_data$price_units[start_rows[sets]:end_rows[sets]]<-units[sets]
    ngx_data$volume_units[start_rows[sets]:end_rows[sets]]<-volumes[sets]
  }
  names(ngx_data)<-ngx_data[2,]
  names(ngx_data)[9:11]<-c("series","price_units","volume_units")
  ngx_data<-ngx_data[-c(series_rows,header_rows),] #strip out series and header rows
  names(ngx_data)<-gsub(" USD/MMbtu","",names(ngx_data))
  names(ngx_data)<-gsub(" MMbtu","",names(ngx_data))
  #fix column types
  ngx_data[,1:2] <- lapply(ngx_data[,1:2], mdy)
  ngx_data[,3:8] <- lapply(ngx_data[,3:8], as.numeric)
  #ngx_data[,9:11] <- lapply(ngx_data[,9:11],factor)
  ngx_data<-clean_names(ngx_data)
  ngx_data <- ngx_data %>% group_by(trade_date,series,price_units) %>% mutate(spot_price=first(settle),trade_month=month(trade_date),trade_year=year(trade_date),
                                                                              inst_month=month(instrument_date),inst_year=year(instrument_date)) %>%
    ungroup() %>% 
    group_by(trade_month,trade_year,instrument_date,series,price_units) %>% mutate(monthly_spot=mean(settle)) %>% 
    ungroup()
  ngx_data[ngx_data$settle!=0,] #take out ones that are exactly zero
}

ngx_old_data<-function(){
  #build ngx data background
  ngx_data_old<-ngx_data_read("NGX_gas_forwards_03_04.csv")
  ngx_data_old<-rbind(ngx_data_old,ngx_data_read("NGX_gas_forwards_05_06.csv"))
  #edited 2007-06-20 data to be the same as 2007-06-19 because of a scrape error (negative prices)
  ngx_data_old<-rbind(ngx_data_old,ngx_data_read("NGX_gas_forwards_07_08.csv"))
  ngx_data_old<-rbind(ngx_data_old,ngx_data_read("NGX_gas_forwards_09_10.csv"))
  ngx_data_old<-rbind(ngx_data_old,ngx_data_read("NGX_gas_forwards_11_12.csv"))
  ngx_data_old<-rbind(ngx_data_old,ngx_data_read("NGX_gas_forwards_13_14.csv"))
  ngx_data_old<-rbind(ngx_data_old,ngx_data_read("NGX_gas_forwards_16.csv"))
  ngx_data_old<-rbind(ngx_data_old,ngx_data_read("NGX_gas_forwards_17.csv"))
  save(ngx_data_old, file= "ngx_gas_old.RData")
}

#ngx_old_data()

load(file=paste(nrg_folder,"ngx_gas_old.RData",sep="/"))
ngx_data<-rbind(ngx_data_old,ngx_data_read("NGX_gas_forwards.csv"))



#send date list and labels
#data should be formatted with 
#trade_date
#trade_month
#trade_year
#instrument date
#spot price  - front month price for every trade
#settle
ngx_forwards<-function(data_sent,date_list,label_list,series_sent,title_sent,units_sent){
  labs<-c("Spot Prices",label_list)
  lab_unit<-ifelse(units_sent=="USD/MMbtu","$US/MMbtu","$CAD/GJ")
  df1<-subset(ngx_data,(trade_date %in% date_list) & (series%in%series_sent) & (price_units==units_sent))
  df1$curve<-paste(df1$series,"forward strip,",format(df1$trade_date, "%b %d, %Y"),sep = " ")
  num_series<-NROW(unique(df1$curve))
  label_list<-levels(as.factor(df1$curve))
  rows_legend<-max(1,round(sum(nchar(label_list))/80))
  ggplot()+weekly_graphs()+
    #geom_line(data=subset(ngx_data,settle==spot_price& series==series_sent&(price_units==units_sent)),aes(trade_date,spot_price,colour="Spot Prices"))+
    geom_line(data=df1,
              aes(instrument_date,settle,group=as.factor(curve),colour=as.factor(curve)),size=1)+
    scale_color_manual("",values = c(colors_ua10()[1:num_series]),labels=label_list)+
    scale_x_date(breaks = date_breaks(width="12 months"),labels = date_format("%b\n%Y"))+
    guides(colour=guide_legend(nrow=rows_legend),reverse=TRUE)+
    theme(axis.title.y = element_text(size = 11,face = "bold", colour="black",margin = margin(r = 10)) )+
    labs(y=paste("Settlement Price (",lab_unit,")",sep=""),x="Settlement Date",
         title=paste("Natural Gas Forward Strips",sep = ""),
         subtitle=title_sent,
         caption="Source: NGX Settlement Data via NRGStream")
  
}



#send date list and labels
#data should be formatted with 
#trade_date
#trade_month
#trade_year
#instrument date
#spot price  - front month price for every trade
#settle
forward_spaghetti<-function(data_sent,date_list,label_list,series_sent,title_sent,units_sent){
  labs<-c("Spot Prices",label_list)
  lab_unit<-ifelse(units_sent=="USD/MMbtu","$US/MMbtu","$CAD/GJ")
  num_series<-NROW(labs)
  ggplot()+
    geom_line(data=subset(ngx_data,settle==spot_price& series==series_sent&(price_units==units_sent)),aes(trade_date,spot_price,colour="Spot Prices"))+
    geom_line(data=subset(ngx_data,(trade_date %in% date_list) & (series==series_sent) & (price_units==units_sent)),
              aes(instrument_date,settle,group=as.factor(trade_date),colour=as.factor(trade_date)),size=1)+
    scale_color_manual("",values = c(colors_ua10()[1:num_series-1],"Black"),labels=rev(labs))+
    scale_x_date(breaks = date_breaks(width="12 months"),labels = date_format("%b\n%Y"))+
    guides(colour = guide_legend(reverse = TRUE))+
    weekly_small()+theme(axis.title.y = element_text(size = 11,face = "bold", colour="black",margin = margin(r = 10)) )+
    labs(y=paste("Settlement Price (",lab_unit,")",sep=""),x="Settlement Date",
         title=paste(title_sent," Natural Gas Spot Prices and Forward Strips",sep = ""),
         caption="Source: NGX Settlement Data via NRGStream")
  
}




## @knitr NGX_forward_graphs



date_max<-max(ngx_data$trade_date)
series_sent<-c("Union Dawn","AB-NIT Fixed")
date_list<-rev(c(date_max,date_max-months(1)))
label_list<-rev(paste("Forward strip, ",format(ymd(date_list), "%b %d, %Y"),sep = ""))
title_sent<-"Ontario Union Dawn and Alberta NIT"
print(ngx_forwards(ngx_data,date_list,label_list,series_sent,title_sent,units_sent="USD/MMbtu"))


date_max<-max(ngx_data$trade_date)
series_sent<-c("Spectra-Stn 2","AB-NIT Fixed")
date_list<-rev(c(date_max))
label_list<-c("Station 2","Alberta NIT")
title_sent<-"BC Station 2 and Alberta NIT"
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("NIT_STN2_forwards.png")
print(ngx_forwards(ngx_data,date_list,label_list,series_sent,title_sent,units_sent="USD/MMbtu"))
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()





date_max<-max(ngx_data$trade_date)
series_sent<-c("AB-NIT Fixed")
date_list<-rev(c(date_max))
label_list<-c("Alberta NIT")
title_sent<-"Alberta NIT"

if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("NIT_forward_strip.png")
print(ngx_forwards(ngx_data,date_list,label_list,series_sent,title_sent,units_sent="CAD/GJ"))
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()




date_max<-max(ngx_data$trade_date)
date_list<-rev(c(date_max,date_max-months(1),date_max-months(2),date_max-months(3),date_max-months(6)))
label_list<-c(as.character(date_max),"1 month previous","2 months previous","3 months previous","6 months previous")
print(forward_spaghetti(ngx_data,date_list,label_list,series_sent="AB-NIT Fixed",title_sent="Alberta NIT",units_sent="USD/MMbtu"))

date_max<-max(ngx_data$trade_date)
date_list<-rev(c(date_max,date_max-years(1),date_max-months(18),date_max-years(2),date_max-years(5),date_max-years(10),date_max-years(15)))
label_list<-c(as.character(date_max),"1 year previous","18 months previous","2 years previous","5 years previous","10 years previous","15 years previous")
print(forward_spaghetti(ngx_data,date_list,label_list,series_sent="AB-NIT Fixed",title_sent="Alberta NIT",units_sent="USD/MMbtu"))
ggsave("test.png",width=8,height=4.5)


date_max<-max(ngx_data$trade_date)
date_list<-rev(c(date_max,date_max-months(1),date_max-months(2),date_max-months(3),date_max-months(6)))
label_list<-c(as.character(date_max),"1 month previous","2 months previous","3 months previous","6 months previous")
print(forward_spaghetti(ngx_data,date_list,label_list,series_sent="Union Dawn",title_sent="Ontario Union Dawn Hub",units_sent="USD/MMbtu"))


#cool price evolution graph
#Dawn is only USD/MMBTu
date_max<-ymd("2018-11-21")
date_list<-rev(c(date_max,date_max-weeks(1),date_max-weeks(2),date_max-weeks(3)))
label_list<-c(as.character(date_max),"1 week previous","2 weeks previous","3 weeks previous")
print(forward_spaghetti(ngx_data,date_list,label_list,series_sent="Union Dawn",title_sent="Ontario Union Dawn Hub",units_sent="USD/MMbtu"))



date_max<-max(ngx_data$trade_date)
date_list<-date_max
label_list<-c(paste("Forward strip, ",as.character(date_max),sep = ""))
print(forward_spaghetti(ngx_data,date_list,label_list,series_sent="AB-NIT Fixed",title_sent="Alberta NIT",units_sent="USD/MMbtu"))



date_max<-max(ngx_data$trade_date)
date_list<-rev(c(date_max-days(1),date_max-years(1),date_max-years(2),date_max-years(4),date_max-years(6),date_max-years(8),date_max-years(10),date_max-years(12),date_max-years(14)))
date_list<-rev(c(date_max,date_max-years(1),date_max-years(2),date_max-years(6),date_max-years(8)))

#label_list<-c(paste("Forward strip, ",as.character(date_max),sep = ""),"1 year previous","2 years previous","4 years previous","6 years previous","8 years previous","10 years previous","12 years previous","14 years previous")
label_list<-rev(paste("Forward strip, ",format(ymd(date_list), "%b %d, %Y"),sep = ""))
png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("NIT_forwards.png")
print(forward_spaghetti(ngx_data,date_list,label_list,series_sent="AB-NIT Fixed",title_sent="Alberta NIT",units_sent="USD/MMbtu"))
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()



print(forward_spaghetti(ngx_data,date_list,label_list,series_sent="AB-NIT Fixed",title_sent="Alberta NIT",units_sent="USD/MMbtu"))



date_max<-max(ngx_data$trade_date)
date_list<-rev(c(date_max,ymd("2008-05-10")))
label_list<-rev(paste("Forward strip, ",format(ymd(date_list), "%b %d, %Y"),sep = ""))
print(forward_spaghetti(ngx_data,date_list,label_list,series_sent="AB-NIT Fixed",title_sent="Alberta NIT",units_sent="USD/MMbtu"))




df_test<-filter(ngx_data,series=="AB-NIT Fixed",price_units=="USD/MMbtu",spot_price>11)




#forward curve for oil sands model

date_max<-ymd("2020-01-29")
fwd_data=subset(ngx_data,(trade_date==date_max) & (series=="AB-NIT Fixed") & (price_units=="CAD/GJ"))

forwards_data<-forwards_data_pull("WTI")[[1]]
forwards_data$Date[1]<-forwards_data$Date[2]-days(1)
df1<-reshape2::melt(forwards_data,id=c("Date"),variable.name = "Instrument")
#operator %m+% in lubridate adds a month without exceeding days in month for target
df1<-df1 %>% mutate(
  Inst_Date=Date%m+%months(as.numeric(gsub("Month ","", as.character(Instrument)))),
  Inst_Date=Inst_Date%m+%months(ifelse(day(Date)>25,1,0)), #add an extra month if we're past calendar day 25
  inst_year=year(Inst_Date),
  Inst_Month=month(Inst_Date),
  Inst_Date=ymd(paste(inst_year,Inst_Month,15,sep = "-")) #set instrument date to the 15th of each month
)
#get cads
cad_df<-cad_forwards()
names(cad_df)[2]<-"0"
cad_df1<-reshape2::melt(cad_df,id=c("Date"),variable.name = "FX_Contract",value.name = "cad_fx")
cad_df1<-na.omit(cad_df1)
cad_df1<-cad_df1 %>% mutate(
  Inst_Date=Date%m+%months(as.numeric(gsub("Month ","", as.character(FX_Contract)))),
  Inst_Date=Inst_Date%m+%months(ifelse(day(Date)>25,1,0)), #add an extra month if we're past calendar day 25
  inst_year=year(Inst_Date),
  Inst_Month=month(Inst_Date),
  Inst_Date=ymd(paste(inst_year,Inst_Month,15,sep = "-")) #set instrument date to the 15th of each month
)
#join cads and commodity data
df_test<-left_join(df1,cad_df1,by=c("Date","Inst_Date","inst_year","Inst_Month")) %>% filter(!is.na(cad_fx))%>%
  group_by(inst_year) %>% summarize(wti=mean(value),cad=mean(1/cad_fx))  


fwd_year<-fwd_data %>% group_by(inst_year) %>% summarize(settle=mean(settle))%>%
  left_join(df_test,by="inst_year")






#Emissions EUAs
#load data
#data<-load_bb_daily()
EUA_data<-data[,c(1,grep("ICE ECX EMISSION",names(data))),]
EUA_data<-EUA_data[,-grep("Current",names(EUA_data))]
EUA_data<-EUA_data[,-grep("EUA Prompt",names(EUA_data))]
EUA_data<-EUA_data[,-grep("Phase 3",names(EUA_data))]
EUA_data <- data.frame(lapply(EUA_data, function(x) {
  gsub("#N/A N/A", "", x)
}))


name_list<- gsub("EUA.", "", names(EUA_data))
name_list<- gsub("ICE.ECX.EMISSION..Dec", "ECX 20", names(EUA_data))
names(EUA_data)<-name_list
EUA_data[,-1] = apply(EUA_data[,-1], 2, function(x) as.numeric(as.character(x)))
EUA_data$Date = as.Date(EUA_data$Date)
EUA_data<-EUA_data[EUA_data$Date>=as.Date("2005-04-22"),]
df1<-reshape2::melt(EUA_data,id=c("Date"),variable.name = "Inst_Year",value.name = "Settle")
df1<-na.omit(df1)
df1$Inst_Year<- gsub("ECX ", "", df1$Inst_Year)

df1<-filter(df1,as.numeric(as.character(Inst_Year))>=as.numeric(year(df1$Date)))

years<-3
lims<-c(max(df1$Date)-years(years),max(df1$Date)+months(1))
Instruments<-c(2015,2017,2019,2021)
df2<-filter(df1,Date>lims[1] & Inst_Year %in% Instruments)
lims_y<-c(min(0,min(df2$Settle,na.rm = T)),max(df2$Settle,na.rm = T)+2)
round(lims_y/2)*2 #round to nearest multiple of 5


ggplot(subset(df1,Inst_Year %in% Instruments)) +
  geom_line(aes(Date,Settle,group=as.factor(Inst_Year),colour=as.factor(Inst_Year)),size=2)+
  #geom_vline(xintercept = as.Date("2017-04-19")+
  scale_color_brewer("EUA Vintage",palette = "Set1")+
  scale_x_date(name=NULL,date_breaks = "3 months", date_labels =  "%b\n%Y",limits=lims,expand=c(0,0)) +
  scale_y_continuous(expand = c(0, 0),limits=lims_y,breaks=c(-5,seq(0,lims_y[2],2))) +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "gray",linetype="dotted"),
        axis.line.x = element_line(color = "gray"),
        axis.line.y = element_line(color = "gray"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        plot.subtitle = element_text(size = 12,hjust=0.5),
        plot.caption = element_text(face="italic",size = 12,hjust=0),
        legend.key.width=unit(2,"line"),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust=0.5,size = 14))+
  labs(y="Settlement Price (Euros/tonne)",x="\nTrade Date",
       title="EUA Settlement Prices",
       caption="Source: Data via Bloomberg\nGraph by Andrew Leach")


today_date<-as.Date("2017-12-31")
today_date<-max(EUA_data$Date)
dates<-c(today_date,today_date-years(1),today_date-years(2),today_date-years(3),today_date-years(4))

png<-1
if(png==1)
  set_png(file="EUA_forwards.png")

ggplot(subset(df1,Date %in% dates)) +
  geom_line(aes(Inst_Year,Settle,colour=as.factor(Date),group=as.factor(Date)),size=2)+
  scale_color_brewer("Trade Date",palette = "Set1")+
  guides(col = guide_legend(ncol = 3))+
  #scale_x_date(name=NULL,date_breaks = "3 months", date_labels =  "%b\n%Y",limits=lims,expand=c(0,0)) +
  scale_x_discrete(expand = c(0,0.1)) +
  scale_y_continuous(expand = c(0, 0),limits=lims_y,breaks=c(-5,seq(0,lims_y[2],2))) +
  theme_classic() +
  theme(panel.border = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "gray",linetype="dotted"),
        axis.line.x = element_line(color = "gray"),
        axis.line.y = element_line(color = "gray"),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(margin = margin(t = 10)),
        axis.title = element_text(size = 12),
        #axis.label.x = element_text(size=20,vjust=+5),
        plot.subtitle = element_text(size = 12,hjust=0.5),
        plot.caption = element_text(face="italic",size = 12,hjust=0),
        legend.key.width=unit(2,"line"),
        legend.position = "bottom",
        #legend.direction = "horizontal",
        #legend.box = "horizontal",
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust=0.5,size = 14))+
  labs(y="Settlement Price (Euros/tonne)",x="\nInstrument Date",
       title="EUA Settlement Prices",
       caption="Source: Data via Bloomberg\nGraph by Andrew Leach")

if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()





level_names<-c("WTI","WCS","Implied Bitumen")
diff_names<-c("WTI","WCS")

names<-c("WTI","WCS")

#two_panel_graph<-function(level_names,diff_names){
top_panel<-levels_chart(data=data,names=level_names,10,"CAD")


bottom_panel<-diffs_area_chart(data=data,names,10,"CAD")
print(bottom_panel)
print(top_panel)



mylegend<-arrangeGrob(g_legend(top_panel),g_legend(bottom_panel), nrow=1)



grid_arrange_shared_legend(top_panel,gridExtra::arrangeGrob(bottom_panel, ncol=1), ncol=1, nrow=2)


png<-1
if(png==1)
  set_png(file=paste("macleans_2019.png",sep=""))
grid.arrange(arrangeGrob(top_panel + theme(legend.position="none",
                                           legend.margin=margin(c(0,0,0,0),unit="cm"),
                                           legend.text = element_text(colour="black", size = 14, face = "bold"),
                                           plot.caption = element_blank(),
                                           plot.title = element_blank(),
                                           plot.subtitle = element_text(size = 14, face = "italic"),
                                           panel.grid.minor = element_blank(),
                                           text = element_text(size = 12,face = "bold"),
                                           axis.text = element_text(size = 12,face = "bold", colour="black"),
                                           axis.text.x = element_blank()
),
bottom_panel +
  #  annotate("rect", fill = "black", alpha = 0.25, 
  #           xmin = ymd("2017-11-17"), xmax =ymd("2017-11-28")+months(1), ymin = -Inf, ymax = Inf)+
  #  annotate("text", x = as.Date("2017-7-1"), y = 50, label = "Keystone\noutage",size=3)+
  #  annotate("rect", fill = "black", alpha = 0.25, 
  #           xmin = ymd("2018-09-18"), xmax =ymd("2018-12-03"), ymin = -Inf, ymax = Inf)+
  #  annotate("text", x = as.Date("2018-5-30"), y = 50, label = "Whiting\nshutdown",size=3)+
  theme(legend.position="none",
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 16, face = "italic"),
        plot.caption = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 12,face = "bold"),
        axis.text = element_text(size = 12,face = "bold", colour="black"),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))),
ncol=1,heights=c(3,1.75)),
mylegend, 
nrow=2,heights=c(10, 1),bottom =text_grob(
  "Source: Data via Bloomberg, graph by Andrew Leach",
  face = "italic", color = "black",size=14,just="center",lineheight = 1
),
top =text_grob(
  "Spot oil prices, implied bitumen values, and Alberta diluted bitumen differentials",
  face = "bold", color = "black",size=14,just="center",lineheight = 1
)

)
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()
#}
#two_panel_graph(level_names,diff_names)



#mid-contenent oil prices

#bipetro_data <- read.csv(file = "bi_petro_illinois_pricing.csv",header = F,stringsAsFactors = F)
#bipetro_data$V1<-mdy(bipetro_data$V1)
#names(bipetro_data)<-c("Date","bi_petro_posted_price","bi_petro_daily_change")
#data<-data %>% left_join(select(bipetro_data,Date,bi_petro_posted_price),by="Date")



#names<-c("Brent","WTI","bi_petro_posted_price")
#test<-levels_chart(data=data,names,12,"USD")
#set_png("global_crude.png")

ctax_graphs<-function(data_sent){
  #ridiculous graphs for a Twitter fight
  df1<-data_sent %>% select(Date,`Reg Gas Retail Edmonton Incl Tax`)%>%
    mutate(ctax=case_when(year(Date) <= 2016 ~ 0,
                          year(Date) == 2017 ~ 4.49/100*158,
                          year(Date) == 2018 ~ 6.73/100*158),
           ctax_rate=ctax/`Reg Gas Retail Edmonton Incl Tax`)%>%
    filter(year(Date)>2017)
  set_png("ctax_rate.png")
  p<-ggplot(df1)+geom_line(aes(Date,ctax_rate*100))+
    scale_colour_manual(NULL,values=colors_ua10())+
    scale_x_date(name=NULL,date_breaks = "2 months", date_labels =  "%b\n%Y",expand=c(0,0)) +
    labs(y="Effective carbon tax rate on gasoline (%)",x="Date",
         title="Carbon taxes as a share of retail gasoline prices",
         caption="Data via Bloomberg, calculations and graph by Andrew Leach")+
    weekly_graphs()
  print(p)
  dev.off()
  
  
  set_png("ctax_real.png")
  p<-ggplot(df1)+geom_line(aes(Date,ctax/158*100))+
    scale_colour_manual(NULL,values=colors_ua10())+
    scale_x_date(name=NULL,date_breaks = "2 months", date_labels =  "%b\n%Y",expand=c(0,0)) +
    labs(y="Carbon tax rate on gasoline (c/l)",x="Date",
         title="Carbon taxes embedded in retail gasoline prices",
         caption="Data via Bloomberg, calculations and graph by Andrew Leach")+
    weekly_graphs()
  print(p)
  dev.off()
}

#ctax_graphs(data)
#diffs and crude by rail:

wti_diff<-data %>% select(date=Date,WTI,WCS,'Implied Bitumen') %>% filter(date>ymd("2010-01-01")) %>%
  clean_names()%>%
  mutate(year=year(date),month=month(date))%>% group_by(year,month)%>%
  summarize(wti=mean(wti),wcs=mean(wcs),bitumen=mean(implied_bitumen))%>%
  mutate(date=ymd(paste(year,month,1,sep="-"))) %>% select(-year,-month)

#crude_by_rail_exports
#https://www.neb-one.gc.ca/nrg/sttstc/crdlndptrlmprdct/stt/cndncrdlxprtsrl-eng.xls
#require(xlsx)
#oil_by_rail <- xlsx::read.xlsx("https://www.neb-one.gc.ca/nrg/sttstc/crdlndptrlmprdct/stt/cndncrdlxprtsrl-eng.xls",sheetName = "CrudeOilExportsByRail")
download.file("https://www.cer-rec.gc.ca/en/data-analysis/energy-commodities/crude-oil-petroleum-products/statistics/canadian-crude-oil-exports-rail-monthly-data.xlsx", destfile="cndncrdlxprtsrl-eng.xlsx", mode = "wb")

oil_by_rail<-read_excel("cndncrdlxprtsrl-eng.xlsx", sheet = NULL, range = NULL, col_names = TRUE,
                        col_types = NULL, na = "", trim_ws = TRUE, skip = 7)
oil_by_rail<-oil_by_rail %>% head(-5)%>%fill(Year)%>% select("Year","Month",5,7) %>% clean_names() %>% 
  mutate(date=ymd(paste(year,month,1,sep="-"))) %>% select(-year,-month)%>%
  left_join(wti_diff)%>%mutate(diff=wti-wcs)


png<-1
cbr_panel<-ggplot(oil_by_rail) +
  #geom_line(aes(Date,`Volume (bbl)`/days_in_month(mth_num)),size=1.75) +
  geom_area(aes(date,volume_bbl_per_day/1000,fill="Crude by rail volume (1000 bbl/d)"),alpha=.9) +
  geom_line(aes(date,diff/.1,colour="WTI-WCS differential ($/bbl), right axis"),size=1.5)+
  
  scale_y_continuous(expand = c(0, 0)) +
  scale_y_continuous(sec.axis = sec_axis(~.*.1, name = "WTI-WCS differential [$/bbl]\n"))+
  
  #geom_point(size=1) +
  scale_color_manual("",values="black")+
  scale_fill_manual("",values=blakes_blue)+
  scale_x_date(name=NULL,date_breaks = "6 months",date_labels =  "%b\n%Y",expand=c(0,0)) +
  
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "gray",linetype="dotted"),
        axis.line.x = element_line(color = "gray"),
        axis.line.y = element_line(color = "gray"),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(margin = margin(t = 10)),
        axis.title = element_text(size = 12),
        #axis.label.x = element_text(size=20,vjust=+5),
        plot.subtitle = element_text(size = 12,hjust=0.5),
        plot.caption = element_text(face="italic",size = 12,hjust=0),
        legend.key.width=unit(2,"line"),
        legend.position = "bottom",
        #legend.direction = "horizontal",
        #legend.box = "horizontal",
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust=0.5,size = 14))+
  labs(y="Export Volumes (1000 bbl/d)",x="Date",
       #title=paste("Canadian Oil Exports by Rail and Crude Pricing Differentials",sep=""),
       #caption="Source: Bloomberg and CER Data, graph by Andrew Leach."
       NULL
       )
cbr_panel
ggsave("cbr_diffs.png",width=16,height=9,dpi=300)


cbr_alone<-ggplot(oil_by_rail) +
  #geom_line(aes(Date,`Volume (bbl)`/days_in_month(mth_num)),size=1.75) +
  geom_col(aes(date,volume_bbl_per_day/1000,fill="Crude by rail volume (1000 bbl/d)"),alpha=.9) +
  #geom_line(aes(date,diff/.1,colour="WTI-WCS differential ($/bbl), right axis"),size=1.5)+
  
  scale_y_continuous(expand = c(0, 0),sec.axis = sec_axis(~ .,name = "Export Volumes (1000 bbl/d)")) +
  #scale_y_continuous()+
  #geom_point(size=1) +
  scale_color_manual("",values="black")+
  scale_fill_manual("",values=blakes_blue)+
  scale_x_date(name=NULL,date_breaks = "6 months",date_labels =  "%b\n%Y",expand=c(0,0)) +
  
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "gray",linetype="dotted"),
        axis.line.x = element_line(color = "gray"),
        axis.line.y = element_line(color = "gray"),
        axis.text = element_text(size = 16),
        axis.text.x = element_text(margin = margin(t = 16)),
        axis.title = element_text(size = 16),
        #axis.label.x = element_text(size=20,vjust=+5),
        plot.subtitle = element_text(size = 16,hjust=0.5),
        plot.caption = element_text(face="italic",size = 12,hjust=0),
        legend.key.width=unit(2,"line"),
        legend.position = "none",
        #legend.direction = "horizontal",
        #legend.box = "horizontal",
        legend.text = element_text(size = 16),
        plot.title = element_text(hjust=0.5,size = 18))+
  labs(y="Export Volumes (1000 bbl/d)",x="Date",
       title=paste("Canadian Oil Exports by Rail (1000 bbl/d)",sep=""),
       caption="Source: CER data, graph by Andrew Leach.",
       NULL
  )
cbr_alone
ggsave("cbr_plot.png",width=16,height=9,dpi=300)












level_names<-c("WTI","WCS","Implied Bitumen")
diff_names<-c("WTI","WCS")

names<-c("WTI","WCS")

#two_panel_graph<-function(level_names,diff_names){
top_panel<-levels_chart(data=data,names=level_names,10,"CAD")


bottom_panel<-diffs_area_chart(data=data,names,10,"CAD")
print(bottom_panel)
print(top_panel)



mylegend<-arrangeGrob(g_legend(top_panel),g_legend(bottom_panel), nrow=1)



grid_arrange_shared_legend(top_panel,gridExtra::arrangeGrob(bottom_panel, ncol=1), ncol=1, nrow=2)


grid.arrange(arrangeGrob(top_panel + theme(legend.position="none",
                                           legend.margin=margin(c(0,0,0,0),unit="cm"),
                                           legend.text = element_text(colour="black", size = 14, face = "bold"),
                                           plot.caption = element_blank(),
                                           plot.title = element_blank(),
                                           plot.subtitle = element_text(size = 14, face = "italic"),
                                           panel.grid.minor = element_blank(),
                                           text = element_text(size = 12,face = "bold"),
                                           axis.text = element_text(size = 12,face = "bold", colour="black"),
                                           axis.text.x = element_blank()
),
bottom_panel +
  #  annotate("rect", fill = "black", alpha = 0.25, 
  #           xmin = ymd("2017-11-17"), xmax =ymd("2017-11-28")+months(1), ymin = -Inf, ymax = Inf)+
  #  annotate("text", x = as.Date("2017-7-1"), y = 50, label = "Keystone\noutage",size=3)+
  #  annotate("rect", fill = "black", alpha = 0.25, 
  #           xmin = ymd("2018-09-18"), xmax =ymd("2018-12-03"), ymin = -Inf, ymax = Inf)+
  #  annotate("text", x = as.Date("2018-5-30"), y = 50, label = "Whiting\nshutdown",size=3)+
  theme(legend.position="none",
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 16, face = "italic"),
        plot.caption = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 12,face = "bold"),
        axis.text = element_text(size = 12,face = "bold", colour="black"),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))),
  cbr_panel
  ,
ncol=1,heights=c(3,1.75,1.75)),
mylegend, 
nrow=2,heights=c(10, 1),bottom =text_grob(
  "Source: Data via Bloomberg, graph by Andrew Leach",
  face = "italic", color = "black",size=14,just="center",lineheight = 1
),
top =text_grob(
  "Spot oil prices, implied bitumen values, and Alberta diluted bitumen differentials",
  face = "bold", color = "black",size=14,just="center",lineheight = 1
)

)





















#Oil sands model output
os_model<-function(){
  
  comdty<-"WTI"
  title_sent<-"WTI Forward Contract"
  single<-0
  relative_labels<-1
  data_sent<-data_retrieve[[1]]
  units_sent<-data_retrieve[[2]]
  lag<-0
  #end testing
  forwards_data<-data_sent
  units<-"$CA/bbl"
  df1<-reshape2::melt(forwards_data,id=c("Date"),variable.name = "Instrument")
  df1<-na.omit(df1)
  #operator %m+% in lubridate adds a month without exceeding days in month for target
  df1<-df1 %>% mutate(
    Inst_Date=Date%m+%months(as.numeric(gsub("Month ","", as.character(Instrument)))),
    Inst_Date=Inst_Date%m+%months(ifelse(day(Date)>25,1,0)), #add an extra month if we're past calendar day 25
    Inst_Year=year(Inst_Date),
    Inst_Month=month(Inst_Date),
    Inst_Date=ymd(paste(Inst_Year,Inst_Month,15,sep = "-")) #set instrument date to the 15th of each month
  )
  date_max<-max(df1$Date)-lag #drop back 1 day if you grabbed mid_day data.
  #get cads
  cad_df<-cad_forwards()
  names(cad_df)[2]<-"0"
  cad_df1<-reshape2::melt(cad_df,id=c("Date"),variable.name = "FX_Contract",value.name = "cad_fx")
  cad_df1<-na.omit(cad_df1)
  cad_df1<-cad_df1 %>% mutate(
    Inst_Date=Date%m+%months(as.numeric(gsub("Month ","", as.character(FX_Contract)))),
    Inst_Date=Inst_Date%m+%months(ifelse(day(Date)>25,1,0)), #add an extra month if we're past calendar day 25
    Inst_Year=year(Inst_Date),
    Inst_Month=month(Inst_Date),
    Inst_Date=ymd(paste(Inst_Year,Inst_Month,15,sep = "-")) #set instrument date to the 15th of each month
  )
  
  
}



load_bb_gasoline<-function()
{ #load BB data
  daily_data <- read.xlsx(xlsxFile = bb_file, sheet = "Gasoline Prices", startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
  cities<-c("Date",daily_data[2,seq(1,ncol(daily_data),by=2)])
  #check here if you have #NA requesting codes in first row
  data<-daily_data[-(1:4),c(1,seq(2,ncol(daily_data),by=2))]
  if(any(grep("#N/A Requesting",data[1,])))
    data<-data[-1,] #take off the first row if you didn't let the refresh finish
  names(data)<-cities
  data$Date<-ymd(data$Date)
  data[1,1]<-data[2,1]-days(1)
  data <- data.frame(lapply(data, function(x) {
    gsub("#N/A N/A", "", x)
  }))
  data[,-1] = apply(data[,-1], 2, function(x) as.numeric(as.character(x)))
  
  #need to get into common currency
  #add currency flag to data and USD/CAD series to sheet
  return(data)
}



add_city<-function(temp_sent,name,country,pop,lat,long){
  temp_data<-temp_sent[1,]
  temp_data$name<-name
  temp_data$country.etc <-country
  temp_data$pop<-pop
  temp_data$lat<- lat
  temp_data$long<- long
  temp_data
}

#EIA gasoline and diesel data is the same as the bloomberg data
#241020

library(maps)

#mapping
#set up the locations
cities<-rbind(canada.cities,us.cities)
oil_cities<-cities[grep("Edmonton",cities$name),]
oil_cities$name[1]<-"Edmonton Mixed Sweet"
#EMSW
oil_cities<-rbind(oil_cities,add_city(oil_cities[1,],"Edmonton Mixed Sweet","light",10000,61,-120))
oil_cities<-oil_cities[-1,]
# SCO Edmonton
oil_cities<-rbind(oil_cities,add_city(oil_cities[1,],"Syncrude Sweet Synthetic","light",10000,61,-108))
# Cushing 35.9851° N, 96.7670° W
oil_cities<-rbind(oil_cities,add_city(oil_cities[1,],"WTI","light",10000,35,-94.7670))
#Hardisty 52.6750° N, 111.3037° W
oil_cities<-rbind(oil_cities,add_city(oil_cities[1,],"WCS","heavy",10000,52.6750,-111.3037))
#Alaska 61.1800 ,-149.19000
oil_cities<-rbind(oil_cities,add_city(oil_cities[1,],"Alaska North Slope","light",10000,64,-145))
#Bakken
oil_cities<-rbind(oil_cities,add_city(oil_cities[1,],"Bakken Clearbrook","light",10000,44,-100.77000))
#WTI Midland 32.0300 -102.10000
oil_cities<-rbind(oil_cities,add_city(oil_cities[1,],"WTI Midland","light",10000, 35,-105))
#Houston 29.7700 -95.39000
oil_cities<-rbind(oil_cities,add_city(oil_cities[1,],"West Texas Sour","light",10000, 27,-105))
#Maya
oil_cities<-rbind(oil_cities,add_city(oil_cities[1,],"Maya","heavy",10000,26,-94))
#LLS
oil_cities<-rbind(oil_cities,add_city(oil_cities[1,],"LLS","light",10000,26,-86))

#Brent

#hibernia is at 46°45.026'N 48°46.976'
oil_cities<-rbind(oil_cities,add_city(oil_cities[1,],"Brent","light",10000,36,-65))

oil_cities<-rbind(oil_cities,add_city(oil_cities[1,],"Arabian Hvy Arab Crude Asia","heavy",10000,50,-145))
oil_cities<-rbind(oil_cities,add_city(oil_cities[1,],"Arabian Light Arab Crude Spot Price Asia","light",10000,40,-145))



map_oil_prices<-data %>% pivot_longer(-Date,values_to = "price",names_to = "commodity") %>% 
  filter(commodity %in% oil_cities$name) %>% group_by(commodity) %>% arrange(Date) %>%
  summarise(price=last(price),Date=max(Date)) %>% left_join(oil_cities,by=c("commodity"="name")) %>%
  mutate(commodity=factor(commodity),
         commodity=fct_recode(commodity, 
                              #"Alaska North Slope"="ANS West Coast Spot Price",
                              "Arabian Heavy, Asia"="Arabian Hvy Arab Crude Asia",
                              "Arabian Light, Asia"="Arabian Light Arab Crude Spot Price Asia",
                              "Bakken Light"="Bakken Clearbrook",
                              "WTI (Midland)"="WTI Midland",
                              "Sweet Synthetic (Edmonton)"="Syncrude Sweet Synthetic",
                              "Mixed Sweet (Edmonton)"="Edmonton Mixed Sweet",
                              "WTI (Cushing)"="WTI",
                              "WCS (Hardisty)"="WCS"),
         label=paste(str_wrap(commodity,10),"\n$",format(round(price, 2), nsmall = 2),sep=""))

library(mapdata)
usa <- map_data("worldHires","usa")
canada <- map_data("worldHires","canada")
mexico <- map_data("worldHires", "Mexico")

cities<-rbind(canada.cities,us.cities)
cities$name[grep("OTTAWA ON",cities$name)]<-"Ottawa ON"

NAmap <- ggplot() + geom_polygon(data = usa, 
                                 aes(x=long, y = lat, group = group), 
                                 fill = "white", 
                                 color="black") +
  geom_polygon(data = canada, aes(x=long, y = lat, group = group), 
               fill = "white", color="black") + 
  geom_polygon(data = mexico, aes(x=long, y = lat, group = group), 
               fill = "white", color="black") +
  coord_fixed(xlim = c(-150, -50),  ylim = c(20, 70), ratio = 1.2)+
  theme_map()+theme(plot.title = element_text(size = 18,face = "bold"),
                    plot.subtitle = element_text(size = 16, face = "italic"))+
  labs(title=paste("Gasoline Prices in North American Cities",sep=""),
       subtitle=paste("Prices in Canadian Cents per Litre",sep=""),
       caption="Source: EIA and Bloomberg Data.")
#NAmap

grob1 <-  str_wrap("Crudes referenced on the map are not necessarily perfect substitutes. For example, Arab heavy crude is lighter (API 27.5 vs 19-22 for Western Canadian Select) and lower sulphur (2.8% vs 3.5% for Western Canadian Select) than the Western Canadian Select blend. Synthetic crude oil and Bakken light are both lighter and lower sulphur than West Texas Intermediate or Brent.",45)
#NAmap 


set_png("crude_map.png")
NAmap+geom_point(data= map_oil_prices, aes(x=long, y=lat,colour=country.etc,),size=0)+
  geom_label(data=map_oil_prices,
             aes(x=long, y=lat,label = label,
                 fill=country.etc),
             fontface = 'bold',
             size = 3.5,
             #box.padding = unit(0, "lines"),
             #point.padding = unit(0, "lines"),
             label.size=1,
             show.legend = F)+
  guides(color= guide_legend(override.aes = list(size = 5)))+
  scale_fill_manual("Crude type",values=colors_ua10()[c(1,3)],labels=c("Heavy Crude","Light Crude"))+
  scale_colour_manual("Crude type",values=colors_ua10()[c(1,3)],labels=c("Heavy Crude","Light Crude"))+
  labs(title=paste("North American Crude Oil Prices",sep=""),
       subtitle=paste("Prices in US Dollars per barrel",sep=""),
       caption=paste("Bloomberg data for ",format(max(map_oil_prices$Date),"%b %d, %Y"),sep=""))+
  annotate("text", x = -75, y = 25, label = grob1,size=2.5,hjust = 0)

dev.off()  


#gas prices map










#mapping
#set up the locations
cities<-rbind(canada.cities,us.cities)
gas_cities<-cities[grep("Edmonton",cities$name),]
# Station 2 55.6977° N, 121.6297° W
gas_cities<-rbind(gas_cities,add_city(gas_cities[1,],"Station 2","Canada",10000,55.6977,-121.6297))
gas_cities<-gas_cities[-1,]
#Henry Hub
gas_cities<-rbind(gas_cities,add_city(gas_cities[1,],"Henry Hub","USA",10000,29.89249643,-92.067833062))

#Algonquin City Gate
gas_cities<-rbind(gas_cities,add_city(gas_cities[1,],"Algonquin Citygate","USA",10000,42.34,-69))

#Chicago City Gate
gas_cities<-rbind(gas_cities,add_city(gas_cities[1,],"Chicago Citygate","USA",10000,41.84,-89))

#Kingsgate
gas_cities<-rbind(gas_cities,add_city(gas_cities[1,],"Kingsgate BC","Canada",10000,49.0060,-122))


#Empress
gas_cities<-rbind(gas_cities,add_city(gas_cities[1,],"Empress","Canada",10000,55.6977,-110.0094))


#PGE 
gas_cities<-rbind(gas_cities,add_city(gas_cities[1,],"PGE Citygate","USA",10000,37.66,-122.42))


#Dawn
gas_cities<-rbind(gas_cities,add_city(gas_cities[1,],"Dawn","Canada",10000,46,-80))

#AECO
gas_cities<-rbind(gas_cities,add_city(gas_cities[1,],"AECO NIT","Canada",10000,51,-111))

#NBP
data<-data %>% mutate(`NBP UK`=`NBP Gas`*`GBP USD`/10)
gas_cities<-rbind(gas_cities,add_city(gas_cities[1,],"NBP UK","UK",10000,41.84,-50))

#Japan LNG
gas_cities<-rbind(gas_cities,add_city(gas_cities[1,],"Japan LNG JCC","Japan",10000,41.84,-140))






coordinates<-function(city){
  paste(cities$lat[grep(city,cities$name)],cities$long[grep(city,cities$name)],sep=",")}



map_gas_prices<-data %>% reshape2::melt(id="Date",value.name = "price",variable.name="commodity") %>% 
  filter(commodity %in% gas_cities$name) %>% group_by(commodity) %>% arrange(Date) %>%
  summarise(price=last(price),Date=max(Date)) %>% left_join(gas_cities,by=c("commodity"="name")) %>%
  mutate(commodity=factor(commodity),
         commodity=fct_recode(commodity, 
                              "Kingsgate"="Kingsgate BC",
                              "Japan LNG"="Japan LNG JCC",
                              "PG&E Citygate"="PGE Citygate",
                              "AECO/NIT"="AECO NIT"),
         label=paste(str_wrap(commodity,10),"\n$",format(round(price, 2), nsmall = 2),sep=""))

set_png("gas_map.png")
NAmap+geom_point(data= map_gas_prices, aes(x=long, y=lat,colour=country.etc,),size=0)+
  geom_label(data=map_gas_prices,
             aes(x=long, y=lat,label = label,
                 fill=country.etc),
             fontface = 'bold',
             color="white",
             size = 3.5,
             #padding = unit(.5, "lines"),
             #point.padding = unit(0, "lines"),
             label.size=1,
             show.legend = F)+
  guides(color= guide_legend(override.aes = list(size = 5)))+
  scale_fill_manual("Location",values=colors_ua10()[c(1,3,4,5)])+
  scale_colour_manual("Location",values=colors_ua10()[c(1,3,4,5)])+
  labs(title=paste("North American and other select natural gas prices",sep=""),
       subtitle=paste("Prices in US Dollars per million cubic feet",sep=""),
       caption=paste("Bloomberg data for ",format(max(map_oil_prices$Date),"%b %d, %Y"),sep=""))
dev.off()  

