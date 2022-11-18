gen_mix <- read.csv(paste(nrg_folder,"AESO_Gen_Mix.csv",sep="/"),skip = 2,header = TRUE, stringsAsFactors=FALSE)
names(gen_mix)<-c("date","Coal","Hydro","Trade","Natural Gas","Other (incl. biomass)","Solar","Wind")
gen_mix<-gen_mix%>%mutate(Trade=-1*Trade)
gen_mix<-gen_mix%>%pivot_longer(-date,names_to="Source",values_to="gen")%>%
  mutate(date=dmy_hms(date),
         #change factor order to sort by avg gen
         Source=factor(Source, levels=c("Wind","Solar","Hydro","Other (incl. biomass)","Natural Gas","Coal","Trade")))

gen_mix_plot_short<-ggplot(gen_mix%>%filter(date(date)==today()),aes(date,gen, group=Source, fill = Source)) +
  geom_area(position="stack")+
  weekly_small()+
  scale_x_datetime(expand = c(0,0),date_breaks="1 hour",date_labels = "%H")+
  #scale_y_continuous(limits=c(0,5990),expand = c(0,0))+
  scale_fill_manual("",values=c(colors_ua10()[1],colors_ua10()[2],colors_ua10()[4],colors_ua10()[3],colors_ua10()[8],colors_ua10()[5],colors_ua10()[6]))+
  guides(fill = guide_legend(nrow = 1, keywidth = 1.25))+
  #theme(legend.position = "none")+
  labs(x="Hour Ending",y="Average Hourly Supply (MW)",
       subtitle = "Alberta Electricity Supply Mix, November 2, 2022",
       title="No coal for a few minutes",
       caption="Source: AESO Data, accessed via NRGStream")
gen_mix_plot_short

ggsave("no_coal.png",height=9,width = 14,dpi=300,bg="white")