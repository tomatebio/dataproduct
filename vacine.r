library(stringr)
library(reshape2)
library(ggplot2)
library(dplyr)
library(nominatim)
vacine<-read.csv("vacine.csv")

vacineClear<-vacine[,c(1:13)]

head(vacineClear)

vacineClear$Outbreak<- gsub(".*[Ff]lu*","flu",vacineClear$Outbreak) 
vacineClear$Outbreak<-as.factor(vacineClear$Outbreak)
vacineClear$Long<-as.numeric(as.vector(vacineClear$Long))



tmp_date<-str_split(vacineClear$Date,"-")

vacineClear$Country<-str_trim(vacineClear$Location.1.1)
extractDate<-function(x){
     return(x[1])
}

tmp_date2<-sapply(tmp_date,extractDate)

tmp_date2[grep("\\.",tmp_date2)]<-as.numeric(tmp_date2[grep("\\.",tmp_date2)])
tmp_date2
# imputacao de data
tmp_date2[grep("/",tmp_date2,invert=TRUE)]<-paste0("1/",tmp_date2[grep("/",tmp_date2,invert=TRUE)])

vacineClear$Date<-as.Date(paste0("1/",tmp_date2), format="%d/%m/%Y")

vacineClear$Year<-as.numeric(format(vacineClear$Date,"%Y"))

head(vacineClear)

vacine_melt<-melt(vacineClear,id.vars = c("Outbreak",
                                          "Year",
                                          "Date",
                                          "Country",
                                          "Continent")
                  , measure.vars = c("Cases","Fatalities"))

vacine_tbl<-tbl_df(vacineClear) 

# tabela cases
 
vacine_tbl %>%
  group_by(Outbreak) %>%
  summarise(Cases=sum(Cases)) 

vacine_tbl %>%
  group_by(Outbreak) %>%
  summarise(Fatalities=sum(Fatalities)) 

vacine_tbl %>%
  group_by(Country) %>%
  summarise(total=sum(Cases),Lat=mean(Lat),lg=mean(Long))







vacine_tbl %>%
  group_by(Country=="Afghanistan") %>%
  select(Long) %>%
  as.numeric()





ggplot(vacine_tbl) + stat_summary(aes(x=Category,y=Cases),fun.y=sum,geom="bar", 
                          fill="lightblue",col="grey50")


ggplot(vacine_tbl) + stat_summary(aes(x=Country,y=Cases),fun.y=sum,geom="bar", 
                                  fill="lightblue",col="grey50")

ggplot(vacine_tbl) + stat_summary(aes(x=Category,y=Fatalities),fun.y=sum,geom="bar", 
                                  fill="lightblue",col="grey50")
ggplot(vacine_tbl,aes(x=Date,color=Outbreak)) + geom_line()+stat_count(sum(Cases))





pum<-levels(vacineClear$Outbreak)[22]

sum(vacineClear[vacineClear$Outbreak==pum & vacineClear$Year==2013,"Cases"])


yrs<-sort(unique(vacineClear$Year))
total<-data.frame(t(rep(NA,12)))
colnames(total)<-c("disease",yrs)
for(i in levels(vacineClear$Outbreak)){
     tmp=i
  for(j in yrs){
    tmp<-c(tmp,(sum(vacineClear[vacineClear$Outbreak==i & vacineClear$Year==j,"Cases"])))
    
  }
    total<-rbind(total,tmp)
}

total<-total[-1,]


write.table(total,"cases.txt")

totalg<-melt(total,id.vars = "disease")

totalg$value<-as.numeric(totalg$value)



ggplot(totalg, aes(x = variable, y = value, group = disease, color=disease)) + 
  geom_line() #+ 
  ylab(label="Number of new members") + 
  xlab("Week Number") + 
  scale_colour_manual(values=c("grey", "blue"))

