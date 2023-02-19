install.packages("choroplethr")
library(choroplethr)
install.packages("choroplethrMaps")
library(choroplethrMaps)
install.packages("openintro")
library(openintro)
library(dplyr)
library(ggplot2)
vehicle <- read.csv(file.choose(),header=T)
car <- as.data.frame(vehicle)
car
#filter
car %>%
  filter(State=='CA' |State=='TX' |State=='FL')
car %>%
  filter(State=='CA',Mileage>1000)
#arrange
car %>%
  filter(State=='CA' |State=='TX' |State=='FL') %>%
  arrange(desc(Mileage))
#summarise here donr for lc i.e. labour cost
car %>%
  summarise(avg_lc=mean(lc),
            sd_lc=sd(lc),
            max_lc=max(lc),
            min_lc=min(lc),
            median_lc=median(lc),
            total=n())
#by above we get summary for labour_cost.
#group by
car %>%
  group_by(State) %>%
  summarise(avg_lc=mean(lc),
            sd_lc=sd(lc),
            max_lc=max(lc),
            min_lc=min(lc),
            median_lc=median(lc),
            total=n()) %>%
  arrange(desc(avg_lc))
#above we group  by state and arrange by descending order of avg_lc.
#mutate -- helps to create new columns with help of calculations.
car %>%
  group_by(State) %>%
  mutate(cph=sum(lc)/sum(lh))%>%
  summarise(avg_cph=mean(cph),
            avg_mileage=mean(Mileage)) %>%
  arrange(desc(avg_cph))
#maximum mileage in state "MA".
car %>%
  filter(State=='MA') %>%
  summarise(max_mileage=max(Mileage))
#which state has the highest sd_lc i.e. standard deviation in labour cost.
car %>%
  group_by(State) %>%
  summarise(sd_lc=sd(lc),
           maxsd=max(sd_lc)) %>%
  arrange(desc(maxsd))
#Histogram
car %>%
  filter(State=='CA' |State=='TX' |State=='FL') %>%
  ggplot(aes(x=lc))+geom_histogram(alpha=0.5)+
  ggtitle("Labour Cost In 3 States")
car %>%
  filter(State=='CA' |State=='TX' |State=='FL') %>%
  ggplot(aes(x=lc,fill=State))+geom_histogram(alpha=0.5,color='darkblue')+
  ggtitle("Labour Cost In 3 States")
#using facet_wrap
#separates histograms into diff ones based on the
#attribute or item provided.
car %>%
  filter(State=='CA' |State=='TX' |State=='FL') %>%
  ggplot(aes(x=lc,fill=State))+geom_histogram(alpha=0.5,color='darkblue')+
  ggtitle("Labour Cost In 3 States")+
  facet_wrap(~State)
#Density
car %>%
  filter(State=='CA' |State=='TX' |State=='FL') %>%
  ggplot(aes(x=lc,fill=State))+geom_density(alpha=0.5,color='darkblue')+
  ggtitle("Labour Cost In 3 States")
#scatterplot
car %>%
  filter(State=='CA' |State=='TX' |State=='FL') %>%
  ggplot(aes(x=lh,y=lc))+geom_point(alpha=0.5,color='darkblue')
car %>%
  filter(State=='CA' |State=='TX' |State=='FL') %>%
  ggplot(aes(x=lh,y=lc))+geom_point(alpha=0.5,color='darkblue')+
  geom_smooth()
car %>%
  filter(State=='CA' |State=='TX' |State=='FL') %>%
  ggplot(aes(x=lh,y=lc))+geom_point(alpha=0.5,color='darkblue')+
  geom_smooth(se=0)
#se=0 to remove error range for line.
car %>%
  filter(State=='CA' |State=='TX' |State=='FL') %>%
  ggplot(aes(x=lh,y=lc,col=State))+geom_point(alpha=0.5,color='darkblue')+
  geom_smooth(se=0)
#changing size of dots below.
car %>%
  filter(State=='CA' |State=='TX' |State=='FL') %>%
  ggplot(aes(x=lh,y=lc,col=State,size=mc))+geom_point(alpha=0.5,color='darkblue')+
  geom_smooth(se=0)
#using facet_wrap
car %>%
  filter(State=='CA' |State=='TX' |State=='FL') %>%
  ggplot(aes(x=lh,y=lc,col=State,size=mc))+geom_point(alpha=0.5,color='darkblue')+
  geom_smooth(se=0)+facet_wrap(~State)
#bar plot
new <- car %>%
  group_by(State) %>%
  mutate(cph=sum(lc)/sum(lh))%>%
  summarise(avg_cph=mean(cph),
            avg_mileage=mean(Mileage)) %>%
  arrange(desc(avg_cph))
ggplot(new,aes(x=State,y=avg_cph))+
  geom_col()
#flipping
new <- car %>%
  group_by(State) %>%
  mutate(cph=sum(lc)/sum(lh))%>%
  summarise(avg_cph=mean(cph),
            avg_mileage=mean(Mileage)) %>%
  arrange(desc(avg_cph))
ggplot(new,aes(x=State,y=avg_cph))+
  geom_col()+
  coord_flip()+
  ggtitle("Cost Per Hour In 50 States")
#filling in color by State
new <- car %>%
  group_by(State) %>%
  mutate(cph=sum(lc)/sum(lh))%>%
  summarise(avg_cph=mean(cph),
            avg_mileage=mean(Mileage)) %>%
  arrange(desc(avg_cph))
ggplot(new,aes(x=State,y=avg_cph,fill=State))+
  geom_col()+
  coord_flip()+
  ggtitle("Cost Per Hour In 50 States")
#box plot
#n() represents total or counting.
car %>%
  group_by(State) %>%
  filter(n()>50)%>%
  ggplot(aes(x=State,y=Mileage,col=State)) +
  geom_boxplot()
#n() above represents counting for car failures 
#according to dataset and filter only those
#where its value > 50.
#Map
new <- car %>%
  group_by(State) %>%
  summarise(total=n(),
            avg_mileage=mean(Mileage))
new
#renaming column names in above dataset
colnames(new) <- c('region','value','mileage')
new
#BELOW USING OPENINTRO LIBRARY WE CONVERT
#ABBREVIATION OF NAMES OF PLACES TO CORRESPONDING
#NAMES.
new$region <-abbr2state(new$region)
new$region <-tolower(new$region)
#we see the 1st row is NA so we remove it.
new <- new[-1,]
#-1 to remove 1st row and , and nothing after it
#for all columns.
new
state_choropleth(new,
                 title="Car Failures In US",
                 legend='No. Of Car Failures')