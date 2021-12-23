# Authors:  Evan Carson, University of Cincinnati
#****************************************************
#*set up R
rm(list=ls())
pacman::p_load(nnet,mgcv,quantreg,systemfit,AER, car, gmodels, haven, jtools, 
               pastecs, plm, psych, stargazer,summarytools,tidyverse, rdrobust,
               openxlsx,rio,skedastic,xlsx)
setwd("~/Desktop/data_comp")
options("scipen"=999,digits=2)

total <- read_csv("incdist.csv")
hist(total, breaks= 10)
str(total)
head(total$inc)
total$inc <- total$...1
hist(total$Total)
count <- total$Total
barplot(count)
counts <- table(total$Total)
counts <- data.frame(Income = total$dist, Frequency = total$Total)
View(counts)
barplot(counts$Frequency)
barplot(counts$Frequency, main="Income Distribution",
        names.arg=counts$Income)
counts$Income

library(ggthemes)
count1 <- counts
count1$Income <- factor(counts$Income,
                     levels = counts$Income)
factor(data1$x,                                    
       levels = c("B", "D", "E", "C", "A"))
View(counts)
barplot(counts$Frequency,names.arg = counts$inc)


View(count1)
counts$Income
counts$inc <- c("less than $10,000","$10,000","$15,000",
                "$25,000","$35,000","$50,000",
                "$75,000","$100,000","$150,000", "$200,000 or greater")
count1$inc <- factor(counts$inc,
                     levels = counts$inc)


ggplot(count1, aes(x=inc, y=Frequency)) + 
  geom_bar(stat = "identity")+
  theme_economist() +
  scale_colour_economist()+
  xlab("Income")+
  ylab("")+
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        title=element_text(size=35),
        axis.title=element_text(size=30),
        axis.text=element_text(size=30))+
  labs(title = "Household Income Distribution in Cincinnati",
       subtitle = "Hamilton, Butler, Warren, Kenton, and Boone Counties",
       caption = "source: United States Census Bureau") 
  

You can change axis text and label size with arguments axis.text= and axis.title= in function theme(). If you need, for example, change only x axis title size, then use axis.title.x=.

g+theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

med <- read_csv("med.csv")
str(med)
ggplot(data,mapping=aes(x=DATE,y=unemprate, group=level,color=level))+
  scale_color_discrete(name="level",labels=c("cincinnati","fed"))+
  geom_line()+
  theme_economist() +
  scale_colour_economist(name="level",labels=c("Cincinnati","U.S."))+
  labs(col="level")+
  xlab("Year")+
  ylab("Unemployment Rate")+
  labs(title = "Cincinnati vs Federal Unemployment",
       caption = "source: FRED")
  
  
str()
cin <- read_csv("cincinnati_unemployment.csv")
str(cin)
View(cin)

unemp <- read_csv("federal_unemployment.csv")
cin$fed <- unemp$UNRATE
unemployment <- gather(
  cin,
  key = "unemployment",
  value = "value",

)
ggplot(house1,mapping=aes(x=DATE,y=medianlist, group=level,color=level))+
  scale_color_discrete(name="level",labels=c("cincinnati","fed"))+
  geom_line()+
  theme_economist() +
  scale_colour_economist(name="level",labels=c("Cincinnati","U.S."))+
  labs(col="level")+
  xlab("Year")+
  ylab("Median List Price $")+
  labs(title = "Cincinnati vs Federal Median Housing Price",
       caption = "source: FRED, Realtor.com")


data <- gather(cin, key="level", value="unemprate", 2:3)
View(data)

cinhouse <- read_csv("medlistcin.csv")
ushouse <- read_csv("medlistus.csv")
cinhouse$MEDLISPRI17140
house <- rename(cinhouse,Cincinnati = MEDLISPRI17140)
View(house)

house1 <- gather(house, key="level",value = "medianlist",2:3)
View(house1)

cinhouse$US <- ushouse$MEDLISPRIUS
ushouse$MEDLISPRIUS

wagefed <- read_csv("hourlywagefed.csv")
wagecin <- read_csv("hourlywagecin.csv")
wagefed <- rename(wagefed,US = CES0500000003)
wagefed$cin <- wagecin$SMU39171400500000003SA
view(wagefed)
wage <- gather(wagefed,key="level",value="avgwage",2:3)

ggplot(wage,mapping=aes(x=DATE,y=avgwage, group=level,color=level))+
  scale_color_discrete()+
  geom_line()+
  theme_economist() +
  scale_colour_economist(name="",labels=c("Cincinnati","U.S."))+
  labs(col="level")+
  xlab("Year")+
  ylab("Median Hourly Wage")+
  labs(title = "Cincinnati vs Federal Average Hourly Wage",
       subtitle="Dollars per Hour, Seasonally Adjusted",
       caption = "source: FRED, BLS")




educ <- read_csv("education.csv")
educ2 <- educ
educ1$education <- factor(educ$`education level`,
                     levels = educ$`education level`)

ggplot(educ1, aes(x=education, y=frequency)) + 
  geom_bar(stat = "identity")+
  theme_economist() +
  scale_colour_economist()+
  xlab("")+
  ylab("")+
  theme(legend.position = "none")+
  labs(title = "Educational Attainment in Cincinnati",
       caption = "source: United States Census Bureau") 

educ2 <- rbind(educ, "No College" = c(68549))
educ2
               