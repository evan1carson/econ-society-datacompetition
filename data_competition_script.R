# Authors:  Evan Carson, University of Cincinnati
#****************************************************
#*set up R
rm(list=ls())
pacman::p_load(nnet,mgcv,quantreg,systemfit,AER, car, gmodels, haven, jtools, 
               pastecs, plm, psych, stargazer,summarytools,tidyverse, rdrobust,
               openxlsx,rio,skedastic,ggthemes,readr)
setwd("~/Desktop/data_comp")
options("scipen"=999,digits=2)

total <- read_csv("incdist.csv") #read in income data from Census

#*create data frame with incomes and their frequencies, 
#*removing unnecessary bottom three rows
counts <- data.frame(Income = total$dist[1:10], Frequency = total$Total[1:10])
counts <- counts %>% mutate(Income=factor(counts$Income, #make "Income" a factor to preserve order in histogram
                        levels = counts$Income))

#Income Distribution Histogram
ggplot(counts, aes(x=Income, y=Frequency)) + 
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

#*######################
#*Median Income
#*######################
med <- read_csv("med.csv")
med$Income <- parse_number(med$Income) #convert Income to numeric

#Median Income Histogram
ggplot(med,mapping=aes(x=Date,y=Income, group=Place,color=Place))+
  geom_line()+
  theme_economist() +
  scale_colour_economist(name="level",labels=c("Cincinnati","Ohio","U.S."))+
  xlab("Year")+
  ylab("Median Income")+
  labs(title = "Median Real Income: US, Ohio, and Cincinnati",
       caption = "source: FRED")

#*######################
#*Median Hourly Wage
#*######################
wagefed <- read_csv("hourlywagefed.csv")
wagecin <- read_csv("hourlywagecin.csv")
wagefed <- rename(wagefed,US = CES0500000003)
wagefed$cin <- wagecin$SMU39171400500000003SA
view(wagefed)
wage <- gather(wagefed,key="level",value="avgwage",2:3)

ggplot(wage,mapping=aes(x=DATE,y=avgwage, group=level,color=level))+
  geom_line()+
  theme_economist() +
  scale_colour_economist(name="",labels=c("Cincinnati","U.S."))+
  labs(col="level")+
  xlab("Year")+
  ylab("Median Hourly Wage")+
  labs(title = "Cincinnati vs Federal Average Hourly Wage",
       subtitle="Dollars per Hour, Seasonally Adjusted",
       caption = "source: FRED, BLS")


#*######################
#*Median Hourly Wage
#*######################
cin <- read_csv("cincinnati_unemployment.csv")
unemp <- read_csv("federal_unemployment.csv")
cin$fed <- unemp$UNRATE
cin <- rename(cin,cincinnati= CINC139URN)
unemployment <- pivot_longer(cin,2:3, names_to = "level", values_to = "unemprate")

ggplot(unemployment,mapping=aes(x=DATE,y=unemprate, group=level,color=level))+
  geom_line()+
  theme_economist() +
  scale_colour_economist(name="level",labels=c("Cincinnati","U.S."))+
  labs(col="level")+
  xlab("Year")+
  ylab("Unemployment Rate")+
  labs(title = "Cincinnati vs Federal Unemployment",
       caption = "source: FRED")
  
#*######################
#*Median House List Price
#*######################

cinhouse <- read_csv("medlistcin.csv")
ushouse <- read_csv("medlistus.csv")
cinhouse$us <- ushouse$MEDLISPRIUS
house<- rename(cinhouse,cincinnati = MEDLISPRI17140)
house1 <- gather(house, key="level",value = "medianlist",2:3)
ggplot(house1,mapping=aes(x=DATE,y=medianlist, group=level,color=level))+
  scale_color_discrete(name="level",labels=c("cincinnati","fed"))+
  geom_line()+
  theme_economist() +
  scale_colour_economist(name="level",labels=c("Cincinnati","U.S."))+
  labs(col="level")+
  xlab("Year")+
  ylab("Median List Price $")+
  theme(title=element_text(size=30),
        legend..size = unit(4,'cm'),
        axis.title=element_text(size=30),
        axis.text=element_text(size=30))+
  labs(title = "Cincinnati vs Federal Median Housing Price",
       caption = "source: FRED, Realtor.com")

#*######################
#*Educational Attainment
#*######################
educ <- read_csv("education.csv")
educ1 <- educ
educ1$education <- factor(educ$`education level`,
                     levels = educ$`education level`)

ggplot(educ1, aes(x=education, y=frequency)) + 
  geom_bar(stat = "identity")+
  theme_economist() +
  scale_colour_economist()+
  xlab("")+
  ylab("Frequency")+
  theme(title=element_text(size=30),
        axis.title=element_text(size=30),
        axis.text=element_text(size=30))+
  theme(legend.position = "none")+
  labs(title = "Educational Attainment in Cincinnati",
       caption = "source: United States Census Bureau") 

educ2 <- educ1 %>% add_row(education = "No Degree", frequency = c(168655))
educ2 <- educ2[-(1:3),]    

ggplot(educ2, aes(x=education, y=frequency)) + 
  geom_bar(stat = "identity")+
  theme_economist() +
  scale_colour_economist()+
  xlab("")+
  ylab("Frequency")+
  theme(title=element_text(size=30),
        axis.title=element_text(size=30),
        axis.text=element_text(size=30))+
  #theme(legend.position = "none")+
  labs(title = "Educational Attainment in Cincinnati",
       caption = "source: United States Census Bureau")
                           