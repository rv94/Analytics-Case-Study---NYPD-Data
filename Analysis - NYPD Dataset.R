##Set Working Directory, change to required folder if need be
setwd("C:/Users/vigne/Downloads")

##Load Useful Libraries
library("tidyverse")
library("ggplot2")
library("readxl")
library("lubridate")
library("ggbeeswarm")
##Read in dataset to dataframe
DF <- read_excel("NYPD Stop&Frisk Dataset.xlsx", sheet = 1)

##Examine structure of dataset and accordingly convert variables to appropriate data types if need be, and boolean conversions
str(DF)
DF$haircolr <- as.factor(DF$haircolr)
DF$eyecolor <- as.factor(DF$eyecolor)
DF$build <- as.factor(DF$build)
DF$sex <- as.factor(DF$sex)
DF$crimsusp <- as.factor(DF$crimsusp)
DF$age <- as.numeric(as.character(DF$age))
DF$city <- as.factor(DF$city)
DF[DF=="Y"] <- 1
DF[DF=="N"] <- 0

##Select columns to be converted to numeric data and change them accordingly
changecols <- 5:28
DF[,changecols] = as.numeric(unlist(DF[,changecols])) 

##Extract Month through floor division of date vector
DF <- DF %>% mutate(Month = datestop%/%1000000)
DF$Month <- as.factor(DF$Month)

##Display months by descending order of stop frequency and store in Q1A
DF %>%
  group_by(Month) %>% 
  summarise(NumberofStops = n()) %>% 
  arrange(desc(NumberofStops))
Q1A <-  DF %>%
          group_by(Month) %>% 
          summarise(NumberofStops = n()) %>% 
          arrange(desc(NumberofStops))
write.csv(Q1A, "Q1A.csv")

##Display Frequencies of stops, save results in Q1B
DF %>%
  select(starts_with("cs")) %>%
  na.omit() %>% 
  summarise_all(sum)
Q1B <- DF %>%
        select(starts_with("cs")) %>%
        na.omit() %>% 
        summarise_all(sum)
write.csv(Q1B, "Q1B.csv")

##Filter for searched subjects and then display success rate of precincts finding contraband through search
DF %>% 
  filter(searched==1) %>% 
  group_by(pct) %>% 
  summarise(ContrabandSuccessRate = sum(contrabn)/n(), Count = n()) %>% 
  arrange(desc(ContrabandSuccessRate)) %>% 
  slice(1:5) %>% 
  ggplot(aes(x=reorder(as.factor(as.character(pct)),-ContrabandSuccessRate), y = ContrabandSuccessRate)) +
    geom_bar(stat = "Identity") +
    xlab("Precinct") +
    ylab("Success rate of finding contraband through a search") +
    ggtitle("5 most successful precincts in terms of rate of finding contraband through a search")

##Same, but for more frequent instances (n>75)
DF %>% 
  filter(searched==1) %>% 
  group_by(pct) %>% 
  summarise(ContrabandSuccessRate = sum(contrabn)/n(), Count = n()) %>%
  filter(Count>75) %>% 
  arrange(desc(ContrabandSuccessRate)) %>% 
  slice(1:5) %>% 
  ggplot(aes(x=reorder(as.factor(as.character(pct)),-ContrabandSuccessRate), y = ContrabandSuccessRate)) +
    geom_bar(stat = "Identity") +
    xlab("Precinct") +
    ylab("Success rate of finding contraband through a search") +
    ggtitle("5 most successful precincts in terms of rate of finding contraband through a search, n>75")

##Select DF2 by filtering for frisks and relevant columns. AvgFrisk stores average number of reasons for a frisk
DF2 <- DF %>% 
          filter(frisked==1) %>% 
          select(starts_with("rf")) 
DF2$FriskReasons <- rowSums(DF2, na.rm = T)
AvgFrisk <- sum(DF2$FriskReasons)/nrow(DF2) ##1.52916
ggplot(DF2, aes(x = FriskReasons)) + 
  geom_histogram() +
  scale_x_continuous(name = "Number of Reasons for Frisk", breaks=seq(0,9,1)) +
  scale_y_continuous(name = "Number of instances", limits = c(0,6000)) +
  ggtitle("Distribution of number of reasons behind frisking")

##Outlier Precinct Identification 
DF %>% 
  group_by(city, pct) %>% 
  summarise(ArrestRate = sum(arstmade)/n(), Count = n()) %>% 
  na.omit() %>% 
  ggplot(aes(x=1, y = ArrestRate, col = ifelse(ArrestRate>0.35,"Black","Red")), label = pct) +
    geom_point() +
    geom_text(aes(label = ifelse(ArrestRate>0.35,pct,'')), hjust = 2) +
    xlab("Precinct") +
    ylab("Arrest rate at Precinct") +
    scale_x_continuous(limits=c(1,1)) +
    facet_grid(.~city) +
    theme(legend.position = "None",
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())

D40 <- DF %>% 
          filter(pct==40)
D42 <- DF %>% 
          filter(pct==42)
D60 <- DF %>% 
          filter(pct==60)
D110 <- DF %>% 
          filter(pct==110)



View(D40 %>% 
  select(starts_with('rf'), starts_with('cs')) %>% 
  summarise_all(sum)) ##Drugs

View(D110 %>% 
       select(starts_with('rf'), starts_with('cs')) %>% 
       summarise_all(sum)) ##Violent Crime

View(D42 %>% 
       select(starts_with('rf'), starts_with('cs')) %>% 
       summarise_all(sum)) ##Weapons

View(D60 %>% 
       select(starts_with('rf'), starts_with('cs')) %>% 
       summarise_all(sum)) ##Casing
