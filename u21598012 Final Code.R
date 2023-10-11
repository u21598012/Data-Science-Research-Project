library(readr)
library(sqldf)
library(lubridate)
library(RH2)
library(dplyr)
library(tidyr)
library(gapminder)
library(stringr)
library(ggplot2)

sac <- read_csv("SouthAfricaCrimeStats_v2.csv")
pp <- read_csv("ProvincePopulation.csv")

colnames(sac)[4] <- "Five"
colnames(sac)[5] <- "Six"
colnames(sac)[6] <- "Seven"
colnames(sac)[7] <- "Eight"
colnames(sac)[8] <- "Nine"
colnames(sac)[9] <- "Ten"
colnames(sac)[10] <- "Eleven"
colnames(sac)[11] <- "Twelve"
colnames(sac)[12] <- "Thirteen"
colnames(sac)[13] <- "Fourteen"
colnames(sac)[14] <- "Fifteen"

c1 <- sqldf("SELECT Category,COUNT(*) AS num, SUM(Five + Six + Seven + Eight + Nine + Ten + Eleven + Twelve + Thirteen + Fourteen + Fifteen) AS TotalOfYears
             FROM sac
             GROUP BY Category
             ORDER BY TotalOfYears DESC
            ")
c2 <- ggplot(c1, aes(y=TotalOfYears,x = Category)) + geom_bar(stat='identity',aes(fill = factor(Category)))+ 
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),legend.position = c(0.8, 0.7),legend.direction = "vertical")+ 
  labs(fill = "Crime Category")+ ggtitle("Bar graph representing the total number of crimes commited per category between 2005 and 2015 ")+ 
  scale_y_continuous(labels = scales::comma)+ylab("Total Number of Crimes")

c3 <- sqldf("SELECT Province,Category,COUNT(*) AS num, SUM(Five + Six + Seven + Eight + Nine + Ten + Eleven + Twelve + Thirteen + Fourteen + Fifteen) AS TotalOfYears
             FROM sac
             GROUP BY Province, Category
            ")
ci <- sqldf("SELECT Province,Category,COUNT(*) AS num, SUM(Five + Six + Seven + Eight + Nine + Ten + Eleven + Twelve + Thirteen + Fourteen + Fifteen) AS TotalOfYears
             FROM sac
             GROUP BY Province, Category
             ORDER BY Province, TotalOfYears
            ")
c4 <- ggplot(c3, aes(y=TotalOfYears,x = Category)) + geom_bar(stat='identity',aes(fill = factor(Category)))+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+ labs(fill = "Crime Category")+ 
  ggtitle("Bar graph representing the total number of Crimes Commited for each Province between 2005 and 2015 ")+ 
  scale_y_continuous(labels = scales::comma)+ylab("Total Number of Crimes")


c5 <- sqldf("SELECT Category,Province, AVG(Five + Six + Seven + Eight + Nine + Ten + Eleven + Twelve + Thirteen + Fourteen + Fifteen) AS Average,
             Five, Six, Seven, Eight, Nine, Ten, Eleven, Twelve, Thirteen, Fourteen, Fifteen
             FROM sac
             GROUP BY Province, Category, Five, Six, Seven, Eight, Nine, Ten, Eleven, Twelve, Thirteen, Fourteen, Fifteen
            ")
c6 <- ggplot(c5, aes(y=Average, x=(Five + Six + Seven + Eight + Nine + Ten + Eleven+Twelve+ Thirteen+ Fourteen+ Fifteen))) + geom_line(stat='identity')+ facet_wrap(~Province)

c7 <- ggplot(c8, aes(y=Average, x=Province)) + geom_line(stat='identity')+ facet_wrap(~Category)

c8 <- sac %>%
  pivot_longer(Five:Fifteen, names_to = "question", values_to = "response")
c10 <- sqldf("SELECT Province,SUM(Five + Six + Seven + Eight + Nine + Ten + Eleven + Twelve + Thirteen + Fourteen + Fifteen) As TotalP
              FROM sac
              GROUP BY Province
            ")

c11 <- sqldf("SELECT pp.Province, pp.Population, (TotalP/pp.Population)*100000 AS Crime_Rate_Per_100000
              FROM pp
              INNER JOIN c10
              ON pp.Province = c10.Province
             
             ")
c12 <- ggplot(c11, aes(x = Province, y=Crime_Rate_Per_100000))+geom_bar(stat='identity')+ geom_bar(stat='identity',aes(fill = factor(Province)))+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+ labs(fill = "Crime Category")+ ggtitle("Bar graph representing the total number of Crimes Commited for each Province between 2005 and 2015 ")+ 
  scale_y_continuous(labels = scales::comma)+ylab("Crime Rate Per 100000")
c13 <- sqldf("SELECT Province,SUM(Five) As `2005`
              FROM sac
              GROUP BY Province
            ")
c14 <- sqldf("SELECT Province,SUM(Six) As `2006`
              FROM sac
              GROUP BY Province
            ")
c15 <- sqldf("SELECT Province,SUM(Seven) As `2007`
              FROM sac
              GROUP BY Province
            ")
c16 <- sqldf("SELECT Province,SUM(Eight) As `2008`
              FROM sac
              GROUP BY Province
            ")
c17 <- sqldf("SELECT Province,SUM(Nine) As `2009`
              FROM sac
              GROUP BY Province
            ")
c18 <- sqldf("SELECT Province,SUM(Ten) As `2010`
              FROM sac
              GROUP BY Province
            ")
c19 <- sqldf("SELECT Province,SUM(Eleven) As `2011`
              FROM sac
              GROUP BY Province
            ")
c20 <- sqldf("SELECT Province,SUM(Twelve) As `2012`
              FROM sac
              GROUP BY Province
            ")
c21 <- sqldf("SELECT Province,SUM(Thirteen) As `2013`
              FROM sac
              GROUP BY Province
            ")
c22 <- sqldf("SELECT Province,SUM(Fourteen) As `2014`
              FROM sac
              GROUP BY Province
            ")
c23 <- sqldf("SELECT Province,SUM(Fifteen) As `2015`
              FROM sac
              GROUP BY Province
            ")
c24 <- sqldf("SELECT pp.PROVINCE, `2005`, `2006`, `2007` 
              FROM pp
              INNER JOIN c13
              ON pp.Province = c13.Province
              INNER JOIN c14
              ON c13.Province = c14.province
              INNER JOIN c15
              ON c14.Province = c15.province
              
            ")

c25 <- sqldf("SELECT c16.Province, `2008`, `2009`, `2010`, `2011`, `2012`, `2013`, `2014`, `2015`
              FROM c16
              INNER JOIN c17
              ON c16.Province = c17.province
              INNER JOIN c18
              ON c17.Province = c18.province
              INNER JOIN c19
              ON c18.Province = c19.province
              INNER JOIN c20
              ON c19.Province = c20.province
              INNER JOIN c21
              ON c20.Province = c21.province
              INNER JOIN c22  
              ON c21.Province = c22.province
              INNER JOIN c23
              ON c22.Province = c23.province
             ")
c26 <- sqldf("SELECT c24.PROVINCE, `2005`, `2006`, `2007`, `2008`, `2009`, `2010`, `2011`, `2012`, `2013`, `2014`, `2015`
              FROM c24
              INNER JOIN c25
              ON c24.PROVINCE = c25.Province
              
             ")

c27 <- c26 %>%
  pivot_longer(`2005`:`2015`, names_to = "Year", values_to = "Total for each year")
c28 <- as.data.frame(c26)
c27$Year <- as.character(c27$Year)

c27$Year <- factor(c27$Year, levels=unique(c27$Year))

c29 <- ggplot(c27, aes(x=Year, y=`Total for each year`))+geom_point(aes(colour = Year))+facet_wrap(~PROVINCE)+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+ 
  ggtitle("Scatter plot representing the change in total number of Crimes Commited for each Province between 2005 and 2015 ")+ 
  scale_y_continuous(labels = scales::comma)+ylab("Total reports for each year")

c47 <- sqldf("SELECT pp.Province, Density, Crime_Rate_Per_100000
             FROM pp
             INNER JOIN c11
             ON pp.Province = c11.Province
             ORDER BY Density
            ")
c48 <- ggplot(c47, aes(x=Density, y=Crime_Rate_Per_100000))+geom_point()+
  geom_smooth(method='lm',se = FALSE, formula= y~x)+ylab("Crime Per 100000")+
  xlab("Population Density")+
  ggtitle("Scatter plot with linear regression line representing the relationship between Crime Rate per 100000 and Population Density")

c49 <- cor(c47$Density,c47$Crime_Rate_Per_100000,method = "pearson")
c50 <- sqldf("SELECT sac.Province, sac.Station, Crime_Rate_Per_100000
             FROM sac
             INNER JOIN c11
             ON sac.Province = c11.Province
             GROUP BY Station
            ")
c51 <- data.frame(rep(0:10,9))
colnames(c51)[1] <- "Delta(Year - 2005)"
c52 <- mutate(c51, vals = c27$`Total for each year`)
c53 <- mutate(c52, Province = c27$PROVINCE)
c54 <- ggplot(c53, aes(x=`Delta(Year - 2005)`, y=vals))+geom_point(aes(colour = `Delta(Year - 2005)`))+
  facet_wrap(~Province)+geom_smooth(method='lm',se = FALSE, formula= y~x)+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+ 
  ggtitle("Scatter plot representing the change in total number of Crimes Commited for each Province between 2005 and 2015 ")+ 
  scale_y_continuous(labels = scales::comma)+ylab("Total reports for each year")

c56 <- cor(c53$`Delta(Year - 2005)`[1:11],c53$vals[1:11])
c57 <- cor(c53$`Delta(Year - 2005)`[12:22],c53$vals[12:22])
c58 <- cor(c53$`Delta(Year - 2005)`[23:33],c53$vals[23:33])
c59 <- cor(c53$`Delta(Year - 2005)`[34:44],c53$vals[34:44])
c60 <- cor(c53$`Delta(Year - 2005)`[45:55],c53$vals[45:55])
c61 <- cor(c53$`Delta(Year - 2005)`[56:66],c53$vals[56:66])
c62 <- cor(c53$`Delta(Year - 2005)`[67:77],c53$vals[67:77])
c63 <- cor(c53$`Delta(Year - 2005)`[78:88],c53$vals[78:88])
c64 <- cor(c53$`Delta(Year - 2005)`[89:99],c53$vals[89:99])
