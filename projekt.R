remove(list = ls())
setwd('C:\\Users\\lenovo\\Desktop\\Nowy folder')
library(tidyverse)
library(lattice)
library(ggplot2)
options(scipen = 9999)
data <- read.csv('master.csv', sep = ',')

data <- data %>% filter(year < 2016)

table(data$age)
data$age <- factor(data$age, levels = c('5-14 years',
                   '15-24 years',
                   '25-34 years',
                   '35-54 years',
                   '55-74 years',
                   '75+ years'))
#Srednia liczba samobojstw w czasie

datastacked <- data %>% group_by(year) %>% 
  dplyr::summarize(Mean = mean(suicides.100k.pop, na.rm = TRUE))


ggplot(data = datastacked, mapping = aes(x = year, y = Mean))+
  geom_line(size = 1, col = 'green')+ggtitle('Œrednia liczba samobójstw na 100k w czasie')+
  geom_point(col = 'green')



#podzial na plec (samobojstwa)
datastackedsex <- data %>% group_by(sex, year) %>% 
  dplyr::summarize(Mean = mean(suicides.100k.pop, na.rm = TRUE))


ggplot(data = datastackedsex, mapping = aes(x = year, y = Mean, col = sex))+
  geom_line(size = 1)+facet_wrap(~sex)+
  ggtitle('Œrednia liczba samobójstw na 100k w czasie')+
  geom_point()

#podzial na wiek (samobojstwa)
datastackedage <- data %>% group_by(age, year) %>% 
  dplyr::summarize(Mean = mean(suicides.100k.pop, na.rm = TRUE))


ggplot(data = datastackedage, mapping = aes(x = year, y = Mean, col = age))+
  geom_line(size = 1)+facet_wrap(~age)+
  ggtitle('Œrednia liczba samobójstw na 100k w czasie')+
  geom_point()

#podzial na generacje (samobojstwa)

datastackedgen <- data %>% group_by(generation, year) %>% 
  dplyr::summarize(Mean = mean(suicides.100k.pop, na.rm = TRUE))


ggplot(data = datastackedgen, mapping = aes(x = year, y = Mean, col = generation))+
  geom_line(size = 1)+facet_wrap(~generation)+
  ggtitle('Œrednia liczba samobójstw na 100k w czasie')+geom_point()

#Polska w porownaniu do swiata (samobojstwa)

str(data)
datastackedf <- data %>% group_by(sex, year) %>% 
  dplyr::summarize(Mean = mean(suicides.100k.pop, na.rm = TRUE)) %>% 
  filter(year > 1989, sex == 'female')


datastackedm <- data %>% group_by(sex, year) %>% 
  dplyr::summarize(Mean = mean(suicides.100k.pop, na.rm = TRUE)) %>%
  filter(year > 1989, sex == 'male')





datastackedPolf <- data %>% group_by(sex, year) %>% 
  filter(ï.¿country == 'Poland', sex == 'female' ) %>% 
  dplyr::summarize(Mean = mean(suicides.100k.pop, na.rm = TRUE))

datastackedPolm <- data %>% group_by(sex, year) %>% 
  filter(ï.¿country == 'Poland', sex == 'male' ) %>% 
  dplyr::summarize(Mean = mean(suicides.100k.pop, na.rm = TRUE))


par(mar = rep(2, 4))
par(mfrow=c(2,2))

plot(datastackedf$year, datastackedf$Mean, type="l", col = 'red',
     main = 'Œrednia liczba samobójstw kobiet na œwiecie (na 100k)', ylim = c(2.5,6.5))
plot(datastackedPolf$year, datastackedPolf$Mean, type="l", col = 'red',
     main = 'Œrednia liczba samobójstw kobiet w Polsce (na 100k)', ylim = c(2.5,6.5))



plot(datastackedm$year, datastackedm$Mean, type="l", col = 'blue',
     main = 'Œrednia liczba samobójstw mê¿czyzn na œwiecie (na 100k)', ylim = c(16,30))
plot(datastackedPolm$year, datastackedPolm$Mean, type="l", col = 'blue',
     main = 'Œrednia liczba samobójstw mê¿czyzn w Polsce (na 100k)', ylim = c(16,30))


  #Histogram samobojstw w roku 2015

datapop <- data %>% filter(year == 2015) %>% group_by(ï.¿country, year)


ggplot(data = datapop, mapping = aes(x = suicides.100k.pop, fill = sex))+
  geom_histogram()+facet_wrap(sex~age, ncol = 3)+ggtitle('Rozk³ad samobójstw na œwiecie w roku 2015 w podziale na grupy wiekowe')



#ktore kraje maja najwyzsza srednia samobojstw


#ogolnie
datacountries <- data %>% group_by(ï.¿country) %>% 
  dplyr::summarize(Mean = mean(suicides.100k.pop, na.rm = TRUE)) %>% 
  arrange(desc(Mean)) %>% top_n(25)


ggplot(data = datacountries, 
       mapping = aes(x = reorder(ï.¿country, Mean), y = Mean))+
  geom_bar(stat = 'identity', fill = 'green', col = 'black')+
  coord_flip()+xlab('Pañstwo')+ylab('Œrednia liczba samobójstw ogólnie')+
  theme(axis.text.y = element_text(size = 10))



#mezczyzni
datacountriesmale <- data %>% group_by(ï.¿country) %>%
  filter(sex == 'male') %>% 
  dplyr::summarize(Mean = mean(suicides.100k.pop, na.rm = TRUE)) %>% 
  arrange(desc(Mean)) %>% top_n(25)


ggplot(data = datacountriesmale, 
       mapping = aes(x = reorder(ï.¿country, Mean), y = Mean))+
         geom_bar(stat = 'identity', fill = 'blue', col = 'black')+
  coord_flip()+xlab('Pañstwo')+ylab('Œrednia liczba samobójstw dla mê¿czyzn')+
  theme(axis.text.y = element_text(size = 10))
        

#kobiety
datacountriesfemale <- data %>% group_by(ï.¿country) %>%
    filter(sex == 'female') %>% 
  dplyr::summarize(Mean = mean(suicides.100k.pop, na.rm = TRUE)) %>% 
  arrange(desc(Mean)) %>% top_n(25)


ggplot(data = datacountriesfemale, 
       mapping = aes(x = reorder(ï.¿country, Mean), y = Mean))+
  geom_bar(stat = 'identity', fill = 'red', col = 'black')+
  coord_flip()+xlab('Pañstwo')+ylab('Œrednia liczba samobójstw dla kobiet')+
  theme(axis.text.y = element_text(size = 10))



#Polska w porownaniu do swiata (PKB)


datastackedgdp <- data %>% group_by(year) %>% 
  dplyr::summarize(Mean = mean(gdp_per_capita...., na.rm = TRUE)) %>% 
  filter(year > 1989 & year < 2016)




datastackedgdppol <- data %>% group_by(year) %>%
  filter(year > 1989, ï.¿country == 'Poland') %>% 
  dplyr::summarize(Mean = mean(gdp_per_capita...., na.rm = TRUE)) 



xyplot(datastackedgdp$Mean~datastackedgdp$year, type = 'l', xlab = 'Rok',
       ylab = 'Poziom œredniego PKB', main = 'Œrednie PKB na œwiecie w czasie',
       ylim = c(0,30000))  

xyplot(datastackedgdppol$Mean~datastackedgdppol$year, type = 'l', xlab = 'Rok',
       ylab = 'Poziom PKB', main = 'PKB w Polsce w czasie',
       col = 'red', ylim = c(0,30000))  

#wykres babelkowy dla roku 2014


datapopstacked2014 <- data %>% filter(year == 2014) %>% 
  group_by(ï.¿country, year) %>% 
  summarise(sum = sum(population))

datasuicidestacked2014 <- data %>% filter(year == 2014) %>% 
  group_by(ï.¿country, year) %>% 
  summarise(mean = mean(suicides.100k.pop))

datagdpstacked2014 <- data %>% filter(year == 2014) %>% 
  group_by(ï.¿country, year) %>% 
  summarise(mean = mean(gdp_per_capita....))


colnames(datapopstacked2014) <- c('country', 'year', 'population')
colnames(datasuicidestacked2014) <- c('country', 'year', 'meansuicideper100k')
colnames(datagdpstacked2014) <- c('country', 'year', 'gdppercapita')


join <- left_join(datapopstacked2014, datasuicidestacked2014)
join2 <- left_join(join, datagdpstacked2014)




ggplot(data = join2, 
       mapping = aes(x = gdppercapita, y = meansuicideper100k, size = population))+
  geom_point(col = 'black', pch = 21, fill = 'deepskyblue2' )+
  ggtitle('Samobójstwa w odniesieniu do PKB per capita w 2014 roku')



#wykres babelkowy dla roku 1990


datapopstacked1990 <- data %>% filter(year == 1990) %>% 
  group_by(ï.¿country, year) %>% 
  summarise(sum = sum(population))

datasuicidestacked1990 <- data %>% filter(year == 1990) %>% 
  group_by(ï.¿country, year) %>% 
  summarise(mean = mean(suicides.100k.pop))

datagdpstacked1990 <- data %>% filter(year == 1990) %>% 
  group_by(ï.¿country, year) %>% 
  summarise(mean = mean(gdp_per_capita....))



colnames(datapopstacked1990) <- c('country', 'year', 'population')
colnames(datasuicidestacked1990) <- c('country', 'year', 'meansuicideper100k')
colnames(datagdpstacked1990) <- c('country', 'year', 'gdppercapita')


join3 <- left_join(datapopstacked1990, datasuicidestacked1990)
join4 <- left_join(join3, datagdpstacked1990)




ggplot(data = join4, 
       mapping = aes(x = gdppercapita, y = meansuicideper100k, size = population))+
  geom_point(col = 'black', fill = 'orange', pch=21)+ggtitle('Samobójstwa w odniesieniu do PKB per capita w 1990 roku')


