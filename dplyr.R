#https://bookdown.org/rdpeng/rprogdatascience/managing-data-frames-with-the-dplyr-package.html



library(dplyr)

chicago <- readRDS("chicago.rds")


dim(chicago)


str(chicago)

select(chicago, -(city:dptp))


chicago <- arrange(chicago, date)


head(select(chicago, date, pm25tmean2), 3)

chicago <- rename(chicago, dewpoint = dptp, pm25 = pm25tmean2)

chicago <- mutate(chicago, pm25detrend = pm25 - mean(pm25, na.rm = TRUE))

chicago <- mutate(chicago, year = as.POSIXlt(date)$year + 1900)

years <- group_by(chicago, year)




#https://www.r-bloggers.com/introducing-dplyr/

library(Lahman)
library(dplyr)

players <- group_by(Batting, playerID)
games <- summarise(players, total = sum(G))
head(arrange(games, desc(total)), 5)





#https://www.rdocumentation.org/packages/dplyr/versions/0.7.2/topics/group_by
#for checking group by



#http://www.listendata.com/2016/08/dplyr-tutorial.html

mydata = read.csv("sampledata.csv")


sample_n(mydata,3)

mydata2 = select(mydata, Index, State:Y2008)


mydata7 = filter(mydata, Index == "A")

mydata6 = rename(mydata, Index1=Index)


mydata8 = filter(mydata6, Index1 %in% c("A", "C") & Y2002 >= 1300000 )



summarise(mydata, Y2015_mean = mean(Y2015), Y2015_med=median(Y2015))

dt = mydata %>% select(Index, State) %>% sample_n(10)

t = summarise_at(group_by(mydata, Index), vars(Y2011, Y2012), funs(n(), mean(., na.rm = TRUE)))

t = mydata %>% group_by(Index) %>%
  summarise_at(vars(Y2011:Y2015), funs(n(), mean(., na.rm = TRUE)))



t = summarise_at(group_by(mydata, Index), vars(Y2011, Y2012), funs(n(), mean(., na.rm = TRUE)))

t = mydata %>% group_by(Index) %>%
  summarise_at(vars(Y2011:Y2015), funs(n(), mean(., na.rm = TRUE)))


t = mydata %>% filter(Index %in% c("A", "C","I")) %>% group_by(Index) %>%
  do(head( . , 2))
t = mydata %>% filter(Index %in% c("A", "C","I")) %>% group_by(Index) %>%
  do(head( . , 2))

head(mydata, . , 2)

slice(mydata,3)

t = mydata %>% select(Index, Y2015) %>%
  filter(Index %in% c("A", "C","I")) %>%
  group_by(Index) %>%
  do(arrange(.,desc(Y2015))) %>%  slice(3)

t = mydata %>% select(Index, Y2015) %>%
  filter(Index %in% c("A", "C","I")) %>%
  group_by(Index) %>%
  do(arrange(.,desc(Y2015))) %>%  slice(3)


t = mydata %>% select(Index, Y2015) %>%
  filter(Index %in% c("A", "C","I")) %>%
  group_by(Index) %>%
  filter(min_rank(desc(Y2015)) == 3)


t = mydata %>%
  group_by(Index)%>%
  summarise(Mean_2014 = mean(Y2014, na.rm=TRUE),
            Mean_2015 = mean(Y2015, na.rm=TRUE)) %>%
  arrange(desc(Mean_2015))


#https://cran.r-project.org/web/packages/dplyr/vignettes/programming.html



library(nycflights13)
library(tidyverse)



flights_sml <- select(flights,
                      year:day,
                      ends_with("delay"),
                      distanceis.na(x)is.na(x),
                      air_time )

mutate(flights_sml,
       gain = arr_delay - dep_delay,
       speed = distance / air_time * 60)



arrange(flights, desc(arr_delay))

View(flights)

arrange(flights,dep_delay)


df <- tibble(x = c(5, sin(1), NA))

arrange(df, desc(is.na(x)))

x <- flights %>% mutate(travel_time = ifelse((arr_time - dep_time < 0), 
                                        2400+(arr_time - dep_time),
                                        arr_time - dep_time)) %>% 
  arrange(travel_time) %>% select(arr_time, dep_time, travel_time)

select(flights,1:5)

arrange(flights, desc(distance)) %>% select(1:5, distance)

flights %>% select(matches("^dep|^arr_time$|delay$"))


x=c(1:4)
y=(1:4)
x==y
x(1,23,4)
x=c('d','d')
x


select(flights,
 air_time,         
       gain = arr_delay - dep_delay ,
         hours = air_time / 60,
          gain_per_hour = gain / hours
)
View(diamonds)
