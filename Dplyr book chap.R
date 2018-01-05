library(nycflights13)
library(tidyverse)
library(ggplot2)
library(Lahman)



by_dest <- group_by(flights, dest)

delay <- summarize(by_dest, count_flights = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE)
)
delay <- filter(delay, count_flights > 20, dest != "HNL")


ggplot( data = delay, mapping = aes(x = dist, y = delay)) +
  geom_point(aes(size = count_flights), alpha = 1/2) +
  geom_smooth(se = FALSE) 


delays <- flights %>%
  group_by(dest) %>%
  summarize(
    count = n(),
    dist = mean(distance, na.rm = TRUE),
    delay = mean(arr_delay, na.rm = TRUE)
  ) %>%
  filter(count > 20, dest != "HNL") 


delays <- not_cancelled %>%
  group_by(tailnum) %>%
  summarize(
    delay = mean(arr_delay)
  )
ggplot(data = delays, mapping = aes(x = delay)) +
  geom_freqpoly(binwidth = 10)

 
   
  summarize((group_by(flights, year, month, day)), 
              mean = mean(dep_delay, na.rm = TRUE))



not_cancelled <- flights %>%
  filter(!is.na(dep_delay), !is.na(arr_delay))
not_cancelled %>%
  group_by(year, month, day) %>%
  summarize(mean = mean(dep_delay))



delays <- not_cancelled %>%
  group_by(tailnum) %>%
  summarize(
    delay = mean(arr_delay, na.rm = TRUE),
    n = n()
  )
ggplot(data = delays, mapping = aes(x = n, y = delay)) +
  geom_point(alpha = 1/10)



# Convert to a tibble so it prints nicely
batting <- as_tibble(Lahman::Batting)
batters <- batting %>%
  group_by(playerID) %>%
  summarize(
    ba = sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE),
    ab = sum(AB, na.rm = TRUE)
  )
batters %>%
  filter(ab > 100) %>%
  ggplot(mapping = aes(x = ab, y = ba)) +
  geom_point() +
  geom_smooth(se = FALSE)
#> `geom_smooth()` using method = 'gam'





not_cancelled %>%
  group_by(year, month, day) %>%
  summarize(
    # average delay:
    avg_delay1 = mean(arr_delay),
    # average positive delay:
    avg_delay2 = mean(arr_delay[arr_delay > 0])
  )


#solutions 
#https://github.com/maxconway/r4ds_solutions/blob/master/transform_solutions.Rmd
#http://cfss.uchicago.edu/r4ds_solutions.html
#https://github.com/maxconway/r4ds_solutions/blob/master/transform_solutions.Rmd



select(flights,tailnum)


flights %>%
  group_by(tailnum) %>%
  summarise(prop_on_time = sum(arr_delay <= 30 & !is.na(arr_delay))/n(),
            mean_arr_delay = mean(arr_delay, na.rm=TRUE),
            flights = n()) %>%
  arrange(prop_on_time, desc(mean_arr_delay))


flights %>%
  ggplot(aes(x=factor(hour), fill=arr_delay>5 | is.na(arr_delay))) + geom_bar()


View(flights)






delays <- not_cancelled %>%
  group_by(tailnum) %>%
  summarize(
    delay = mean(arr_delay)
  )
ggplot(data = delays, mapping = aes(x = delay)) +
  geom_histogram(binwidth = 10)+
  geom_freqpoly(binwidth = 10)
  


delays <- not_cancelled %>%
  group_by(tailnum) %>%
  summarize(
    delay = mean(arr_delay, na.rm = TRUE),
    n = n()
  )

ggplot(data = delays, mapping = aes(x = n, y = delay)) +
  geom_point(alpha = 1/10)


  ggplot(mapping = aes(x = n, y = delay)) +
  geom_point(data = (  filter(delays,n > 25)), color = "red", alpha = 1/10)+
  geom_point(data = (  filter(delays,n < 25)), color = "blue", alpha = 1/10)
  

  
  batting <- as_tibble(Lahman::Batting)
  batters <- batting %>%
    group_by(playerID) %>%
    summarize(
      ba = sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE),
      ab = sum(AB, na.rm = TRUE)
    )
  
  batters %>%
    filter(ab > 100) %>%
    ggplot(mapping = aes(x = ab, y = ba)) +
    geom_point() +
    geom_smooth(se = FALSE)
  #> `geom_smooth()` using method = 'gam'

  
  
  
  ggplot(diamonds) +
    geom_histogram(mapping = aes(x = y), binwidth = 0.5) +
    coord_cartesian(ylim = c(0, 50))
  Variation
  
  
  
  unusual <- diamonds %>%
    filter(y < 3 | y > 20) %>%
    arrange(y)
  unusual
  
  
  
  diamonds2 <- diamonds %>%
    mutate(y = ifelse(y < 3 | y > 20, NA, y))
  
  
  ggplot(data = diamonds2, mapping = aes(x = x, y = y)) +
    geom_point()
  
  
  
  nycflights13::flights %>%
    mutate(
      cancelled = is.na(dep_time),
      sched_hour = sched_dep_time %/% 100,
      sched_min = sched_dep_time %% 100,
      sched_dep_time = sched_hour + sched_min / 60
    ) %>%
    ggplot(mapping = aes(sched_dep_time)) +
    geom_freqpoly(
      mapping = aes(color = cancelled),
      binwidth = 1/4
    )
  
  
  
  
  mtcars %>% 
    dplyr::group_by(cyl, gear) %>%
    dplyr::summarise(length(gear))
  
  
  ggplot(
    data = diamonds,
    mapping = aes(x = price, y = ..density..)
  ) +
    geom_freqpoly(mapping = aes(color = cut), binwidth = 500)

  
  
  
  ggplot(data = diamonds, mapping = aes(x = cut, y = price)) +
    geom_boxplot()
  
  
  
  read_csv("a,b,c\n1,2,.", na = ".")
  
  
  x <- (parse_logical(c("TRUE", "FALSE", "NA")))
  # Str stands for strcuture and not string 
  
  
  