#https://www3.nd.edu/~steve/computing_with_data/24_dplyr/dplyr.html

library(dplyr)

library(hflights)
head(hflights)

str(hflights)

hflights_df <- tbl_df(hflights)
class(hflights_df)

planes <- group_by(hflights_df, TailNum)
planes

delay2 <- summarize(planes, count = n(), dist = mean(Distance, na.rm = T), 
delay = mean(ArrDelay,na.rm = T))
delay2

daily <- group_by(hflights_df, Year, Month, DayofMonth)

# Other examples of Groupby

