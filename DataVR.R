#Play with Data
library(tidyverse)
library(ggplot2)

dat = read.csv("DataVR.csv", header = TRUE)

write.csv2(batting, "writebatting.csv", sep=",")

datv2 <- dat %>%
  group_by(Counterparty_Name) %>%
  summarize(
    meantenor = mean(Tenor.year)
  )

#https://stackoverflow.com/questions/38509139/display-weighted-mean-by-group-in-the-data-frame

library(dplyr)
datv3 <- dat %>%
  group_by(Firmname) %>% 
  mutate(weighted_ten = weighted.mean(Tenor.year, EE.BHC))

datjoined <- dat 

datjoined %>% 
  left_join(datv2)

ggplot(data = mpg) +
  geom_boxplot(
    mapping = aes(
      x = reorder(class, hwy, FUN = median),
      
      y = hwy
    )
  ) +
  coord_flip()

View(mpg)

#todo 
#Create new values for tenor category and then use the heatmap


datgrouped <- dat %>%
  group_by(Firmname) %>%
  summarize(
    totalexposure = sum(EE.BHC)
  )

ggplot(data = datgrouped, mapping = aes(x = Firmname, y = totalexposure)) +
  geom_point()

#http://ggplot2.tidyverse.org/reference/geom_bar.html
ggplot(data = datgrouped, mapping = aes(x = Firmname, y = totalexposure)) +
  geom_col()


ggplot(data = dat, mapping = aes(x = Firmname, y = EE.BHC)) +
  geom_boxplot()


datacateg <- dat %>%
  mutate(tenorcat = ifelse(Tenor.year > 0 & Tenor.year < 1, '0-1',
                   ifelse( Tenor.year > 1 & Tenor.year < 100, '1-100'),
                           'other')
          )

datgrouped <- datacateg %>%
  group_by(Firmname,tenorcat) %>%
  summarize(
    totalexposure = sum(EE.BHC)
  #  countedTenor= length(tenorcat)
  )

ggplot(data = datgrouped, mapping = aes(x = Firmname, y = totalexposure, color=tenorcat)) +
  geom_col()


cars$bin <- cut(cars$dist, c(1, 10, 30, 50, 200))
ggplot(cars) + geom_boxplot(aes(bin, speed))



ggplot(mtcars, aes(factor(cyl))) + 
  geom_bar() + 
  scale_x_discrete(limits=c(8,4,6))