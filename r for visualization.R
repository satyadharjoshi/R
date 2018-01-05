#library(RColorBrewer)
library(ggplot2)
library(dplyr)
mpg

diamonds



ggplot(mpg, aes(x = displ, y = hwy)) +  geom_point()


ggplot(economics, aes(date, unemploy)) + geom_line()


qplot(displ, hwy, data = mpg, colour = "blue")


require(ggplot2, quietly = TRUE, warn.conflicts = FALSE)
require(dplyr, quietly = TRUE, warn.conflicts = FALSE)
options(digits = 4, dplyr.print_min = 6, dplyr.print_max = 6)
mpg


summary(mpg)



mpgTol100km <- function(milespergallon){
  GalloLiter <- 3.785411784
  MileKilometer <- 1.609344 
  
  l100km <- (100*GalloLiter)/(milespergallon*MileKilometer)
  l100km
}


apply(mpg[, c("cty", "hwy")], 2, mpgTol100km) %>% 
  head()

mpg



mpg %>% 
  transmute("cty_l100km" = mpgTol100km(cty),
            "hwy_l100km" = mpgTol100km(hwy))

q4_1 <- mpg %>% 
  group_by(manufacturer) %>% 
  tally(sort = TRUE)


q4_2 <- mpg %>% 
  group_by(manufacturer) %>% 
  transmute("n" = length(unique(model))) %>% 
  unique() %>%
  ungroup() %>% 
  arrange(desc(n))



df <- data.frame(rbind(q4_1, q4_2), "Type" = as.factor(rep(c("overall", "unique"), each = 15)))

ggplot(df, aes(x = reorder(as.factor(manufacturer), n), y = n, fill = Type)) + 
  geom_bar(width = 0.5, stat = "identity", position = "dodge") + 
  theme(panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank()) +
  scale_y_continuous(breaks = seq(0,40, by = 5)) + 
  ggtitle("Number of models per manufacturer") +
  labs(y = "Count", x = "Model") +
  coord_flip()





ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth()


ggplot() +
  geom_point(
    data = mpg,
    mapping = aes(x = displ, y = hwy)
  ) +
  geom_smooth(
    data = mpg,
    mapping = aes(x = displ, y = hwy)
  )



base1 <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy, group=drv))
base1 + geom_point() + geom_smooth(se = FALSE)
base1 + geom_smooth(se = FALSE) + geom_point()


base2 <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy))
base2 + geom_point(size = 4, colour = "white") + geom_point(aes(colour = drv))




ggplot(data = diamonds) +
  stat_summary(
    mapping = aes(x = cut, y = depth)
  )



ggplot(data = mpg, mapping = aes(x = factor(1), fill = class)) +
  geom_bar(width = 1)+
  coord_polar(theta = "y")

library(nycflights13)

filter(nycflights13::flights, month == 1)

