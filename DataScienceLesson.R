#http://jeffgoldsmith.com/DSI/topic_data_wrangling_i.html
#http://jeffgoldsmith.com/DSI/data_import.html
#https://github.com/jeff-goldsmith/DSI/tree/master/data

library(tidyverse)


getwd()


litters_data = readr::read_csv(file = "./data/FAS_litters.csv")
