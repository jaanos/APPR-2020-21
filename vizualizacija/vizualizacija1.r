library(ggplot2)
library(dplyr)
library(tidyverse)

vrste_dohodka_2019 <- vrste_dohodka %>%
  filter(Leto == 2019) %>%
  select(Vrsta.dohodka, Dohodek)

graf_vrste_dohodka_2019 <- ggplot(vrste_dohodka_2019, aes(x=Vrsta.dohodka, y=Dohodek)) + 
  geom_histogram(stat = "identity")
print(graf_vrste_dohodka_2019)