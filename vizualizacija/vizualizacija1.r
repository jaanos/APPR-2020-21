library(ggplot2)
library(dplyr)
library(tidyverse)

vrste_dohodka_08_19 <- vrste_dohodka %>%
  filter(Leto == 2008 | Leto == 2019) %>%
  arrange(Vrsta.dohodka, desc(Leto), Dohodek) %>%
  select(Vrsta.dohodka, Leto, Dohodek)

graf_vrste_dohodka_08_19 <- ggplot(vrste_dohodka_08_19, aes(x=Vrsta.dohodka, 
                                                            y=Dohodek, fill=factor(Leto))) + 
  geom_histogram(stat = "identity", binwidth=1, position="identity", color="black") +
  labs(title="Dohodek glede na vrsto", x="Vrsta dohodka", y = "Dohodek", fill="Leto") +
  scale_x_discrete(guide=guide_axis(n.dodge=2))
print(graf_vrste_dohodka_08_19)