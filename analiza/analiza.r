# 4. faza: Analiza podatkov

# predikcija povprečnega razpoložljivega dohodka (moški)
spol_moski <- spol %>%
  filter(Spol == "Moški")

model <- lm(Dohodek ~ Leto , data = spol_moski)

prihodnost <- data.frame(Leto = seq(2020,2024), Spol = "Moški")

napoved <- prihodnost %>%
  mutate(Dohodek= predict(model, .)) %>%
  select(Spol, Leto, Dohodek)

napoved_spol_moski <- bind_rows( (spol_moski),napoved)


graf_napoved_spol_moski <- ggplot(napoved_spol_moski, aes(x=Leto, y=Dohodek)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, col="red") +
  scale_x_continuous(breaks = seq(2008, 2024, 2)) +
  ggtitle("Napoved dohodka - moški")

print(graf_napoved_spol_moski)