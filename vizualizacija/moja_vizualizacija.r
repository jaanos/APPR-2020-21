#View(stat17[c("round", "goals_scored", "own_goals")] %>% group_by(round) %>% summarise(st_golov=sum(goals_scored + own_goals)))


goli_po_krogih17 <- skupna17[c("round", "goals_scored", "own_goals")] %>% group_by(round) %>%
  summarise(st_zadetkov_17 = sum(goals_scored + own_goals))

goli_po_krogih18 <- skupna18[c("round", "goals_scored", "own_goals")] %>% group_by(round) %>%
  summarise(st_zadetkov_18 = sum(goals_scored + own_goals)) %>% select(-round)

goli_po_krogih19 <- skupna19[c("round", "goals_scored", "own_goals")] %>% group_by(round) %>%
  summarise(st_zadetkov_19 = sum(goals_scored + own_goals)) %>% select(-round)

razp <-cbind(goli_po_krogih17,goli_po_krogih18, goli_po_krogih19)

f <-function(x) {if (x == TRUE) {x="Doma"} else {x="V gosteh"}}
ggplot(cbind(skupna17[c("round", "goals_scored", "own_goals")] %>% group_by(round) %>%
               summarise(st_zadetkov_17 = sum(goals_scored + own_goals)),
             skupna18[c("round", "goals_scored", "own_goals")] %>% group_by(round) %>%
               summarise(st_zadetkov_18 = sum(goals_scored + own_goals)) %>% select(-round),
             skupna19[c("round", "goals_scored", "own_goals")] %>% group_by(round) %>%
               summarise(st_zadetkov_19 = sum(goals_scored + own_goals)) %>% select(-round))) +
  aes(x=round) + aes(y = st_zadetkov_17, colour = "st_zadetkov_17") + geom_line() + 
  geom_line(aes(y = st_zadetkov_18, colour = "st_zadetkov_18")) +
  geom_line(aes(y = st_zadetkov_19, colour = "st_zadetkov_19"))


# TO DELUJE: geom_line(aes(y = st_zadetkov_17, colour = "st_zadetkov_17"))
#g <- ggplot(skupna17[c("round", "goals_scored", "own_goals")] %>% group_by(round) %>%
#               summarise(st_zadetkov = sum(goals_scored + own_goals))) + 
#  aes(x=round, y=st_zadetkov) + geom_line()

#h <- ggplot(skupna18[c("round", "goals_scored", "own_goals")] %>% group_by(round) %>%
#               summarise(st_zadetkov = sum(goals_scored + own_goals))) + 
#  aes(x=round, y=st_zadetkov) + geom_line()

goli_po_sezonah <- cbind(skupna17[c("goals_scored", "own_goals","was_home")] %>% group_by(was_home) %>%
  summarise("2016/17"=sum(goals_scored + own_goals)) %>% rename(Teren=was_home),
  skupna18[c("goals_scored", "own_goals","was_home")] %>% group_by(was_home) %>%
    summarise("2017/18"=sum(goals_scored + own_goals)) %>% select(-was_home),
  skupna19[c("goals_scored", "own_goals","was_home")] %>% group_by(was_home) %>%
    summarise("2018/19"=sum(goals_scored + own_goals)) %>% select(-was_home)) %>%
  pivot_longer(-Teren,names_to="sezona", values_to="stevilo_golov") %>%
  mutate(Teren=sapply(Teren, function(x) {if (x == TRUE) {x="Doma"} else {x="V gosteh"}}))

#podatki <- goli_po_sezonah %>% pivot_longer(-Teren,names_to="sezona", values_to="št. golov")
ggplot(goli_po_sezonah) + aes(x=sezona, y=stevilo_golov, fill=Teren) + geom_col(position="dodge") +
  geom_text(aes(label = stevilo_golov), vjust=1.5, position=position_dodge(0.9)) +
  ggtitle("Število zadetkov doma in v gosteh") + ylab("Število zadetkov") + xlab("Sezona")

goli_pozicija <- kom17[c("goals_scored","element_type")] %>% group_by(element_type) %>%
  summarise(odstotek_zadetkov = sum(goals_scored)/sum(kom17$goals_scored) * 100) %>%
  mutate(element_type = c("Vratar", "Branilec", "Vezist", "Napadalec")[element_type]) %>%
  rename(Pozicija=element_type) %>% filter(odstotek_zadetkov > 0) %>%
  mutate(odstotek_zadetkov=round(odstotek_zadetkov,2))

ggplot(goli_pozicija) + aes(x="", y=odstotek_zadetkov, fill=Pozicija) + geom_col() +
  coord_polar(theta="y") + geom_text(aes(label=odstotek_zadetkov))
pie(goli_pozicija$odstotek_zadetkov, labels=paste(goli_pozicija$Pozicija, goli_pozicija$odstotek_zadetkov, "%"),
    main="Odstotek zadetkov po igralnih pozicijah")
