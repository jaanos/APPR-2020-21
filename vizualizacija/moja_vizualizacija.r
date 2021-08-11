#View(stat17[c("round", "goals_scored", "own_goals")] %>% group_by(round) %>% summarise(st_golov=sum(goals_scored + own_goals)))


goli_po_krogih17 <- skupna17[c("round", "goals_scored", "own_goals")] %>% group_by(round) %>%
  summarise(st_zadetkov_17 = sum(goals_scored + own_goals))

goli_po_krogih18 <- skupna18[c("round", "goals_scored", "own_goals")] %>% group_by(round) %>%
  summarise(st_zadetkov_18 = sum(goals_scored + own_goals)) %>% select(-round)

goli_po_krogih19 <- skupna19[c("round", "goals_scored", "own_goals")] %>% group_by(round) %>%
  summarise(st_zadetkov_19 = sum(goals_scored + own_goals)) %>% select(-round)

razp <-cbind(goli_po_krogih17,goli_po_krogih18, goli_po_krogih19)


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


