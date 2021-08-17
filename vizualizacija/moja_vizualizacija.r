# LE ZA ZGLED KAKO DODAJAMO ČRTE POSTOPOMA
ggplot(cbind(skupna17[c("round", "goals_scored", "own_goals")] %>% group_by(round) %>%
               summarise(st_zadetkov_17 = sum(goals_scored + own_goals)),
             skupna18[c("round", "goals_scored", "own_goals")] %>% group_by(round) %>%
               summarise(st_zadetkov_18 = sum(goals_scored + own_goals)) %>% select(-round),
             skupna19[c("round", "goals_scored", "own_goals")] %>% group_by(round) %>%
               summarise(st_zadetkov_19 = sum(goals_scored + own_goals)) %>% select(-round))) +
  aes(x=round) + aes(y = st_zadetkov_17, colour = "st_zadetkov_17") + geom_line() + 
  geom_line(aes(y = st_zadetkov_18, colour = "st_zadetkov_18")) +
  geom_line(aes(y = st_zadetkov_19, colour = "st_zadetkov_19"))




goli_po_sezonah <- cbind(skupna17[c("goals_scored", "own_goals","was_home")] %>%
                           group_by(was_home) %>%
                           summarise("2016/17"=sum(goals_scored + own_goals)) %>%
                           rename(Teren=was_home),
                         skupna18[c("goals_scored", "own_goals","was_home")] %>%
                           group_by(was_home) %>%
                           summarise("2017/18"=sum(goals_scored + own_goals)) %>% select(-was_home),
                         skupna19[c("goals_scored", "own_goals","was_home")] %>%
                           group_by(was_home) %>%
                           summarise("2018/19"=sum(goals_scored + own_goals)) %>%
                           select(-was_home)) %>%
  pivot_longer(-Teren,names_to="sezona", values_to="stevilo_golov") %>%
  mutate(Teren=sapply(Teren, function(x) {if (x == TRUE) {x="Doma"} else {x="V gosteh"}}))

ggplot(goli_po_sezonah) + aes(x=sezona, y=stevilo_golov, fill=Teren) + geom_col(position="dodge") +
  geom_text(aes(label = stevilo_golov), vjust=1.5, position=position_dodge(0.9)) +
  ggtitle("Število zadetkov doma in v gosteh") + ylab("Število zadetkov") + xlab("Sezona")


goli_pozicija <- cbind(kom17[c("goals_scored","element_type")] %>% filter(element_type < 5) %>%
                         group_by(element_type) %>%
                         summarise(odstotek_zadetkov17 = sum(goals_scored)/sum(kom17$goals_scored) * 100),
                       kom18[c("goals_scored","element_type")] %>% filter(element_type < 5) %>%
                         group_by(element_type) %>% 
                         summarise(odstotek_zadetkov18 = sum(goals_scored)/sum(kom17$goals_scored) * 100) %>%
                         select(-element_type),
                       kom19[c("goals_scored","element_type")] %>% filter(element_type < 5) %>%
                         group_by(element_type) %>%
                         summarise(odstotek_zadetkov19 = sum(goals_scored)/sum(kom17$goals_scored) * 100) %>%
                         select(-element_type)) %>%
  mutate(odstotek_zadetkov17=round(odstotek_zadetkov17,2),
         odstotek_zadetkov18=round(odstotek_zadetkov18,2),
         odstotek_zadetkov19=round(odstotek_zadetkov19,2)) %>% rename(Pozicija = element_type) %>%
    mutate(Pozicija = c("Vratar", "Branilec", "Vezist", "Napadalec")[Pozicija]) %>%
  filter(odstotek_zadetkov17 > 0, odstotek_zadetkov18 > 0, odstotek_zadetkov19 > 0) %>%
  rename("2016/17" = odstotek_zadetkov17, "2017/18" = odstotek_zadetkov18,
         "2018/19" = odstotek_zadetkov19) %>%
  pivot_longer(-Pozicija, names_to = "sezona", values_to = "odstotek_zadetkov")
    
ggplot(goli_pozicija) + aes(x=sezona, y=odstotek_zadetkov, fill=Pozicija) +
  geom_col(position = "dodge") +
  geom_text(aes(label = odstotek_zadetkov), vjust=1.5, position=position_dodge(0.9)) +
  ggtitle("Odstotek vseh zadetkov razdeljen po igralnih položajih") + ylab("Odstotek zadetkov") + xlab("Sezona")




















