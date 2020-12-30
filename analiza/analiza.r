# 4. faza: Analiza podatkov

# Podatke katere bomo modelirali

podatki <- st12 %>% 
   mutate(emp=parse_number(TOT_EMP, locale=locale(decimal_mark = ",",grouping_mark = ".") ))  %>%
   mutate(AM=parse_number(A_MEAN, locale=locale(decimal_mark = ".",grouping_mark = ",") )) %>% 
  mutate(AME=parse_number(A_MEDIAN, locale=locale(decimal_mark = ",",grouping_mark = ".") )) 
podatki$H_MEAN  <- as.numeric(as.character(podatki$H_MEAN))
podatki$H_MEDIAN <- as.numeric(as.character(podatki$H_MEDIAN))
podatki <- podatki  %>% filter(STATE=="Alaska") %>% filter(AM < 33) %>%
  .[c(1,2,4,6,8,9,10)] %>% drop_na(H_MEAN, H_MEDIAN,AME, AM, emp) %>% rename(HM=H_MEAN, HME=H_MEDIAN)
imena <- podatki %>% .[c(3,4,5,6,7)] %>% names()

#Prileganje podatkom

# ggpairs(leto.norm %>% select(H_MEAN, AME, AM, emp))

# Različni modeli predikcij gibanja 

g <- ggplot(podatki, aes(x=AM, y=emp)) + geom_point()

lin <- lm(data=podatki, AM ~ emp)
g1 <- g +  geom_smooth(method="lm", se = FALSE) 

kv <- lm(data=podatki, AM ~ emp + I(emp^2))
g2 <- ggplot(podatki, aes(x=AM, y=emp)) + 
  geom_point() + 
  geom_smooth(method="lm", formula=y ~ x+ I(x^2) ) 


mls <- loess(data=podatki, AM ~ emp)
g3 <- g + geom_smooth(method = "loess") +               
    scale_x_continuous(name = "AM", breaks = seq(0,30,5)) 


mgam <- gam(data=podatki, AM ~ s(emp))
g4 <- g + geom_smooth(method="gam", formula=y ~ s(x), fullrange=TRUE, 
                      # se=FALSE, 
                      color="red")

g5 <- g4 +xlim(20,30) + ylim(0,10000)

c1 <-cor(podatki$emp,podatki$AM)


