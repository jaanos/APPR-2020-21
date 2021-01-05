# 4. faza: Analiza podatkov

#LINEARNA REGRESIJA

library(ggplot2)
library(GGally)
library(mgcv)

g <- ggplot(tabela_tri_zdruzene, aes(x=skupaj.x, y=skupaj.y)) + geom_point()
#print(g)
graf9 <- g + geom_smooth(method="lm")
kv <- lm(data=tabela_tri_zdruzene, skupaj.y ~ skupaj.x + I(skupaj.x^2))
graf12 <- g + geom_smooth(method="lm", formula=y ~ x + I(x^2)+ I(x^3) + I(x^4) + I(x^5))
#lin <- lm(data=tabela_tri_zdruzene, skupaj.x ~ skupaj.y)
#lin
#predict(lin, data.frame(skupaj.y=seq(100, 800, 100)))

p <- ggplot(tabela_tri_zdruzene, aes(x=skupaj.x, y=skupaj)) + geom_point()
#print(p)
graf10 <- p + geom_smooth(method="lm")
#p + geom_smooth(method="loess")
kv <- lm(data=tabela_tri_zdruzene, skupaj ~ skupaj.x + I(skupaj.x^2))
graf11 <- p + geom_smooth(method="lm", formula=y ~ x + I(x^2)+ I(x^3) + I(x^4) + I(x^5))
