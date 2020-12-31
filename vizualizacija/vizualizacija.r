# 3. faza: Vizualizacija podatkov

require(ggplot2)
require(dplyr)

#Analiza plač glede na gospodarsko dejavnost, izobrazbo in spol
View(gospodarskadejavnost)
#Kmetijstvo in lov, gozdrastvo, ribištvo
gospodarskadejavnostA_osmanj_moski <- gospodarskadejavnost %>% 
  filter(izobrazba == "Osnovnošolska ali manj", spol == "Moški", 
         gospodarska.dejavnost == "A KMETIJSTVO IN LOV, GOZDARSTVO, RIBIŠTVO")
gospodarskadejavnostA_osmanj_zenske <- gospodarskadejavnost %>% 
  filter(izobrazba == "Osnovnošolska ali manj", spol == "Ženske", 
         gospodarska.dejavnost == "A KMETIJSTVO IN LOV, GOZDARSTVO, RIBIŠTVO")
gospodarskadejavnostA_sr_moski <- gospodarskadejavnost %>% 
  filter(izobrazba == "Srednješolska", spol == "Moški", 
         gospodarska.dejavnost == "A KMETIJSTVO IN LOV, GOZDARSTVO, RIBIŠTVO")
gospodarskadejavnostA_sr_zenske <- gospodarskadejavnost %>% 
  filter(izobrazba == "Srednješolska", spol == "Ženski", 
         gospodarska.dejavnost == "A KMETIJSTVO IN LOV, GOZDARSTVO, RIBIŠTVO")
gospodarskadejavnostA_vs_moski <- gospodarskadejavnost %>% 
  filter(izobrazba == "Višješolska, visokošolska", spol == "Moški", 
         gospodarska.dejavnost == "A KMETIJSTVO IN LOV, GOZDARSTVO, RIBIŠTVO")
gospodarskadejavnostA_vs_zenske <- gospodarskadejavnost %>% 
  filter(izobrazba == "Višješolska, visokošolska", spol == "Ženske", 
         gospodarska.dejavnost == "A KMETIJSTVO IN LOV, GOZDARSTVO, RIBIŠTVO") 

ggplot(gospodarskadejavnostA_osmanj_moski, aes(x= leto,y= placa)) + 
  geom_point(color="blue", size=2) +
  geom_point(data=gospodarskadejavnostA_sr_moski, aes(x= leto,y= placa), color="green", size=2) + 
  geom_point(data=gospodarskadejavnostA_vs_moski, aes(x= leto,y= placa), color="black", size=2) + 
  geom_point(data=gospodarskadejavnostA_osmanj_zenske, aes(x= leto,y= placa), color="purple", size=2) +
  geom_point(data=gospodarskadejavnostA_sr_zenske, aes(x= leto,y= placa), color="yellow", size=2) +
  geom_point(data=gospodarskadejavnostA_vs_zenske, aes(x= leto,y= placa), color="red", size=2) +
  labs(title="Primerjava plače v panogi kmetijstvo in lov, gozdrastvo, ribištvo") +
  ylab("Višina plače(€)") +
  xlab("Leto")

#Rudarstvo
gospodarskadejavnostB_osmanj_moski <- gospodarskadejavnost %>% 
  filter(izobrazba == "Osnovnošolska ali manj", spol == "Moški", 
         gospodarska.dejavnost == "B RUDARSTVO")
gospodarskadejavnostB_osmanj_zenske <- gospodarskadejavnost %>% 
  filter(izobrazba == "Osnovnošolska ali manj", spol == "Ženske", 
         gospodarska.dejavnost == "B RUDARSTVO")
gospodarskadejavnostB_sr_moski <- gospodarskadejavnost %>% 
  filter(izobrazba == "Srednješolska", spol == "Moški", 
         gospodarska.dejavnost == "B RUDARSTVO")
gospodarskadejavnostB_sr_zenske <- gospodarskadejavnost %>% 
  filter(izobrazba == "Srednješolska", spol == "Ženski", 
         gospodarska.dejavnost == "B RUDARSTVO")
gospodarskadejavnostB_vs_moski <- gospodarskadejavnost %>% 
  filter(izobrazba == "Višješolska, visokošolska", spol == "Moški", 
         gospodarska.dejavnost == "B RUDARSTVO")
gospodarskadejavnostB_vs_zenske <- gospodarskadejavnost %>% 
  filter(izobrazba == "Višješolska, visokošolska", spol == "Ženske", 
         gospodarska.dejavnost == "B RUDARSTVO")

ggplot(gospodarskadejavnostB_osmanj_moski, aes(x= leto,y= placa)) + 
  geom_point(color="blue", size=2) +
  geom_point(data=gospodarskadejavnostB_sr_moski, aes(x= leto,y= placa), color="green", size=2) + 
  geom_point(data=gospodarskadejavnostB_vs_moski, aes(x= leto,y= placa), color="black", size=2) + 
  geom_point(data=gospodarskadejavnostB_osmanj_zenske, aes(x= leto,y= placa), color="purple", size=2) +
  geom_point(data=gospodarskadejavnostB_sr_zenske, aes(x= leto,y= placa), color="yellow", size=2) +
  geom_point(data=gospodarskadejavnostB_vs_zenske, aes(x= leto,y= placa), color="red", size=2) +
  labs(title="Primerjava plače v panogi rudarstvo") +
  ylab("Višina plače(€)") +
  xlab("Leto")

#Predelovalne dejavnosti
gospodarskadejavnostC_osmanj_moski <- gospodarskadejavnost %>% 
  filter(izobrazba == "Osnovnošolska ali manj", spol == "Moški", 
         gospodarska.dejavnost == "C PREDELOVALNE DEJAVNOSTI")
gospodarskadejavnostC_osmanj_zenske <- gospodarskadejavnost %>% 
  filter(izobrazba == "Osnovnošolska ali manj", spol == "Ženske", 
         gospodarska.dejavnost == "C PREDELOVALNE DEJAVNOSTI")
gospodarskadejavnostC_sr_moski <- gospodarskadejavnost %>% 
  filter(izobrazba == "Srednješolska", spol == "Moški", 
         gospodarska.dejavnost == "C PREDELOVALNE DEJAVNOSTI")
gospodarskadejavnostC_sr_zenske <- gospodarskadejavnost %>% 
  filter(izobrazba == "Srednješolska", spol == "Ženski", 
         gospodarska.dejavnost == "C PREDELOVALNE DEJAVNOSTI")
gospodarskadejavnostC_vs_moski <- gospodarskadejavnost %>% 
  filter(izobrazba == "Višješolska, visokošolska", spol == "Moški", 
         gospodarska.dejavnost == "C PREDELOVALNE DEJAVNOSTI")
gospodarskadejavnostC_vs_zenske <- gospodarskadejavnost %>% 
  filter(izobrazba == "Višješolska, visokošolska", spol == "Ženske", 
         gospodarska.dejavnost == "C PREDELOVALNE DEJAVNOSTI")

ggplot(gospodarskadejavnostC_osmanj_moski, aes(x= leto,y= placa)) + 
  geom_point(color="blue", size=2) +
  geom_point(data=gospodarskadejavnostC_sr_moski, aes(x= leto,y= placa), color="green", size=2) + 
  geom_point(data=gospodarskadejavnostC_vs_moski, aes(x= leto,y= placa), color="black", size=2) + 
  geom_point(data=gospodarskadejavnostC_osmanj_zenske, aes(x= leto,y= placa), color="purple", size=2) +
  geom_point(data=gospodarskadejavnostC_sr_zenske, aes(x= leto,y= placa), color="yellow", size=2) +
  geom_point(data=gospodarskadejavnostC_vs_zenske, aes(x= leto,y= placa), color="red", size=2) +
  labs(title="Primerjava plače v panogi predelovalne dejavnosti") +
  ylab("Višina plače(€)") +
  xlab("Leto")

#Oskrba z električno energijo, plinom in paro
gospodarskadejavnostD_osmanj_moski <- gospodarskadejavnost %>% 
  filter(izobrazba == "Osnovnošolska ali manj", spol == "Moški", 
         gospodarska.dejavnost == "D OSKRBA Z ELEKTRIČNO ENERGIJO, PLINOM IN PARO")
gospodarskadejavnostD_osmanj_zenske <- gospodarskadejavnost %>% 
  filter(izobrazba == "Osnovnošolska ali manj", spol == "Ženske", 
         gospodarska.dejavnost == "D OSKRBA Z ELEKTRIČNO ENERGIJO, PLINOM IN PARO")
gospodarskadejavnostD_sr_moski <- gospodarskadejavnost %>% 
  filter(izobrazba == "Srednješolska", spol == "Moški", 
         gospodarska.dejavnost == "D OSKRBA Z ELEKTRIČNO ENERGIJO, PLINOM IN PARO")
gospodarskadejavnostD_sr_zenske <- gospodarskadejavnost %>% 
  filter(izobrazba == "Srednješolska", spol == "Ženski", 
         gospodarska.dejavnost == "D OSKRBA Z ELEKTRIČNO ENERGIJO, PLINOM IN PARO")
gospodarskadejavnostD_vs_moski <- gospodarskadejavnost %>% 
  filter(izobrazba == "Višješolska, visokošolska", spol == "Moški", 
         gospodarska.dejavnost == "D OSKRBA Z ELEKTRIČNO ENERGIJO, PLINOM IN PARO")
gospodarskadejavnostD_vs_zenske <- gospodarskadejavnost %>% 
  filter(izobrazba == "Višješolska, visokošolska", spol == "Ženske", 
         gospodarska.dejavnost == "D OSKRBA Z ELEKTRIČNO ENERGIJO, PLINOM IN PARO")

ggplot(gospodarskadejavnostD_osmanj_moski, aes(x= leto,y= placa)) + 
  geom_point(color="blue", size=2) +
  geom_point(data=gospodarskadejavnostD_sr_moski, aes(x= leto,y= placa), color="green", size=2) + 
  geom_point(data=gospodarskadejavnostD_vs_moski, aes(x= leto,y= placa), color="black", size=2) + 
  geom_point(data=gospodarskadejavnostD_osmanj_zenske, aes(x= leto,y= placa), color="purple", size=2) +
  geom_point(data=gospodarskadejavnostD_sr_zenske, aes(x= leto,y= placa), color="yellow", size=2) +
  geom_point(data=gospodarskadejavnostD_vs_zenske, aes(x= leto,y= placa), color="red", size=2) +
  labs(title="Primerjava plače v panogi oskrba z električno energijo, plinom in paro") +
  ylab("Višina plače(€)") +
  xlab("Leto")

#Oskrba z vodo, ravnanje z odplakami in odpadki, saniranje okolja
gospodarskadejavnostE_osmanj_moski <- gospodarskadejavnost %>% 
  filter(izobrazba == "Osnovnošolska ali manj", spol == "Moški", 
         gospodarska.dejavnost == "E OSKRBA Z VODO, RAVNANJE Z ODPLAKAMI IN ODPADKI, SANIRANJE OKOLJE")
gospodarskadejavnostE_osmanj_zenske <- gospodarskadejavnost %>% 
  filter(izobrazba == "Osnovnošolska ali manj", spol == "Ženske", 
         gospodarska.dejavnost == "E OSKRBA Z VODO, RAVNANJE Z ODPLAKAMI IN ODPADKI, SANIRANJE OKOLJE")
gospodarskadejavnostE_sr_moski <- gospodarskadejavnost %>% 
  filter(izobrazba == "Srednješolska", spol == "Moški", 
         gospodarska.dejavnost == "E OSKRBA Z VODO, RAVNANJE Z ODPLAKAMI IN ODPADKI, SANIRANJE OKOLJE")
gospodarskadejavnostE_sr_zenske <- gospodarskadejavnost %>% 
  filter(izobrazba == "Srednješolska", spol == "Ženski", 
         gospodarska.dejavnost == "E OSKRBA Z VODO, RAVNANJE Z ODPLAKAMI IN ODPADKI, SANIRANJE OKOLJE")
gospodarskadejavnostE_vs_moski <- gospodarskadejavnost %>% 
  filter(izobrazba == "Višješolska, visokošolska", spol == "Moški", 
         gospodarska.dejavnost == "E OSKRBA Z VODO, RAVNANJE Z ODPLAKAMI IN ODPADKI, SANIRANJE OKOLJE")
gospodarskadejavnostE_vs_zenske <- gospodarskadejavnost %>% 
  filter(izobrazba == "Višješolska, visokošolska", spol == "Ženske", 
         gospodarska.dejavnost == "E OSKRBA Z VODO, RAVNANJE Z ODPLAKAMI IN ODPADKI, SANIRANJE OKOLJE")

ggplot(gospodarskadejavnostE_osmanj_moski, aes(x= leto,y= placa)) + 
  geom_point(color="blue", size=2) +
  geom_point(data=gospodarskadejavnostE_sr_moski, aes(x= leto,y= placa), color="green", size=2) + 
  geom_point(data=gospodarskadejavnostE_vs_moski, aes(x= leto,y= placa), color="black", size=2) + 
  geom_point(data=gospodarskadejavnostE_osmanj_zenske, aes(x= leto,y= placa), color="purple", size=2) +
  geom_point(data=gospodarskadejavnostE_sr_zenske, aes(x= leto,y= placa), color="yellow", size=2) +
  geom_point(data=gospodarskadejavnostE_vs_zenske, aes(x= leto,y= placa), color="red", size=2) +
  labs(title="Primerjava plače v panogi oskrba z vodo, ravnanje z odplakami in odpadki, saniranje okolja") +
  ylab("Višina plače(€)") +
  xlab("Leto")

#Gradbeništvo
gospodarskadejavnostF_osmanj_moski <- gospodarskadejavnost %>% 
  filter(izobrazba == "Osnovnošolska ali manj", spol == "Moški", 
         gospodarska.dejavnost == " F GRADBENIŠTVO")
gospodarskadejavnostF_osmanj_zenske <- gospodarskadejavnost %>% 
  filter(izobrazba == "Osnovnošolska ali manj", spol == "Ženske", 
         gospodarska.dejavnost == " F GRADBENIŠTVO")
gospodarskadejavnostF_sr_moski <- gospodarskadejavnost %>% 
  filter(izobrazba == "Srednješolska", spol == "Moški", 
         gospodarska.dejavnost == " F GRADBENIŠTVO")
gospodarskadejavnostF_sr_zenske <- gospodarskadejavnost %>% 
  filter(izobrazba == "Srednješolska", spol == "Ženski", 
         gospodarska.dejavnost == " F GRADBENIŠTVO")
gospodarskadejavnostF_vs_moski <- gospodarskadejavnost %>% 
  filter(izobrazba == "Višješolska, visokošolska", spol == "Moški", 
         gospodarska.dejavnost == " F GRADBENIŠTVO")
gospodarskadejavnostF_vs_zenske <- gospodarskadejavnost %>% 
  filter(izobrazba == "Višješolska, visokošolska", spol == "Ženske", 
         gospodarska.dejavnost == " F GRADBENIŠTVO")

ggplot(gospodarskadejavnostF_osmanj_moski, aes(x= leto,y= placa)) + 
  geom_point(color="blue", size=2) +
  geom_point(data=gospodarskadejavnostF_sr_moski, aes(x= leto,y= placa), color="green", size=2) + 
  geom_point(data=gospodarskadejavnostF_vs_moski, aes(x= leto,y= placa), color="black", size=2) + 
  geom_point(data=gospodarskadejavnostF_osmanj_zenske, aes(x= leto,y= placa), color="purple", size=2) +
  geom_point(data=gospodarskadejavnostF_sr_zenske, aes(x= leto,y= placa), color="yellow", size=2) +
  geom_point(data=gospodarskadejavnostF_vs_zenske, aes(x= leto,y= placa), color="red", size=2) +
  labs(title="Primerjava plače v panogi gradbeništvo") +
  ylab("Višina plače(€)") +
  xlab("Leto")

#Trgovina, vzdrževanje in popravilo motronih vozil
gospodarskadejavnostG_osmanj_moski <- gospodarskadejavnost %>% 
  filter(izobrazba == "Osnovnošolska ali manj", spol == "Moški", 
         gospodarska.dejavnost == "G TRGOVINA, VZDRŽEVANJE IN POPRAVILA MOTORNIH VOZIL")
gospodarskadejavnostG_osmanj_zenske <- gospodarskadejavnost %>% 
  filter(izobrazba == "Osnovnošolska ali manj", spol == "Ženske", 
         gospodarska.dejavnost == "G TRGOVINA, VZDRŽEVANJE IN POPRAVILA MOTORNIH VOZIL")
gospodarskadejavnostG_sr_moski <- gospodarskadejavnost %>% 
  filter(izobrazba == "Srednješolska", spol == "Moški", 
         gospodarska.dejavnost == "G TRGOVINA, VZDRŽEVANJE IN POPRAVILA MOTORNIH VOZIL")
gospodarskadejavnostG_sr_zenske <- gospodarskadejavnost %>% 
  filter(izobrazba == "Srednješolska", spol == "Ženski", 
         gospodarska.dejavnost == "G TRGOVINA, VZDRŽEVANJE IN POPRAVILA MOTORNIH VOZIL")
gospodarskadejavnostG_vs_moski <- gospodarskadejavnost %>% 
  filter(izobrazba == "Višješolska, visokošolska", spol == "Moški", 
         gospodarska.dejavnost == "G TRGOVINA, VZDRŽEVANJE IN POPRAVILA MOTORNIH VOZIL")
gospodarskadejavnostG_vs_zenske <- gospodarskadejavnost %>% 
  filter(izobrazba == "Višješolska, visokošolska", spol == "Ženske", 
         gospodarska.dejavnost == "G TRGOVINA, VZDRŽEVANJE IN POPRAVILA MOTORNIH VOZIL")

ggplot(gospodarskadejavnostG_osmanj_moski, aes(x= leto,y= placa)) + 
  geom_point(color="blue", size=2) +
  geom_point(data=gospodarskadejavnostG_sr_moski, aes(x= leto,y= placa), color="green", size=2) + 
  geom_point(data=gospodarskadejavnostG_vs_moski, aes(x= leto,y= placa), color="black", size=2) + 
  geom_point(data=gospodarskadejavnostG_osmanj_zenske, aes(x= leto,y= placa), color="purple", size=2) +
  geom_point(data=gospodarskadejavnostG_sr_zenske, aes(x= leto,y= placa), color="yellow", size=2) +
  geom_point(data=gospodarskadejavnostG_vs_zenske, aes(x= leto,y= placa), color="red", size=2) +
  labs(title="Primerjava plače v panogi trgovina, vzdrževanje in popravilo motronih vozil") +
  ylab("Višina plače(€)") +
  xlab("Leto")

#Promet in skladiščenje
gospodarskadejavnostH_osmanj_moski <- gospodarskadejavnost %>% 
  filter(izobrazba == "Osnovnošolska ali manj", spol == "Moški", 
         gospodarska.dejavnost == "H PROMET IN SKLADIŠČENJE")
gospodarskadejavnostH_osmanj_zenske <- gospodarskadejavnost %>% 
  filter(izobrazba == "Osnovnošolska ali manj", spol == "Ženske", 
         gospodarska.dejavnost == "H PROMET IN SKLADIŠČENJE")
gospodarskadejavnostH_sr_moski <- gospodarskadejavnost %>% 
  filter(izobrazba == "Srednješolska", spol == "Moški", 
         gospodarska.dejavnost == "H PROMET IN SKLADIŠČENJE")
gospodarskadejavnostH_sr_zenske <- gospodarskadejavnost %>% 
  filter(izobrazba == "Srednješolska", spol == "Ženski", 
         gospodarska.dejavnost == "H PROMET IN SKLADIŠČENJE")
gospodarskadejavnostH_vs_moski <- gospodarskadejavnost %>% 
  filter(izobrazba == "Višješolska, visokošolska", spol == "Moški", 
         gospodarska.dejavnost == "H PROMET IN SKLADIŠČENJE")
gospodarskadejavnostH_vs_zenske <- gospodarskadejavnost %>% 
  filter(izobrazba == "Višješolska, visokošolska", spol == "Ženske", 
         gospodarska.dejavnost == "H PROMET IN SKLADIŠČENJE")

ggplot(gospodarskadejavnostH_osmanj_moski, aes(x= leto,y= placa)) + 
  geom_point(color="blue", size=2) +
  geom_point(data=gospodarskadejavnostH_sr_moski, aes(x= leto,y= placa), color="green", size=2) + 
  geom_point(data=gospodarskadejavnostH_vs_moski, aes(x= leto,y= placa), color="black", size=2) + 
  geom_point(data=gospodarskadejavnostH_osmanj_zenske, aes(x= leto,y= placa), color="purple", size=2) +
  geom_point(data=gospodarskadejavnostH_sr_zenske, aes(x= leto,y= placa), color="yellow", size=2) +
  geom_point(data=gospodarskadejavnostH_vs_zenske, aes(x= leto,y= placa), color="red", size=2) +
  labs(title="Primerjava plače v panogi promet in skladiščenje") +
  ylab("Višina plače(€)") +
  xlab("Leto")

#Gostinstvo
gospodarskadejavnostI_osmanj_moski <- gospodarskadejavnost %>% 
  filter(izobrazba == "Osnovnošolska ali manj", spol == "Moški", 
         gospodarska.dejavnost == "I GOSTINSTVO")
gospodarskadejavnostI_osmanj_zenske <- gospodarskadejavnost %>% 
  filter(izobrazba == "Osnovnošolska ali manj", spol == "Ženske", 
         gospodarska.dejavnost == "I GOSTINSTVO")
gospodarskadejavnostI_sr_moski <- gospodarskadejavnost %>% 
  filter(izobrazba == "Srednješolska", spol == "Moški", 
         gospodarska.dejavnost == "I GOSTINSTVO")
gospodarskadejavnostI_sr_zenske <- gospodarskadejavnost %>% 
  filter(izobrazba == "Srednješolska", spol == "Ženski", 
         gospodarska.dejavnost == "I GOSTINSTVO")
gospodarskadejavnostI_vs_moski <- gospodarskadejavnost %>% 
  filter(izobrazba == "Višješolska, visokošolska", spol == "Moški", 
         gospodarska.dejavnost == "I GOSTINSTVO")
gospodarskadejavnostI_vs_zenske <- gospodarskadejavnost %>% 
  filter(izobrazba == "Višješolska, visokošolska", spol == "Ženske", 
         gospodarska.dejavnost == "I GOSTINSTVO")

ggplot(gospodarskadejavnostI_osmanj_moski, aes(x= leto,y= placa)) + 
  geom_point(color="blue", size=2) +
  geom_point(data=gospodarskadejavnostI_sr_moski, aes(x= leto,y= placa), color="green", size=2) + 
  geom_point(data=gospodarskadejavnostI_vs_moski, aes(x= leto,y= placa), color="black", size=2) + 
  geom_point(data=gospodarskadejavnostI_osmanj_zenske, aes(x= leto,y= placa), color="purple", size=2) +
  geom_point(data=gospodarskadejavnostI_sr_zenske, aes(x= leto,y= placa), color="yellow", size=2) +
  geom_point(data=gospodarskadejavnostI_vs_zenske, aes(x= leto,y= placa), color="red", size=2) +
  labs(title="Primerjava plače v panogi gostinstvo") +
  ylab("Višina plače(€)") +
  xlab("Leto")

#Informacijske in komunikacijske dejavnosti
gospodarskadejavnostJ_osmanj_moski <- gospodarskadejavnost %>% 
  filter(izobrazba == "Osnovnošolska ali manj", spol == "Moški", 
         gospodarska.dejavnost == "J INFORMACIJSKE IN KOMUNIKACIJSKE DEJAVNOSTI")
gospodarskadejavnostJ_osmanj_zenske <- gospodarskadejavnost %>% 
  filter(izobrazba == "Osnovnošolska ali manj", spol == "Ženske", 
         gospodarska.dejavnost == "J INFORMACIJSKE IN KOMUNIKACIJSKE DEJAVNOSTI")
gospodarskadejavnostJ_sr_moski <- gospodarskadejavnost %>% 
  filter(izobrazba == "Srednješolska", spol == "Moški", 
         gospodarska.dejavnost == "J INFORMACIJSKE IN KOMUNIKACIJSKE DEJAVNOSTI")
gospodarskadejavnostJ_sr_zenske <- gospodarskadejavnost %>% 
  filter(izobrazba == "Srednješolska", spol == "Ženski", 
         gospodarska.dejavnost == "J INFORMACIJSKE IN KOMUNIKACIJSKE DEJAVNOSTI")
gospodarskadejavnostJ_vs_moski <- gospodarskadejavnost %>% 
  filter(izobrazba == "Višješolska, visokošolska", spol == "Moški", 
         gospodarska.dejavnost == "J INFORMACIJSKE IN KOMUNIKACIJSKE DEJAVNOSTI")
gospodarskadejavnostJ_vs_zenske <- gospodarskadejavnost %>% 
  filter(izobrazba == "Višješolska, visokošolska", spol == "Ženske", 
         gospodarska.dejavnost == "J INFORMACIJSKE IN KOMUNIKACIJSKE DEJAVNOSTI")

ggplot(gospodarskadejavnostJ_osmanj_moski, aes(x= leto,y= placa)) + 
  geom_point(color="blue", size=2) +
  geom_point(data=gospodarskadejavnostJ_sr_moski, aes(x= leto,y= placa), color="green", size=2) + 
  geom_point(data=gospodarskadejavnostJ_vs_moski, aes(x= leto,y= placa), color="black", size=2) + 
  geom_point(data=gospodarskadejavnostJ_osmanj_zenske, aes(x= leto,y= placa), color="purple", size=2) +
  geom_point(data=gospodarskadejavnostJ_sr_zenske, aes(x= leto,y= placa), color="yellow", size=2) +
  geom_point(data=gospodarskadejavnostJ_vs_zenske, aes(x= leto,y= placa), color="red", size=2) +
  labs(title="Primerjava plače v panogi informacijske in komunikacijske dejavnosti") +
  ylab("Višina plače(€)") +
  xlab("Leto")

#Finančne in zavarovalniške dejavnosti
gospodarskadejavnostK_osmanj_moski <- gospodarskadejavnost %>% 
  filter(izobrazba == "Osnovnošolska ali manj", spol == "Moški", 
         gospodarska.dejavnost == "K FINANČNE IN ZAVAROVALNIŠKE DEJAVNOSTI")
gospodarskadejavnostK_osmanj_zenske <- gospodarskadejavnost %>% 
  filter(izobrazba == "Osnovnošolska ali manj", spol == "Ženske", 
         gospodarska.dejavnost == "K FINANČNE IN ZAVAROVALNIŠKE DEJAVNOSTI")
gospodarskadejavnostK_sr_moski <- gospodarskadejavnost %>% 
  filter(izobrazba == "Srednješolska", spol == "Moški", 
         gospodarska.dejavnost == "K FINANČNE IN ZAVAROVALNIŠKE DEJAVNOSTI")
gospodarskadejavnostK_sr_zenske <- gospodarskadejavnost %>% 
  filter(izobrazba == "Srednješolska", spol == "Ženski", 
         gospodarska.dejavnost == "K FINANČNE IN ZAVAROVALNIŠKE DEJAVNOSTI")
gospodarskadejavnostK_vs_moski <- gospodarskadejavnost %>% 
  filter(izobrazba == "Višješolska, visokošolska", spol == "Moški", 
         gospodarska.dejavnost == "K FINANČNE IN ZAVAROVALNIŠKE DEJAVNOSTI")
gospodarskadejavnostK_vs_zenske <- gospodarskadejavnost %>% 
  filter(izobrazba == "Višješolska, visokošolska", spol == "Ženske", 
         gospodarska.dejavnost == "K FINANČNE IN ZAVAROVALNIŠKE DEJAVNOSTI")

ggplot(gospodarskadejavnostK_osmanj_moski, aes(x= leto,y= placa)) + 
  geom_point(color="blue", size=2) +
  geom_point(data=gospodarskadejavnostK_sr_moski, aes(x= leto,y= placa), color="green", size=2) + 
  geom_point(data=gospodarskadejavnostK_vs_moski, aes(x= leto,y= placa), color="black", size=2) + 
  geom_point(data=gospodarskadejavnostK_osmanj_zenske, aes(x= leto,y= placa), color="purple", size=2) +
  geom_point(data=gospodarskadejavnostK_sr_zenske, aes(x= leto,y= placa), color="yellow", size=2) +
  geom_point(data=gospodarskadejavnostK_vs_zenske, aes(x= leto,y= placa), color="red", size=2) +
  labs(title="Primerjava plače v panogi finančne in zavarovalniške dejavnosti") +
  ylab("Višina plače(€)") +
  xlab("Leto")

#Poslovanje z nepremičninami
gospodarskadejavnostL_osmanj_moski <- gospodarskadejavnost %>% 
  filter(izobrazba == "Osnovnošolska ali manj", spol == "Moški", 
         gospodarska.dejavnost == "L POSLOVANJE Z NEPREMIČNINAMI")
gospodarskadejavnostL_osmanj_zenske <- gospodarskadejavnost %>% 
  filter(izobrazba == "Osnovnošolska ali manj", spol == "Ženske", 
         gospodarska.dejavnost == "L POSLOVANJE Z NEPREMIČNINAMI")
gospodarskadejavnostL_sr_moski <- gospodarskadejavnost %>% 
  filter(izobrazba == "Srednješolska", spol == "Moški", 
         gospodarska.dejavnost == "L POSLOVANJE Z NEPREMIČNINAMI")
gospodarskadejavnostL_sr_zenske <- gospodarskadejavnost %>% 
  filter(izobrazba == "Srednješolska", spol == "Ženski", 
         gospodarska.dejavnost == "L POSLOVANJE Z NEPREMIČNINAMI")
gospodarskadejavnostL_vs_moski <- gospodarskadejavnost %>% 
  filter(izobrazba == "Višješolska, visokošolska", spol == "Moški", 
         gospodarska.dejavnost == "L POSLOVANJE Z NEPREMIČNINAMI")
gospodarskadejavnostL_vs_zenske <- gospodarskadejavnost %>% 
  filter(izobrazba == "Višješolska, visokošolska", spol == "Ženske", 
         gospodarska.dejavnost == "L POSLOVANJE Z NEPREMIČNINAMI")

ggplot(gospodarskadejavnostL_osmanj_moski, aes(x= leto,y= placa)) + 
  geom_point(color="blue", size=2) +
  geom_point(data=gospodarskadejavnostL_sr_moski, aes(x= leto,y= placa), color="green", size=2) + 
  geom_point(data=gospodarskadejavnostL_vs_moski, aes(x= leto,y= placa), color="black", size=2) + 
  geom_point(data=gospodarskadejavnostL_osmanj_zenske, aes(x= leto,y= placa), color="purple", size=2) +
  geom_point(data=gospodarskadejavnostL_sr_zenske, aes(x= leto,y= placa), color="yellow", size=2) +
  geom_point(data=gospodarskadejavnostL_vs_zenske, aes(x= leto,y= placa), color="red", size=2) +
  labs(title="Primerjava plače v panogi poslovanje z nepremičninami") +
  ylab("Višina plače(€)") +
  xlab("Leto")

#Strokovne, znanstvene in tehnične dejavnosti
gospodarskadejavnostM_osmanj_moski <- gospodarskadejavnost %>% 
  filter(izobrazba == "Osnovnošolska ali manj", spol == "Moški", 
         gospodarska.dejavnost == "M STROKOVNE, ZNANSTVENE IN TEHNIČNE DEJAVNOSTI")
gospodarskadejavnostM_osmanj_zenske <- gospodarskadejavnost %>% 
  filter(izobrazba == "Osnovnošolska ali manj", spol == "Ženske", 
         gospodarska.dejavnost == "M STROKOVNE, ZNANSTVENE IN TEHNIČNE DEJAVNOSTI")
gospodarskadejavnostM_sr_moski <- gospodarskadejavnost %>% 
  filter(izobrazba == "Srednješolska", spol == "Moški", 
         gospodarska.dejavnost == "M STROKOVNE, ZNANSTVENE IN TEHNIČNE DEJAVNOSTI")
gospodarskadejavnostM_sr_zenske <- gospodarskadejavnost %>% 
  filter(izobrazba == "Srednješolska", spol == "Ženski", 
         gospodarska.dejavnost == "M STROKOVNE, ZNANSTVENE IN TEHNIČNE DEJAVNOSTI")
gospodarskadejavnostM_vs_moski <- gospodarskadejavnost %>% 
  filter(izobrazba == "Višješolska, visokošolska", spol == "Moški", 
         gospodarska.dejavnost == "M STROKOVNE, ZNANSTVENE IN TEHNIČNE DEJAVNOSTI")
gospodarskadejavnostM_vs_zenske <- gospodarskadejavnost %>% 
  filter(izobrazba == "Višješolska, visokošolska", spol == "Ženske", 
         gospodarska.dejavnost == "M STROKOVNE, ZNANSTVENE IN TEHNIČNE DEJAVNOSTI")

ggplot(gospodarskadejavnostM_osmanj_moski, aes(x= leto,y= placa)) + 
  geom_point(color="blue", size=2) +
  geom_point(data=gospodarskadejavnostM_sr_moski, aes(x= leto,y= placa), color="green", size=2) + 
  geom_point(data=gospodarskadejavnostM_vs_moski, aes(x= leto,y= placa), color="black", size=2) + 
  geom_point(data=gospodarskadejavnostM_osmanj_zenske, aes(x= leto,y= placa), color="purple", size=2) +
  geom_point(data=gospodarskadejavnostM_sr_zenske, aes(x= leto,y= placa), color="yellow", size=2) +
  geom_point(data=gospodarskadejavnostM_vs_zenske, aes(x= leto,y= placa), color="red", size=2) +
  labs(title="Primerjava plače v panogi strokovne, znanstvene in tehnične dejavnosti") +
  ylab("Višina plače(€)") +
  xlab("Leto")

#Druge raznovrstne poslovne dejavnosti
gospodarskadejavnostN_osmanj_moski <- gospodarskadejavnost %>% 
  filter(izobrazba == "Osnovnošolska ali manj", spol == "Moški", 
         gospodarska.dejavnost == "N DRUGE RAZNOVRSTNE POSLOVNE DEJAVNOSTI")
gospodarskadejavnostN_osmanj_zenske <- gospodarskadejavnost %>% 
  filter(izobrazba == "Osnovnošolska ali manj", spol == "Ženske", 
         gospodarska.dejavnost == "N DRUGE RAZNOVRSTNE POSLOVNE DEJAVNOSTI")
gospodarskadejavnostN_sr_moski <- gospodarskadejavnost %>% 
  filter(izobrazba == "Srednješolska", spol == "Moški", 
         gospodarska.dejavnost == "N DRUGE RAZNOVRSTNE POSLOVNE DEJAVNOSTI")
gospodarskadejavnostN_sr_zenske <- gospodarskadejavnost %>% 
  filter(izobrazba == "Srednješolska", spol == "Ženski", 
         gospodarska.dejavnost == "N DRUGE RAZNOVRSTNE POSLOVNE DEJAVNOSTI")
gospodarskadejavnostN_vs_moski <- gospodarskadejavnost %>% 
  filter(izobrazba == "Višješolska, visokošolska", spol == "Moški", 
         gospodarska.dejavnost == "N DRUGE RAZNOVRSTNE POSLOVNE DEJAVNOSTI")
gospodarskadejavnostN_vs_zenske <- gospodarskadejavnost %>% 
  filter(izobrazba == "Višješolska, visokošolska", spol == "Ženske", 
         gospodarska.dejavnost == "N DRUGE RAZNOVRSTNE POSLOVNE DEJAVNOSTI")

ggplot(gospodarskadejavnostN_osmanj_moski, aes(x= leto,y= placa)) + 
  geom_point(color="blue", size=2) +
  geom_point(data=gospodarskadejavnostN_sr_moski, aes(x= leto,y= placa), color="green", size=2) + 
  geom_point(data=gospodarskadejavnostN_vs_moski, aes(x= leto,y= placa), color="black", size=2) + 
  geom_point(data=gospodarskadejavnostN_osmanj_zenske, aes(x= leto,y= placa), color="purple", size=2) +
  geom_point(data=gospodarskadejavnostN_sr_zenske, aes(x= leto,y= placa), color="yellow", size=2) +
  geom_point(data=gospodarskadejavnostN_vs_zenske, aes(x= leto,y= placa), color="red", size=2) +
  labs(title="Primerjava plače v panogi druge raznovrstne poslovne dejavnosti") +
  ylab("Višina plače(€)") +
  xlab("Leto")

#Dejavnosti javne uprave in obrambe, dejavnosti obvezne socialne varnosti
gospodarskadejavnostO_osmanj_moski <- gospodarskadejavnost %>% 
  filter(izobrazba == "Osnovnošolska ali manj", spol == "Moški", 
         gospodarska.dejavnost == "O DEJAVNOST JAVNE UPRAVE IN OBRAMBE, DEJAVNOST OBVEZNE SOCIALNE VARNOSTI")
gospodarskadejavnostO_osmanj_zenske <- gospodarskadejavnost %>% 
  filter(izobrazba == "Osnovnošolska ali manj", spol == "Ženske", 
         gospodarska.dejavnost == "O DEJAVNOST JAVNE UPRAVE IN OBRAMBE, DEJAVNOST OBVEZNE SOCIALNE VARNOSTI")
gospodarskadejavnostO_sr_moski <- gospodarskadejavnost %>% 
  filter(izobrazba == "Srednješolska", spol == "Moški", 
         gospodarska.dejavnost == "O DEJAVNOST JAVNE UPRAVE IN OBRAMBE, DEJAVNOST OBVEZNE SOCIALNE VARNOSTI")
gospodarskadejavnostO_sr_zenske <- gospodarskadejavnost %>% 
  filter(izobrazba == "Srednješolska", spol == "Ženski", 
         gospodarska.dejavnost == "O DEJAVNOST JAVNE UPRAVE IN OBRAMBE, DEJAVNOST OBVEZNE SOCIALNE VARNOSTI")
gospodarskadejavnostO_vs_moski <- gospodarskadejavnost %>% 
  filter(izobrazba == "Višješolska, visokošolska", spol == "Moški", 
         gospodarska.dejavnost == "O DEJAVNOST JAVNE UPRAVE IN OBRAMBE, DEJAVNOST OBVEZNE SOCIALNE VARNOSTI")
gospodarskadejavnostO_vs_zenske <- gospodarskadejavnost %>% 
  filter(izobrazba == "Višješolska, visokošolska", spol == "Ženske", 
         gospodarska.dejavnost == "O DEJAVNOST JAVNE UPRAVE IN OBRAMBE, DEJAVNOST OBVEZNE SOCIALNE VARNOSTI")

ggplot(gospodarskadejavnostO_osmanj_moski, aes(x= leto,y= placa)) + 
  geom_point(color="blue", size=2) +
  geom_point(data=gospodarskadejavnostO_sr_moski, aes(x= leto,y= placa), color="green", size=2) + 
  geom_point(data=gospodarskadejavnostO_vs_moski, aes(x= leto,y= placa), color="black", size=2) + 
  geom_point(data=gospodarskadejavnostO_osmanj_zenske, aes(x= leto,y= placa), color="purple", size=2) +
  geom_point(data=gospodarskadejavnostO_sr_zenske, aes(x= leto,y= placa), color="yellow", size=2) +
  geom_point(data=gospodarskadejavnostO_vs_zenske, aes(x= leto,y= placa), color="red", size=2) +
  labs(title="Primerjava plače v panogi dejavnosti javne uprave in obrambe, dejavnosti obvezne socialne varnosti") +
  ylab("Višina plače(€)") +
  xlab("Leto")

#Izobraževanje
gospodarskadejavnostP_osmanj_moski <- gospodarskadejavnost %>% 
  filter(izobrazba == "Osnovnošolska ali manj", spol == "Moški", 
         gospodarska.dejavnost == "P IZOBRAŽEVANJE")
gospodarskadejavnostP_osmanj_zenske <- gospodarskadejavnost %>% 
  filter(izobrazba == "Osnovnošolska ali manj", spol == "Ženske", 
         gospodarska.dejavnost == "P IZOBRAŽEVANJE")
gospodarskadejavnostP_sr_moski <- gospodarskadejavnost %>% 
  filter(izobrazba == "Srednješolska", spol == "Moški", 
         gospodarska.dejavnost == "P IZOBRAŽEVANJE")
gospodarskadejavnostP_sr_zenske <- gospodarskadejavnost %>% 
  filter(izobrazba == "Srednješolska", spol == "Ženski", 
         gospodarska.dejavnost == "P IZOBRAŽEVANJE")
gospodarskadejavnostP_vs_moski <- gospodarskadejavnost %>% 
  filter(izobrazba == "Višješolska, visokošolska", spol == "Moški", 
         gospodarska.dejavnost == "P IZOBRAŽEVANJE")
gospodarskadejavnostP_vs_zenske <- gospodarskadejavnost %>% 
  filter(izobrazba == "Višješolska, visokošolska", spol == "Ženske", 
         gospodarska.dejavnost == "P IZOBRAŽEVANJE")


ggplot(gospodarskadejavnostP_osmanj_moski, aes(x= leto,y= placa)) + 
  geom_point(color="blue", size=2) +
  geom_point(data=gospodarskadejavnostP_sr_moski, aes(x= leto,y= placa), color="green", size=2) + 
  geom_point(data=gospodarskadejavnostP_vs_moski, aes(x= leto,y= placa), color="black", size=2) + 
  geom_point(data=gospodarskadejavnostP_osmanj_zenske, aes(x= leto,y= placa), color="purple", size=2) +
  geom_point(data=gospodarskadejavnostP_sr_zenske, aes(x= leto,y= placa), color="yellow", size=2) +
  geom_point(data=gospodarskadejavnostP_vs_zenske, aes(x= leto,y= placa), color="red", size=2) +
  labs(title="Primerjava plače v panogi izobraževanje") +
  ylab("Višina plače(€)") +
  xlab("Leto")

#Zdravstvo in socialno varstvo
gospodarskadejavnostQ_osmanj_moski <- gospodarskadejavnost %>% 
  filter(izobrazba == "Osnovnošolska ali manj", spol == "Moški", 
         gospodarska.dejavnost == "Q ZDRAVSTVO IN SOCIALNO VARSTVO")
gospodarskadejavnostQ_osmanj_zenske <- gospodarskadejavnost %>% 
  filter(izobrazba == "Osnovnošolska ali manj", spol == "Ženske", 
         gospodarska.dejavnost == "Q ZDRAVSTVO IN SOCIALNO VARSTVO")
gospodarskadejavnostQ_sr_moski <- gospodarskadejavnost %>% 
  filter(izobrazba == "Srednješolska", spol == "Moški", 
         gospodarska.dejavnost == "Q ZDRAVSTVO IN SOCIALNO VARSTVO")
gospodarskadejavnostQ_sr_zenske <- gospodarskadejavnost %>% 
  filter(izobrazba == "Srednješolska", spol == "Ženski", 
         gospodarska.dejavnost == "Q ZDRAVSTVO IN SOCIALNO VARSTVO")
gospodarskadejavnostQ_vs_moski <- gospodarskadejavnost %>% 
  filter(izobrazba == "Višješolska, visokošolska", spol == "Moški", 
         gospodarska.dejavnost == "Q ZDRAVSTVO IN SOCIALNO VARSTVO")
gospodarskadejavnostQ_vs_zenske <- gospodarskadejavnost %>% 
  filter(izobrazba == "Višješolska, visokošolska", spol == "Ženske", 
         gospodarska.dejavnost == "Q ZDRAVSTVO IN SOCIALNO VARSTVO")

ggplot(gospodarskadejavnostQ_osmanj_moski, aes(x= leto,y= placa)) + 
  geom_point(color="blue", size=2) +
  geom_point(data=gospodarskadejavnostQ_sr_moski, aes(x= leto,y= placa), color="green", size=2) + 
  geom_point(data=gospodarskadejavnostQ_vs_moski, aes(x= leto,y= placa), color="black", size=2) + 
  geom_point(data=gospodarskadejavnostQ_osmanj_zenske, aes(x= leto,y= placa), color="purple", size=2) +
  geom_point(data=gospodarskadejavnostQ_sr_zenske, aes(x= leto,y= placa), color="yellow", size=2) +
  geom_point(data=gospodarskadejavnostQ_vs_zenske, aes(x= leto,y= placa), color="red", size=2) +
  labs(title="Primerjava plače v panogi zdravstvo in socialno varstvo") +
  ylab("Višina plače(€)") +
  xlab("Leto")

#Kulturne, razvedrilne in rekreacijske dejavnosti
gospodarskadejavnostR_osmanj_moski <- gospodarskadejavnost %>% 
  filter(izobrazba == "Osnovnošolska ali manj", spol == "Moški", 
         gospodarska.dejavnost == "R KULTURNE, RAZVEDRILNE IN REKREACIJSKE DEJAVNOSTI")
gospodarskadejavnostR_osmanj_zenske <- gospodarskadejavnost %>% 
  filter(izobrazba == "Osnovnošolska ali manj", spol == "Ženske", 
         gospodarska.dejavnost == "R KULTURNE, RAZVEDRILNE IN REKREACIJSKE DEJAVNOSTI")
gospodarskadejavnostR_sr_moski <- gospodarskadejavnost %>% 
  filter(izobrazba == "Srednješolska", spol == "Moški", 
         gospodarska.dejavnost == "R KULTURNE, RAZVEDRILNE IN REKREACIJSKE DEJAVNOSTI")
gospodarskadejavnostR_sr_zenske <- gospodarskadejavnost %>% 
  filter(izobrazba == "Srednješolska", spol == "Ženski", 
         gospodarska.dejavnost == "R KULTURNE, RAZVEDRILNE IN REKREACIJSKE DEJAVNOSTI")
gospodarskadejavnostR_vs_moski <- gospodarskadejavnost %>% 
  filter(izobrazba == "Višješolska, visokošolska", spol == "Moški", 
         gospodarska.dejavnost == "R KULTURNE, RAZVEDRILNE IN REKREACIJSKE DEJAVNOSTI")
gospodarskadejavnostR_vs_zenske <- gospodarskadejavnost %>% 
  filter(izobrazba == "Višješolska, visokošolska", spol == "Ženske", 
         gospodarska.dejavnost == "R KULTURNE, RAZVEDRILNE IN REKREACIJSKE DEJAVNOSTI")

ggplot(gospodarskadejavnostR_osmanj_moski, aes(x= leto,y= placa)) + 
  geom_point(color="blue", size=2) +
  geom_point(data=gospodarskadejavnostR_sr_moski, aes(x= leto,y= placa), color="green", size=2) + 
  geom_point(data=gospodarskadejavnostR_vs_moski, aes(x= leto,y= placa), color="black", size=2) + 
  geom_point(data=gospodarskadejavnostR_osmanj_zenske, aes(x= leto,y= placa), color="purple", size=2) +
  geom_point(data=gospodarskadejavnostR_sr_zenske, aes(x= leto,y= placa), color="yellow", size=2) +
  geom_point(data=gospodarskadejavnostR_vs_zenske, aes(x= leto,y= placa), color="red", size=2) +
  labs(title="Primerjava plače v panogi kulturne, razvedrilne in rekreacijske dejavnosti") +
  ylab("Višina plače(€)") +
  xlab("Leto")

#Druge dejavnosti
gospodarskadejavnostS_osmanj_moski <- gospodarskadejavnost %>% 
  filter(izobrazba == "Osnovnošolska ali manj", spol == "Moški", 
         gospodarska.dejavnost == "S DRUGE DEJAVNOSTI")
gospodarskadejavnostS_osmanj_zenske <- gospodarskadejavnost %>% 
  filter(izobrazba == "Osnovnošolska ali manj", spol == "Ženske", 
         gospodarska.dejavnost == "S DRUGE DEJAVNOSTI")
gospodarskadejavnostS_sr_moski <- gospodarskadejavnost %>% 
  filter(izobrazba == "Srednješolska", spol == "Moški", 
         gospodarska.dejavnost == "S DRUGE DEJAVNOSTI")
gospodarskadejavnostS_sr_zenske <- gospodarskadejavnost %>% 
  filter(izobrazba == "Srednješolska", spol == "Ženski", 
         gospodarska.dejavnost == "S DRUGE DEJAVNOSTI")
gospodarskadejavnostS_vs_moski <- gospodarskadejavnost %>% 
  filter(izobrazba == "Višješolska, visokošolska", spol == "Moški", 
         gospodarska.dejavnost == "S DRUGE DEJAVNOSTI")
gospodarskadejavnostS_vs_zenske <- gospodarskadejavnost %>% 
  filter(izobrazba == "Višješolska, visokošolska", spol == "Ženske", 
         gospodarska.dejavnost == "S DRUGE DEJAVNOSTI")

ggplot(gospodarskadejavnostS_osmanj_moski, aes(x= leto,y= placa)) + 
  geom_point(color="blue", size=2) +
  geom_point(data=gospodarskadejavnostS_sr_moski, aes(x= leto,y= placa), color="green", size=2) + 
  geom_point(data=gospodarskadejavnostS_vs_moski, aes(x= leto,y= placa), color="black", size=2) + 
  geom_point(data=gospodarskadejavnostS_osmanj_zenske, aes(x= leto,y= placa), color="purple", size=2) +
  geom_point(data=gospodarskadejavnostS_sr_zenske, aes(x= leto,y= placa), color="yellow", size=2) +
  geom_point(data=gospodarskadejavnostS_vs_zenske, aes(x= leto,y= placa), color="red", size=2) +
  labs(title="Primerjava plače v panogi druge dejavnosti") +
  ylab("Višina plače(€)") +
  xlab("Leto")

 #Najvišja in najnižja plača v vsaki panogi glede na izobrazbo in spol
gospodarska_dejavnost <- c("Kmetijstvo in lov, gozdarstvo, ribištvo","Kmetijstvo in lov, gozdarstvo, ribištvo","Kmetijstvo in lov, gozdarstvo, ribištvo",
                           "Rudarstvo","Rudarstvo","Rudarstvo",
                           "Predelovalne dejavnosti","Predelovalne dejavnosti","Predelovalne dejavnosti",
                           "Oskrba z električno energijo, plinom in paro","Oskrba z električno energijo, plinom in paro","Oskrba z električno energijo, plinom in paro",
                           "Oskrba z vodo, ravnanje z odplakami in odpadki, saniranje okolja", "Oskrba z vodo, ravnanje z odplakami in odpadki, saniranje okolja", "Oskrba z vodo, ravnanje z odplakami in odpadki, saniranje okolja",
                           "Gradbeništvo","Gradbeništvo","Gradbeništvo",
                           "Trgovina, vzdrževanje in popravilo motronih vozil","Trgovina, vzdrževanje in popravilo motronih vozil", "Trgovina, vzdrževanje in popravilo motronih vozil",
                           "Promet in skladiščenje", "Promet in skladiščenje", "Promet in skladiščenje",
                           "Gostinstvo", "Gostinstvo","Gostinstvo",
                           "Informacijske in komunikacijske dejavnosti","Informacijske in komunikacijske dejavnosti","Informacijske in komunikacijske dejavnosti",
                           "Finančne in zavarovalniške dejavnosti","Finančne in zavarovalniške dejavnosti","Finančne in zavarovalniške dejavnosti",
                           "Poslovanje z nepremičninami", "Poslovanje z nepremičninami","Poslovanje z nepremičninami",
                           "Strokovne, znanstvene in tehnične dejavnosti", "Strokovne, znanstvene in tehnične dejavnosti", "Strokovne, znanstvene in tehnične dejavnosti",
                           "Druge raznovrstne dejavnosti", "Druge raznovrstne dejavnosti", "Druge raznovrstne dejavnosti",
                           "Dejavnosti javne uprave in obrambe, dejavnosti obvezne socialne varnosti", "Dejavnosti javne uprave in obrambe, dejavnosti obvezne socialne varnosti", "Dejavnosti javne uprave in obrambe, dejavnosti obvezne socialne varnosti",
                           "Izobraževanje", "Izobraževanje", "Izobraževanje",
                           "Zdravstvo in socialno varstvo","Zdravstvo in socialno varstvo","Zdravstvo in socialno varstvo",
                           "Kulturne, razvedrilne in rekreacijske dejavnosti","Kulturne, razvedrilne in rekreacijske dejavnosti","Kulturne, razvedrilne in rekreacijske dejavnosti",
                           "Druge dejavnosti","Druge dejavnosti","Druge dejavnosti")
izobrazba <- c("OSNOVNOŠOLSKA ALI MANJ","SREDNJEŠOLSKA", "VIŠJEŠOLSKA, VISOKOŠOLSKA",
               "OSNOVNOŠOLSKA ALI MANJ","SREDNJEŠOLSKA", "VIŠJEŠOLSKA, VISOKOŠOLSKA",
               "OSNOVNOŠOLSKA ALI MANJ","SREDNJEŠOLSKA", "VIŠJEŠOLSKA, VISOKOŠOLSKA",
               "OSNOVNOŠOLSKA ALI MANJ","SREDNJEŠOLSKA", "VIŠJEŠOLSKA, VISOKOŠOLSKA",
               "OSNOVNOŠOLSKA ALI MANJ","SREDNJEŠOLSKA", "VIŠJEŠOLSKA, VISOKOŠOLSKA",
               "OSNOVNOŠOLSKA ALI MANJ","SREDNJEŠOLSKA", "VIŠJEŠOLSKA, VISOKOŠOLSKA",
               "OSNOVNOŠOLSKA ALI MANJ","SREDNJEŠOLSKA", "VIŠJEŠOLSKA, VISOKOŠOLSKA",
               "OSNOVNOŠOLSKA ALI MANJ","SREDNJEŠOLSKA", "VIŠJEŠOLSKA, VISOKOŠOLSKA",
               "OSNOVNOŠOLSKA ALI MANJ","SREDNJEŠOLSKA", "VIŠJEŠOLSKA, VISOKOŠOLSKA",
               "OSNOVNOŠOLSKA ALI MANJ","SREDNJEŠOLSKA", "VIŠJEŠOLSKA, VISOKOŠOLSKA",
               "OSNOVNOŠOLSKA ALI MANJ","SREDNJEŠOLSKA", "VIŠJEŠOLSKA, VISOKOŠOLSKA",
               "OSNOVNOŠOLSKA ALI MANJ","SREDNJEŠOLSKA", "VIŠJEŠOLSKA, VISOKOŠOLSKA",
               "OSNOVNOŠOLSKA ALI MANJ","SREDNJEŠOLSKA", "VIŠJEŠOLSKA, VISOKOŠOLSKA",
               "OSNOVNOŠOLSKA ALI MANJ","SREDNJEŠOLSKA", "VIŠJEŠOLSKA, VISOKOŠOLSKA",
               "OSNOVNOŠOLSKA ALI MANJ","SREDNJEŠOLSKA", "VIŠJEŠOLSKA, VISOKOŠOLSKA",
               "OSNOVNOŠOLSKA ALI MANJ","SREDNJEŠOLSKA", "VIŠJEŠOLSKA, VISOKOŠOLSKA",
               "OSNOVNOŠOLSKA ALI MANJ","SREDNJEŠOLSKA", "VIŠJEŠOLSKA, VISOKOŠOLSKA",
               "OSNOVNOŠOLSKA ALI MANJ","SREDNJEŠOLSKA", "VIŠJEŠOLSKA, VISOKOŠOLSKA",
               "OSNOVNOŠOLSKA ALI MANJ","SREDNJEŠOLSKA", "VIŠJEŠOLSKA, VISOKOŠOLSKA")
maksimalna_placa_moski <- c(max(gospodarskadejavnostA_osmanj_moski$placa),
               max(gospodarskadejavnostA_sr_moski$placa),
               max(gospodarskadejavnostA_vs_moski$placa),
               max(gospodarskadejavnostB_osmanj_moski$placa),
               max(gospodarskadejavnostB_sr_moski$placa),
               max(gospodarskadejavnostB_vs_moski$placa),
               max(gospodarskadejavnostC_osmanj_moski$placa),
               max(gospodarskadejavnostC_sr_moski$placa),
               max(gospodarskadejavnostC_vs_moski$placa),
               max(gospodarskadejavnostD_osmanj_moski$placa),
               max(gospodarskadejavnostD_sr_moski$placa),
               max(gospodarskadejavnostD_vs_moski$placa),
               max(gospodarskadejavnostE_osmanj_moski$placa),
               max(gospodarskadejavnostE_sr_moski$placa),
               max(gospodarskadejavnostE_vs_moski$placa),
               max(gospodarskadejavnostF_osmanj_moski$placa),
               max(gospodarskadejavnostF_sr_moski$placa),
               max(gospodarskadejavnostF_vs_moski$placa),
               max(gospodarskadejavnostG_osmanj_moski$placa),
               max(gospodarskadejavnostG_sr_moski$placa),
               max(gospodarskadejavnostG_vs_moski$placa),
               max(gospodarskadejavnostH_osmanj_moski$placa),
               max(gospodarskadejavnostH_sr_moski$placa),
               max(gospodarskadejavnostH_vs_moski$placa),
               max(gospodarskadejavnostI_osmanj_moski$placa),
               max(gospodarskadejavnostI_sr_moski$placa),
               max(gospodarskadejavnostI_vs_moski$placa),
               max(gospodarskadejavnostJ_osmanj_moski$placa),
               max(gospodarskadejavnostJ_sr_moski$placa),
               max(gospodarskadejavnostJ_vs_moski$placa),
               max(gospodarskadejavnostK_osmanj_moski$placa),
               max(gospodarskadejavnostK_sr_moski$placa),
               max(gospodarskadejavnostK_vs_moski$placa),
               max(gospodarskadejavnostL_osmanj_moski$placa),
               max(gospodarskadejavnostL_sr_moski$placa),
               max(gospodarskadejavnostL_vs_moski$placa),
               max(gospodarskadejavnostM_osmanj_moski$placa),
               max(gospodarskadejavnostM_sr_moski$placa),
               max(gospodarskadejavnostM_vs_moski$placa),
               max(gospodarskadejavnostN_osmanj_moski$placa),
               max(gospodarskadejavnostN_sr_moski$placa),
               max(gospodarskadejavnostN_vs_moski$placa),
               max(gospodarskadejavnostO_osmanj_moski$placa),
               max(gospodarskadejavnostO_sr_moski$placa),
               max(gospodarskadejavnostO_vs_moski$placa),
               max(gospodarskadejavnostP_osmanj_moski$placa),
               max(gospodarskadejavnostP_sr_moski$placa),
               max(gospodarskadejavnostP_vs_moski$placa),
               max(gospodarskadejavnostQ_osmanj_moski$placa),
               max(gospodarskadejavnostQ_sr_moski$placa),
               max(gospodarskadejavnostQ_vs_moski$placa),
               max(gospodarskadejavnostR_osmanj_moski$placa),
               max(gospodarskadejavnostR_sr_moski$placa),
               max(gospodarskadejavnostR_vs_moski$placa),
               max(gospodarskadejavnostS_osmanj_moski$placa),
               max(gospodarskadejavnostS_sr_moski$placa),
               max(gospodarskadejavnostS_vs_moski$placa))
minimalna_placa_moski <- c(min(gospodarskadejavnostA_osmanj_moski$placa),
                           min(gospodarskadejavnostA_sr_moski$placa),
                           min(gospodarskadejavnostA_vs_moski$placa),
                           min(gospodarskadejavnostB_osmanj_moski$placa),
                           min(gospodarskadejavnostB_sr_moski$placa),
                           min(gospodarskadejavnostB_vs_moski$placa),
                           min(gospodarskadejavnostC_osmanj_moski$placa),
                           min(gospodarskadejavnostC_sr_moski$placa),
                           min(gospodarskadejavnostC_vs_moski$placa),
                           min(gospodarskadejavnostD_osmanj_moski$placa),
                           min(gospodarskadejavnostD_sr_moski$placa),
                           min(gospodarskadejavnostD_vs_moski$placa),
                           min(gospodarskadejavnostE_osmanj_moski$placa),
                           min(gospodarskadejavnostE_sr_moski$placa),
                           min(gospodarskadejavnostE_vs_moski$placa),
                           min(gospodarskadejavnostF_osmanj_moski$placa),
                           min(gospodarskadejavnostF_sr_moski$placa),
                           min(gospodarskadejavnostF_vs_moski$placa),
                           min(gospodarskadejavnostG_osmanj_moski$placa),
                           min(gospodarskadejavnostG_sr_moski$placa),
                           min(gospodarskadejavnostG_vs_moski$placa),
                           min(gospodarskadejavnostH_osmanj_moski$placa),
                           min(gospodarskadejavnostH_sr_moski$placa),
                           min(gospodarskadejavnostH_vs_moski$placa),
                           min(gospodarskadejavnostI_osmanj_moski$placa),
                           min(gospodarskadejavnostI_sr_moski$placa),
                           min(gospodarskadejavnostI_vs_moski$placa),
                           min(gospodarskadejavnostJ_osmanj_moski$placa),
                           min(gospodarskadejavnostJ_sr_moski$placa),
                           min(gospodarskadejavnostJ_vs_moski$placa),
                           min(gospodarskadejavnostK_osmanj_moski$placa),
                           min(gospodarskadejavnostK_sr_moski$placa),
                           min(gospodarskadejavnostK_vs_moski$placa),
                           min(gospodarskadejavnostL_osmanj_moski$placa),
                           min(gospodarskadejavnostL_sr_moski$placa),
                           min(gospodarskadejavnostL_vs_moski$placa),
                           min(gospodarskadejavnostM_osmanj_moski$placa),
                           min(gospodarskadejavnostM_sr_moski$placa),
                           min(gospodarskadejavnostM_vs_moski$placa),
                           min(gospodarskadejavnostN_osmanj_moski$placa),
                           min(gospodarskadejavnostN_sr_moski$placa),
                           min(gospodarskadejavnostN_vs_moski$placa),
                           min(gospodarskadejavnostO_osmanj_moski$placa),
                           min(gospodarskadejavnostO_sr_moski$placa),
                           min(gospodarskadejavnostO_vs_moski$placa),
                           min(gospodarskadejavnostP_osmanj_moski$placa),
                           min(gospodarskadejavnostP_sr_moski$placa),
                           min(gospodarskadejavnostP_vs_moski$placa),
                           min(gospodarskadejavnostQ_osmanj_moski$placa),
                           min(gospodarskadejavnostQ_sr_moski$placa),
                           min(gospodarskadejavnostQ_vs_moski$placa),
                           min(gospodarskadejavnostR_osmanj_moski$placa),
                           min(gospodarskadejavnostR_sr_moski$placa),
                           min(gospodarskadejavnostR_vs_moski$placa),
                           min(gospodarskadejavnostS_osmanj_moski$placa),
                           min(gospodarskadejavnostS_sr_moski$placa),
                           min(gospodarskadejavnostS_vs_moski$placa))
maksimalna_placa_zenske <- c(max(gospodarskadejavnostA_osmanj_zenske$placa),
                             max(gospodarskadejavnostA_sr_zenske$placa),
                             max(gospodarskadejavnostA_vs_zenske$placa),
                             max(gospodarskadejavnostB_osmanj_zenske$placa),
                             max(gospodarskadejavnostB_sr_zenske$placa),
                             max(gospodarskadejavnostB_vs_zenske$placa),
                             max(gospodarskadejavnostC_osmanj_zenske$placa),
                             max(gospodarskadejavnostC_sr_zenske$placa),
                             max(gospodarskadejavnostC_vs_zenske$placa),
                             max(gospodarskadejavnostD_osmanj_zenske$placa),
                             max(gospodarskadejavnostD_sr_zenske$placa),
                             max(gospodarskadejavnostD_vs_zenske$placa),
                             max(gospodarskadejavnostE_osmanj_zenske$placa),
                             max(gospodarskadejavnostE_sr_zenske$placa),
                             max(gospodarskadejavnostE_vs_zenske$placa),
                             max(gospodarskadejavnostF_osmanj_zenske$placa),
                             max(gospodarskadejavnostF_sr_zenske$placa),
                             max(gospodarskadejavnostF_vs_zenske$placa),
                             max(gospodarskadejavnostG_osmanj_zenske$placa),
                             max(gospodarskadejavnostG_sr_zenske$placa),
                             max(gospodarskadejavnostG_vs_zenske$placa),
                             max(gospodarskadejavnostH_osmanj_zenske$placa),
                             max(gospodarskadejavnostH_sr_zenske$placa),
                             max(gospodarskadejavnostH_vs_zenske$placa),
                             max(gospodarskadejavnostI_osmanj_zenske$placa),
                             max(gospodarskadejavnostI_sr_zenske$placa),
                             max(gospodarskadejavnostI_vs_zenske$placa),
                             max(gospodarskadejavnostJ_osmanj_zenske$placa),
                             max(gospodarskadejavnostJ_sr_zenske$placa),
                             max(gospodarskadejavnostJ_vs_zenske$placa),
                             max(gospodarskadejavnostK_osmanj_zenske$placa),
                             max(gospodarskadejavnostK_sr_zenske$placa),
                             max(gospodarskadejavnostK_vs_zenske$placa),
                             max(gospodarskadejavnostL_osmanj_zenske$placa),
                             max(gospodarskadejavnostL_sr_zenske$placa),
                             max(gospodarskadejavnostL_vs_zenske$placa),
                             max(gospodarskadejavnostM_osmanj_zenske$placa),
                             max(gospodarskadejavnostM_sr_zenske$placa),
                             max(gospodarskadejavnostM_vs_zenske$placa),
                             max(gospodarskadejavnostN_osmanj_zenske$placa),
                             max(gospodarskadejavnostN_sr_zenske$placa),
                             max(gospodarskadejavnostN_vs_zenske$placa),
                             max(gospodarskadejavnostO_osmanj_zenske$placa),
                             max(gospodarskadejavnostO_sr_zenske$placa),
                             max(gospodarskadejavnostO_vs_zenske$placa),
                             max(gospodarskadejavnostP_osmanj_zenske$placa),
                             max(gospodarskadejavnostP_sr_zenske$placa),
                             max(gospodarskadejavnostP_vs_zenske$placa),
                             max(gospodarskadejavnostQ_osmanj_zenske$placa),
                             max(gospodarskadejavnostQ_sr_zenske$placa),
                             max(gospodarskadejavnostQ_vs_zenske$placa),
                             max(gospodarskadejavnostR_osmanj_zenske$placa),
                             max(gospodarskadejavnostR_sr_zenske$placa),
                             max(gospodarskadejavnostR_vs_zenske$placa),
                             max(gospodarskadejavnostS_osmanj_zenske$placa),
                             max(gospodarskadejavnostS_sr_zenske$placa),
                             max(gospodarskadejavnostS_vs_zenske$placa))
minimalna_placa_zenske <- c(min(gospodarskadejavnostA_osmanj_zenske$placa),
                            min(gospodarskadejavnostA_sr_zenske$placa),
                            min(gospodarskadejavnostA_vs_zenske$placa),
                            min(gospodarskadejavnostB_osmanj_zenske$placa),
                            min(gospodarskadejavnostB_sr_zenske$placa),
                            min(gospodarskadejavnostB_vs_zenske$placa),
                            min(gospodarskadejavnostC_osmanj_zenske$placa),
                            min(gospodarskadejavnostC_sr_zenske$placa),
                            min(gospodarskadejavnostC_vs_zenske$placa),
                            min(gospodarskadejavnostD_osmanj_zenske$placa),
                            min(gospodarskadejavnostD_sr_zenske$placa),
                            min(gospodarskadejavnostD_vs_zenske$placa),
                            min(gospodarskadejavnostE_osmanj_zenske$placa),
                            min(gospodarskadejavnostE_sr_zenske$placa),
                            min(gospodarskadejavnostE_vs_zenske$placa),
                            min(gospodarskadejavnostF_osmanj_zenske$placa),
                            min(gospodarskadejavnostF_sr_zenske$placa),
                            min(gospodarskadejavnostF_vs_zenske$placa),
                            min(gospodarskadejavnostG_osmanj_zenske$placa),
                            min(gospodarskadejavnostG_sr_zenske$placa),
                            min(gospodarskadejavnostG_vs_zenske$placa),
                            min(gospodarskadejavnostH_osmanj_zenske$placa),
                            min(gospodarskadejavnostH_sr_zenske$placa),
                            min(gospodarskadejavnostH_vs_zenske$placa),
                            min(gospodarskadejavnostI_osmanj_zenske$placa),
                            min(gospodarskadejavnostI_sr_zenske$placa),
                            min(gospodarskadejavnostI_vs_zenske$placa),
                            min(gospodarskadejavnostJ_osmanj_zenske$placa),
                            min(gospodarskadejavnostJ_sr_zenske$placa),
                            min(gospodarskadejavnostJ_vs_zenske$placa),
                            min(gospodarskadejavnostK_osmanj_zenske$placa),
                            min(gospodarskadejavnostK_sr_zenske$placa),
                            min(gospodarskadejavnostK_vs_zenske$placa),
                            min(gospodarskadejavnostL_osmanj_zenske$placa),
                            min(gospodarskadejavnostL_sr_zenske$placa),
                            min(gospodarskadejavnostL_vs_zenske$placa),
                            min(gospodarskadejavnostM_osmanj_zenske$placa),
                            min(gospodarskadejavnostM_sr_zenske$placa),
                            min(gospodarskadejavnostM_vs_zenske$placa),
                            min(gospodarskadejavnostN_osmanj_zenske$placa),
                            min(gospodarskadejavnostN_sr_zenske$placa),
                            min(gospodarskadejavnostN_vs_zenske$placa),
                            min(gospodarskadejavnostO_osmanj_zenske$placa),
                            min(gospodarskadejavnostO_sr_zenske$placa),
                            min(gospodarskadejavnostO_vs_zenske$placa),
                            min(gospodarskadejavnostP_osmanj_zenske$placa),
                            min(gospodarskadejavnostP_sr_zenske$placa),
                            min(gospodarskadejavnostP_vs_zenske$placa),
                            min(gospodarskadejavnostQ_osmanj_zenske$placa),
                            min(gospodarskadejavnostQ_sr_zenske$placa),
                            min(gospodarskadejavnostQ_vs_zenske$placa),
                            min(gospodarskadejavnostR_osmanj_zenske$placa),
                            min(gospodarskadejavnostR_sr_zenske$placa),
                            min(gospodarskadejavnostR_vs_zenske$placa),
                            min(gospodarskadejavnostS_osmanj_zenske$placa),
                            min(gospodarskadejavnostS_sr_zenske$placa),
                            min(gospodarskadejavnostS_vs_zenske$placa))
maksimalna_minimalna <- data.frame(gospodarska_dejavnost, izobrazba, 
                                   maksimalna_placa_moski, minimalna_placa_moski, maksimalna_placa_zenske, minimalna_placa_zenske)

#Analiza plač glede na javni in zasebni sektor
javnisektor_osmanj_moski <- javnisektor[c(1:9), ]
javnisektor_osmanj_zenske <- javnisektor[c(10:18), ]
javnisektor_sr_moski <- javnisektor[c(19:27), ]
javnisektor_sr_zenske <- javnisektor[c(28:36), ]
javnisektor_vs_moski <- javnisektor[c(37:45), ]
javnisektor_vs_zenske <- javnisektor[c(46:54), ]
zasebnisektor_osmanj_moski <- javnisektor[c(55:62), ]
zasebnisektor_osmanj_zenske <- javnisektor[c(63:71), ]
zasebnisektor_sr_moski <- javnisektor[c(72:80), ]
zasebnisektor_sr_zenske <- javnisektor[c(81:89), ]
zasebnisektor_vs_moski <- javnisektor[c(90:98), ]
zasebnisektor_vs_zenske <- javnisektor[c(99:108), ]

  #Graf-javni sektor
ggplot(javnisektor_osmanj_moski, aes(x=leto, y=placa)) +
  geom_point(color="blue", size=2) +
  geom_point(data=javnisektor_osmanj_zenske, aes(x=leto, y=placa),color="light blue", size=2) +
  geom_point(data=javnisektor_sr_moski, aes(x=leto, y=placa),color="red", size=2) +
  geom_point(data=javnisektor_sr_zenske, aes(x=leto, y=placa),color="pink", size=2) +
  geom_point(data=javnisektor_vs_moski, aes(x=leto, y=placa),color="green", size=2) +
  geom_point(data=javnisektor_vs_zenske, aes(x=leto, y=placa),color="dark green", size=2) +
  labs(title="Primerjava plače v javnem sektorju glede na izobrazbo in spol") +
  ylab("Višina plače(€)") +
  xlab("Leto")

  #Graf-zasebni sektor
ggplot(zasebnisektor_osmanj_moski, aes(x=leto, y=placa)) +
  geom_point(color="blue", size=2) +
  geom_point(data=zasebnisektor_osmanj_zenske, aes(x=leto, y=placa),color="light blue", size=2) +
  geom_point(data=zasebnisektor_sr_moski, aes(x=leto, y=placa),color="red", size=2) +
  geom_point(data=zasebnisektor_sr_zenske, aes(x=leto, y=placa),color="pink", size=2) +
  geom_point(data=zasebnisektor_vs_moski, aes(x=leto, y=placa),color="green", size=2) +
  geom_point(data=zasebnisektor_vs_zenske, aes(x=leto, y=placa),color="dark green", size=2) +
  labs(title="Primerjava plače v zasebnem sektorju glede na izobrazbo in spol") +
  ylab("Višina plače(€)") +
  xlab("Leto")

  #Graf-analiza povprečne plače po letih v javnem in zasebnem sektorju
javnisektor_osmanj <- javnisektor_spolskupaj[c(1:9), ]
zasebnisektor_osmanj <- javnisektor_spolskupaj[c(28:36), ]
javnisektor_sr <- javnisektor_spolskupaj[c(10:18), ]
zasebnisektor_sr <- javnisektor_spolskupaj[c(37:45), ]
javnisektor_vs <- javnisektor_spolskupaj[c(19:27), ]
zasebnisektor_vs <- javnisektor_spolskupaj[c(46:54), ]

ggplot(javnisektor_osmanj, aes(x=leto, y=placa)) + 
  geom_point(color="dark blue", size=2) +
  geom_point(data=zasebnisektor_osmanj, aes(x=leto, y=placa),color="light blue", size=2) +
  geom_point(data=javnisektor_sr, aes(x=leto, y=placa),color="red", size=2) +
  geom_point(data=zasebnisektor_sr, aes(x=leto, y=placa),color="light pink", size=2) +
  geom_point(data=javnisektor_vs, aes(x=leto, y=placa),color="dark green", size=2) +
  geom_point(data=zasebnisektor_vs, aes(x=leto, y=placa),color="green", size=2) +
  labs(title="Primerjava plače v zasebnem sektorju glede na izobrazbo") +
  ylab("Višina plače(€)") +
  xlab("Leto")

izobrazba <- c("OSNOVNOŠOLSKA ALI MANJ", "SREDNJEŠOLSKA", "VIŠJEŠOLSKA/VISOKOŠOSLKA",
               "OSNOVNOŠOLSKA ALI MANJ", "SREDNJEŠOLSKA", "VIŠJEŠOLSKA/VISOKOŠOSLKA")
vrsta_place <- c("MAX PLAČA", "MAX PLAČA", "MAX PLAČA",
                 "MIN PLAČA", "MIN PLAČA", "MIN PLAČA")
placa <- c(max(javnisektor_osmanj_moski$placa), max(javnisektor_sr_moski$placa), max(javnisektor_vs_moski$placa), 
           min(javnisektor_osmanj_moski$placa),min(javnisektor_sr_moski$placa), min(javnisektor_vs_moski$placa))
placa_javnisektor_moski <- data.frame(izobrazba, vrsta_place, placa)

placa <- c(max(zasebnisektor_osmanj_moski$placa),max(zasebnisektor_sr_moski$placa), max(zasebnisektor_vs_moski$placa),
               min(zasebnisektor_osmanj_moski$placa),min(zasebnisektor_sr_moski$placa), min(zasebnisektor_vs_moski$placa))
placa_zasebnisektor_moski <- data.frame(izobrazba, vrsta_place, placa)

placa <- c(max(zasebnisektor_osmanj_zenske$placa),max(zasebnisektor_sr_zenske$placa), max(zasebnisektor_vs_zenske$placa),
               min(zasebnisektor_osmanj_zenske$placa),min(zasebnisektor_sr_zenske$placa), min(zasebnisektor_vs_zenske$placa))
placa_javnisektor_zenske <- data.frame(izobrazba, vrsta_place, placa)

placa <- c(max(zasebnisektor_osmanj_zenske$placa),max(zasebnisektor_sr_zenske$placa), max(zasebnisektor_vs_zenske$placa),
               min(zasebnisektor_osmanj_zenske$placa),min(zasebnisektor_sr_zenske$placa), min(zasebnisektor_vs_zenske$placa))
placa_zasebnisektor_zenske <- data.frame(izobrazba, vrsta_place, placa)

  #Graf primerjave minimalne in maksimalne place glede na izobrazbo
ggplot(placa_javnisektor_moski, aes(x=izobrazba, y=placa)) +
  geom_point(color="dark blue", size=2) +
  geom_point(placa_zasebnisektor_moski, mapping=aes(x=izobrazba, y=placa), color="dark green", size=2) +
  geom_point(placa_javnisektor_zenske, mapping=aes(x=izobrazba, y=placa), color="red", size=2) +
  geom_point(placa_zasebnisektor_zenske, mapping=aes(x=izobrazba, y=placa), color="pink", size=2) +
  labs(title="Primerjava minimalne in maksimalne plače v zasebnem in javnem sektorju") +
  ylab("Višina plače(€)") +
  xlab("Izobrazba")
  
#Krizi(2008,2020)
meseci <- c("12", "11","10","09","08","07","06","05","04","03","02","01",
            "12", "11","10","09","08","07","06","05","04","03","02","01",
            "12", "11","10","09","08","07","06","05","04","03","02","01")
kriza2008_meseci <- data.frame(meseci, kriza2008$placa)
kriza_leto2009 <- kriza2008_meseci[c(1:12), ]
kriza_leto2008 <- kriza2008_meseci[c(13:24), ]
kriza_leto2007 <- kriza2008_meseci[c(25:36), ]

 #Kriza leta 2008
ggplot(kriza_leto2007, mapping=aes(x=meseci, y=kriza2008.placa)) +
  geom_point(color="red", size=2) +
  geom_point(kriza_leto2008,mapping=aes(x=meseci, y=kriza2008.placa), color="blue", size=2) +
  geom_point(kriza_leto2009,mapping=aes(x=meseci, y=kriza2008.placa), color="yellow", size=2) +
  labs(title="Primerjava plače v letih 2007, 2008, 2009") +
  ylab("Višina plače(€)") +
  xlab("Meseci")
 
  #Primerjava leta 2007 z letom 2019
meseci <- c("09","08","07","06","05","04","03","02","01",
            "12", "11","10","09","08","07","06","05","04","03","02","01")
kriza2020_meseci <- data.frame(meseci, kriza2020$placa)
kriza_leto2019 <- kriza2020_meseci[c(10:21), ]

ggplot(kriza_leto2007, mapping=aes(x=meseci, y=kriza2008.placa)) +
  geom_point(color="red", size=2) +
  geom_point(kriza_leto2019, mapping=aes(x=meseci, y=kriza2020.placa), color="blue", size=2) +
  labs(title="Primerjava plače v letih pred krizo(2007,2019)") +
  ylab("Višina plače(€)") +
  xlab("Meseci")

  #Primerjava leta 2008 in 2020(do meseca 09)
kriza_leto2020 <- kriza2020_meseci[c(1:9), ]

ggplot(kriza_leto2008, mapping=aes(x=meseci, y=kriza2008.placa)) +
  geom_point(color="red", size=2) +
  geom_point(kriza_leto2020, mapping=aes(x=meseci, y=kriza2020.placa), color="blue", size=2) +
  labs(title="Primerjava plače v letih 2008,2020") +
  ylab("Višina plače(€)") +
  xlab("Meseci")
