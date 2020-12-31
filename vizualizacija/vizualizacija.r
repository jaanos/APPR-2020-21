# 3. faza: Vizualizacija podatkov

require(ggplot2)
require(dplyr)

#Analiza plač glede na gospodarsko dejavnost, izobrazbo in spol

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
maksimum <- data.frame(gospodarskadejavnost %>% 
  group_by(gospodarska.dejavnost, izobrazba, spol) %>%
  summarise(maksimum = max(placa)))
minimum <- data.frame(gospodarskadejavnost %>% 
  group_by(gospodarska.dejavnost, izobrazba, spol) %>%
  summarise(minimum = min(placa)))
max_min <- merge(maksimum,minimum,by=c("spol", "izobrazba", "gospodarska.dejavnost"))

#Analiza plač glede na javni in zasebni sektor
javnisektor_osmanj_moski <- javnisektor %>%
  filter(izobrazba == "Osnovnošolska ali manj", spol== "Moški",
         sektor== "11 Javni sektor - SKUPAJ")
javnisektor_osmanj_zenske <- javnisektor %>%
  filter(izobrazba == "Osnovnošolska ali manj", spol== "Ženske",
         sektor== "11 Javni sektor - SKUPAJ")
javnisektor_sr_moski <- javnisektor %>%
  filter(izobrazba == "Srednješolska", spol== "Moški",
         sektor== "11 Javni sektor - SKUPAJ")
javnisektor_sr_zenske <- javnisektor %>%
  filter(izobrazba == "Srednješolska", spol== "Ženske",
         sektor== "11 Javni sektor - SKUPAJ")
javnisektor_vs_moski <- javnisektor %>%
  filter(izobrazba == "Višješolska, visokošolska", spol== "Moški",
         sektor== "11 Javni sektor - SKUPAJ")
javnisektor_vs_zenske <- javnisektor %>%
  filter(izobrazba == "Višješolska, visokošolska", spol== "Ženske",
         sektor== "11 Javni sektor - SKUPAJ")
zasebnisektor_osmanj_moski <- javnisektor %>%
  filter(izobrazba == "Osnovnošolska ali manj", spol=="Moški",
         sektor== "12 Zasebni sektor - SKUPAJ")
zasebnisektor_osmanj_zenske <- javnisektor %>%
  filter(izobrazba == "Osnovnošolska ali manj", spol=="Ženske",
         sektor== "12 Zasebni sektor - SKUPAJ")
zasebnisektor_sr_moski <- javnisektor %>%
  filter(izobrazba == "Srednješolska", spol=="Moški",
         sektor== "12 Zasebni sektor - SKUPAJ")
zasebnisektor_sr_zenske <- javnisektor %>%
  filter(izobrazba == "Srednješolska", spol=="Ženske",
         sektor== "12 Zasebni sektor - SKUPAJ")
zasebnisektor_vs_moski <- javnisektor %>%
  filter(izobrazba == "Višješolska, visokošolska", spol=="Moški",
         sektor== "12 Zasebni sektor - SKUPAJ")
zasebnisektor_vs_zenske <-  javnisektor %>%
  filter(izobrazba == "Višješolska, visokošolska", spol=="Ženske",
         sektor== "12 Zasebni sektor - SKUPAJ")

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
javnisektor_osmanj <- javnisektor %>%
  filter(izobrazba == "Osnovnošolska ali manj", sektor== "11 Javni sektor - SKUPAJ")
zasebnisektor_osmanj <- javnisektor %>%
  filter(izobrazba == "Osnovnošolska ali manj", sektor== "12 Zasebni sektor - SKUPAJ")
javnisektor_sr <- javnisektor %>%
  filter(izobrazba == "Srednješolska", sektor== "11 Javni sektor - SKUPAJ")
zasebnisektor_sr <- javnisektor %>%
  filter(izobrazba == "Srednješolska", sektor== "12 Zasebni sektor - SKUPAJ")
javnisektor_vs <- javnisektor %>%
  filter(izobrazba == "Višješolska, visokošolska", sektor== "11 Javni sektor - SKUPAJ")
zasebnisektor_vs <- javnisektor %>%
  filter(izobrazba == "Višješolska, visokošolska", sektor== "12 Zasebni sektor - SKUPAJ")

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

  #Minimalna in maksimalna placa glede na izobrazbo
maksimum <- data.frame(javnisektor %>%
                       group_by(sektor, izobrazba, spol) %>%
                       summarise(maksimum = max(placa)))
minimum <- data.frame(javnisektor %>% 
                      group_by(sektor, izobrazba, spol) %>%
                      summarise(minimum = min(placa)))
max_min <- merge(maksimum,minimum,by=c("spol", "izobrazba", "sektor"))

placa_javnisektor_mosk <- max_min %>%
  filter(sektor=="11 Javni sektor - SKUPAJ", spol== "Moški")
placa_zasebnisektor_moski <- max_min %>%
  filter(sektor=="12 Zasebni sektor - SKUPAJ", spol== "Moški") 
placa_javnisektor_zenske <- max_min %>%
  filter(sektor=="11 Javni sektor - SKUPAJ", spol== "Ženske")
placa_zasebnisektor_zenske <- max_min %>%
  filter(sektor=="12 Zasebni sektor - SKUPAJ", spol== "Ženske")


  #Graf primerjave minimalne in maksimalne place glede na izobrazbo
ggplot(placa_javnisektor_moski, aes(x=izobrazba, y=maksimum)) +
  geom_point(color="dark blue", size=2) +
  geom_point(placa_zasebnisektor_moski, mapping=aes(x=izobrazba, y=maksimum), color="dark green", size=2) +
  geom_point(placa_javnisektor_zenske, mapping=aes(x=izobrazba, y=maksimum), color="red", size=2) +
  geom_point(placa_zasebnisektor_zenske, mapping=aes(x=izobrazba, y=maksimum), color="pink", size=2) +
  geom_point(placa_zasebnisektor_moski, mapping=aes(x=izobrazba, y=minimum), color="black", size=2) +
  geom_point(placa_javnisektor_zenske, mapping=aes(x=izobrazba, y=minimum), color="grey", size=2) +
  geom_point(placa_zasebnisektor_zenske, mapping=aes(x=izobrazba, y=minimum), color="yellow", size=2) +
  geom_point(placa_javnisektor_moski, mapping=aes(x=izobrazba, y=minimum), color="green", size=2)
  labs(title="Primerjava minimalne in maksimalne plače v zasebnem in javnem sektorju") +
  ylab("Višina plače(€)") +
  xlab("Izobrazba")
  
#Krizi(2008,2020)
meseci <- c("12", "11","10","09","08","07","06","05","04","03","02","01",
            "12", "11","10","09","08","07","06","05","04","03","02","01",
            "12", "11","10","09","08","07","06","05","04","03","02","01")
leto <- c("2009","2009","2009","2009","2009","2009","2009","2009","2009","2009","2009","2009",
          "2008","2008","2008","2008","2008","2008","2008","2008","2008","2008","2008","2008",
          "2007","2007","2007","2007","2007","2007","2007","2007","2007","2007","2007","2007")
kriza2008_meseci <- data.frame(leto, meseci, kriza2008$placa)
kriza_leto2009 <- kriza2008_meseci %>%
  filter(leto == "2009")
kriza_leto2008 <- kriza2008_meseci %>%
  filter(leto == "2008")
kriza_leto2007 <- kriza2007_meseci %>%
  filter(leto == "2008")

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
leto <- c("2020","2020","2020","2020","2020","2020","2020","2020","2020",
          "2019","2019","2019","2019","2019","2019","2019","2019","2019","2019","2019","2019")
kriza2020_meseci <- data.frame(leto, meseci, kriza2020$placa)
kriza_leto2019 <- kriza2020_meseci %>%
  filter(leto == "2019")

ggplot(kriza_leto2007, mapping=aes(x=meseci, y=kriza2008.placa)) +
  geom_point(color="red", size=2) +
  geom_point(kriza_leto2019, mapping=aes(x=meseci, y=kriza2020.placa), color="blue", size=2) +
  labs(title="Primerjava plače v letih pred krizo(2007,2019)") +
  ylab("Višina plače(€)") +
  xlab("Meseci")

  #Primerjava leta 2008 in 2020(do meseca 09)
kriza_leto2020 <- kriza2020_meseci %>%
  filter(leto == "2020")

ggplot(kriza_leto2008, mapping=aes(x=meseci, y=kriza2008.placa)) +
  geom_point(color="red", size=2) +
  geom_point(kriza_leto2020, mapping=aes(x=meseci, y=kriza2020.placa), color="blue", size=2) +
  labs(title="Primerjava plače v letih 2008,2020") +
  ylab("Višina plače(€)") +
  xlab("Meseci")
