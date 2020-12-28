# 3. faza: Vizualizacija podatkov

require(ggplot2)
require(dplyr)

#Analiza plač glede na gospodarsko dejavnost, izobrazbo in spol

#Kmetijstvo in lov, gozdrastvo, ribištvo
gospodarskadejavnostA_osmanj_moski <- gospodarskadejavnost[c(1:9), ] 
gospodarskadejavnostA_osmanj_zenske <- gospodarskadejavnost[c(10:18), ]
gospodarskadejavnostA_sr_moski <- gospodarskadejavnost[c(19:27), ]
gospodarskadejavnostA_sr_zenske <- gospodarskadejavnost[c(28:36), ]
gospodarskadejavnostA_vs_moski <- gospodarskadejavnost[c(37:45), ]
gospodarskadejavnostA_vs_zenske <- gospodarskadejavnost[c(46:54), ]

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
gospodarskadejavnostB_osmanj_moski <- gospodarskadejavnost[c(55:63), ] 
gospodarskadejavnostB_osmanj_zenske <- gospodarskadejavnost[c(64:72), ]
gospodarskadejavnostB_sr_moski <- gospodarskadejavnost[c(73:81), ]
gospodarskadejavnostB_sr_zenske <- gospodarskadejavnost[c(82:90), ]
gospodarskadejavnostB_vs_moski <- gospodarskadejavnost[c(91:99), ]
gospodarskadejavnostB_vs_zenske <- gospodarskadejavnost[c(100:108), ]

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
gospodarskadejavnostC_osmanj_moski <- gospodarskadejavnost[c(109:117), ] 
gospodarskadejavnostC_osmanj_zenske <- gospodarskadejavnost[c(118:126), ]
gospodarskadejavnostC_sr_moski <- gospodarskadejavnost[c(127:135), ]
gospodarskadejavnostC_sr_zenske <- gospodarskadejavnost[c(136:144), ]
gospodarskadejavnostC_vs_moski <- gospodarskadejavnost[c(145:153), ]
gospodarskadejavnostC_vs_zenske <- gospodarskadejavnost[c(154:162), ]

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
gospodarskadejavnostD_osmanj_moski <- gospodarskadejavnost[c(163:171), ] 
gospodarskadejavnostD_osmanj_zenske <- gospodarskadejavnost[c(172:180), ]
gospodarskadejavnostD_sr_moski <- gospodarskadejavnost[c(181:189), ]
gospodarskadejavnostD_sr_zenske <- gospodarskadejavnost[c(190:198), ]
gospodarskadejavnostD_vs_moski <- gospodarskadejavnost[c(199:207), ]
gospodarskadejavnostD_vs_zenske <- gospodarskadejavnost[c(208:216), ]

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
gospodarskadejavnostE_osmanj_moski <- gospodarskadejavnost[c(217:225), ] 
gospodarskadejavnostE_osmanj_zenske <- gospodarskadejavnost[c(226:234), ]
gospodarskadejavnostE_sr_moski <- gospodarskadejavnost[c(235:243), ]
gospodarskadejavnostE_sr_zenske <- gospodarskadejavnost[c(244:252), ]
gospodarskadejavnostE_vs_moski <- gospodarskadejavnost[c(253:261), ]
gospodarskadejavnostE_vs_zenske <- gospodarskadejavnost[c(262:270), ]

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
gospodarskadejavnostF_osmanj_moski <- gospodarskadejavnost[c(271:279), ] 
gospodarskadejavnostF_osmanj_zenske <- gospodarskadejavnost[c(280:288), ]
gospodarskadejavnostF_sr_moski <- gospodarskadejavnost[c(289:297), ]
gospodarskadejavnostF_sr_zenske <- gospodarskadejavnost[c(298:306), ]
gospodarskadejavnostF_vs_moski <- gospodarskadejavnost[c(307:315), ]
gospodarskadejavnostF_vs_zenske <- gospodarskadejavnost[c(316:324), ]

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
gospodarskadejavnostG_osmanj_moski <- gospodarskadejavnost[c(325:333), ] 
gospodarskadejavnostG_osmanj_zenske <- gospodarskadejavnost[c(334:342), ]
gospodarskadejavnostG_sr_moski <- gospodarskadejavnost[c(343:351), ]
gospodarskadejavnostG_sr_zenske <- gospodarskadejavnost[c(352:360), ]
gospodarskadejavnostG_vs_moski <- gospodarskadejavnost[c(361:369), ]
gospodarskadejavnostG_vs_zenske <- gospodarskadejavnost[c(370:378), ]

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
gospodarskadejavnostH_osmanj_moski <- gospodarskadejavnost[c(379:387), ] 
gospodarskadejavnostH_osmanj_zenske <- gospodarskadejavnost[c(388:396), ]
gospodarskadejavnostH_sr_moski <- gospodarskadejavnost[c(397:405), ]
gospodarskadejavnostH_sr_zenske <- gospodarskadejavnost[c(406:414), ]
gospodarskadejavnostH_vs_moski <- gospodarskadejavnost[c(415:423), ]
gospodarskadejavnostH_vs_zenske <- gospodarskadejavnost[c(424:432), ]

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
gospodarskadejavnostI_osmanj_moski <- gospodarskadejavnost[c(433:441), ] 
gospodarskadejavnostI_osmanj_zenske <- gospodarskadejavnost[c(442:450), ]
gospodarskadejavnostI_sr_moski <- gospodarskadejavnost[c(451:459), ]
gospodarskadejavnostI_sr_zenske <- gospodarskadejavnost[c(460:468), ]
gospodarskadejavnostI_vs_moski <- gospodarskadejavnost[c(469:477), ]
gospodarskadejavnostI_vs_zenske <- gospodarskadejavnost[c(478:486), ]

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
gospodarskadejavnostJ_osmanj_moski <- gospodarskadejavnost[c(487:495), ] 
gospodarskadejavnostJ_osmanj_zenske <- gospodarskadejavnost[c(496:504), ]
gospodarskadejavnostJ_sr_moski <- gospodarskadejavnost[c(505:513), ]
gospodarskadejavnostJ_sr_zenske <- gospodarskadejavnost[c(514:522), ]
gospodarskadejavnostJ_vs_moski <- gospodarskadejavnost[c(523:531), ]
gospodarskadejavnostJ_vs_zenske <- gospodarskadejavnost[c(532:540), ]

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
gospodarskadejavnostK_osmanj_moski <- gospodarskadejavnost[c(541:549), ] 
gospodarskadejavnostK_osmanj_zenske <- gospodarskadejavnost[c(550:558), ]
gospodarskadejavnostK_sr_moski <- gospodarskadejavnost[c(559:567), ]
gospodarskadejavnostK_sr_zenske <- gospodarskadejavnost[c(568:576), ]
gospodarskadejavnostK_vs_moski <- gospodarskadejavnost[c(577:585), ]
gospodarskadejavnostK_vs_zenske <- gospodarskadejavnost[c(586:594), ]

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
gospodarskadejavnostL_osmanj_moski <- gospodarskadejavnost[c(595:603), ] 
gospodarskadejavnostL_osmanj_zenske <- gospodarskadejavnost[c(604:612), ]
gospodarskadejavnostL_sr_moski <- gospodarskadejavnost[c(613:621), ]
gospodarskadejavnostL_sr_zenske <- gospodarskadejavnost[c(622:630), ]
gospodarskadejavnostL_vs_moski <- gospodarskadejavnost[c(631:639), ]
gospodarskadejavnostL_vs_zenske <- gospodarskadejavnost[c(640:648), ]

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
gospodarskadejavnostM_osmanj_moski <- gospodarskadejavnost[c(649:657), ] 
gospodarskadejavnostM_osmanj_zenske <- gospodarskadejavnost[c(658:666), ]
gospodarskadejavnostM_sr_moski <- gospodarskadejavnost[c(667:675), ]
gospodarskadejavnostM_sr_zenske <- gospodarskadejavnost[c(676:684), ]
gospodarskadejavnostM_vs_moski <- gospodarskadejavnost[c(685:693), ]
gospodarskadejavnostM_vs_zenske <- gospodarskadejavnost[c(694:702), ]

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
gospodarskadejavnostN_osmanj_moski <- gospodarskadejavnost[c(703:711), ] 
gospodarskadejavnostN_osmanj_zenske <- gospodarskadejavnost[c(712:720), ]
gospodarskadejavnostN_sr_moski <- gospodarskadejavnost[c(721:729), ]
gospodarskadejavnostN_sr_zenske <- gospodarskadejavnost[c(730:738), ]
gospodarskadejavnostN_vs_moski <- gospodarskadejavnost[c(739:747), ]
gospodarskadejavnostN_vs_zenske <- gospodarskadejavnost[c(748:756), ]

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
gospodarskadejavnostO_osmanj_moski <- gospodarskadejavnost[c(757:765), ] 
gospodarskadejavnostO_osmanj_zenske <- gospodarskadejavnost[c(766:774), ]
gospodarskadejavnostO_sr_moski <- gospodarskadejavnost[c(775:783), ]
gospodarskadejavnostO_sr_zenske <- gospodarskadejavnost[c(784:792), ]
gospodarskadejavnostO_vs_moski <- gospodarskadejavnost[c(793:801), ]
gospodarskadejavnostO_vs_zenske <- gospodarskadejavnost[c(802:810), ]

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
gospodarskadejavnostP_osmanj_moski <- gospodarskadejavnost[c(811:819), ] 
gospodarskadejavnostP_osmanj_zenske <- gospodarskadejavnost[c(820:828), ]
gospodarskadejavnostP_sr_moski <- gospodarskadejavnost[c(829:837), ]
gospodarskadejavnostP_sr_zenske <- gospodarskadejavnost[c(838:846), ]
gospodarskadejavnostP_vs_moski <- gospodarskadejavnost[c(847:855), ]
gospodarskadejavnostP_vs_zenske <- gospodarskadejavnost[c(856:864), ]

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
gospodarskadejavnostQ_osmanj_moski <- gospodarskadejavnost[c(865:873), ] 
gospodarskadejavnostQ_osmanj_zenske <- gospodarskadejavnost[c(874:882), ]
gospodarskadejavnostQ_sr_moski <- gospodarskadejavnost[c(883:891), ]
gospodarskadejavnostQ_sr_zenske <- gospodarskadejavnost[c(892:900), ]
gospodarskadejavnostQ_vs_moski <- gospodarskadejavnost[c(901:909), ]
gospodarskadejavnostQ_vs_zenske <- gospodarskadejavnost[c(910:918), ]

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
gospodarskadejavnostR_osmanj_moski <- gospodarskadejavnost[c(919:927), ] 
gospodarskadejavnostR_osmanj_zenske <- gospodarskadejavnost[c(928:936), ]
gospodarskadejavnostR_sr_moski <- gospodarskadejavnost[c(937:945), ]
gospodarskadejavnostR_sr_zenske <- gospodarskadejavnost[c(946:954), ]
gospodarskadejavnostR_vs_moski <- gospodarskadejavnost[c(955:963), ]
gospodarskadejavnostR_vs_zenske <- gospodarskadejavnost[c(964:972), ]

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
gospodarskadejavnostS_osmanj_moski <- gospodarskadejavnost[c(973:981), ] 
gospodarskadejavnostS_osmanj_zenske <- gospodarskadejavnost[c(982:990), ]
gospodarskadejavnostS_sr_moski <- gospodarskadejavnost[c(991:999), ]
gospodarskadejavnostS_sr_zenske <- gospodarskadejavnost[c(1000:1008), ]
gospodarskadejavnostS_vs_moski <- gospodarskadejavnost[c(1009:1017), ]
gospodarskadejavnostS_vs_zenske <- gospodarskadejavnost[c(1018:1026), ]

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

