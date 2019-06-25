data dat;
	input name$ cost;
	datalines;
ChineseTrain 750E9
Paks2 311.3E10
MeszarosLoLo 381.3E9
Csanyi 317.2E9
JeffBezos 3.46888E13
GySoros 1.81184E13
TunnelChaneBridge_planned 23E9
VizesVB 103E9
DunaArena 46E9
Puskas_planned 100E9
Puskas_now 143E9
HospitalRenovation_StImre 6.8E9
HospitalRenovation_Sport 10E9
HospitalRenovation_total 114.5E9
M4Line_planned 120E9
M4Line_final 452E9
M6Highway_2010 384.7E9
FelcsuTrain 1.15E9
AvgProperty 27.1E6
;

dat <- sqrt(dat/dat$AvgProperty) #each pixel is a Ford Mondeo and sqrt is for square side sizes
gap <- 10 #gap between rectangles on the plot

plot <- ggplot(data=dat) + coord_fixed(ratio = 1) +ggtitle("Some hugh figures from Hungary compared to the avarege \n family welth")+ xlab("x27,1 million HUF") + ylab("x27,1 million HUF") + theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))
#total value of hospital restorations since 2010 according to government communication
THx1 <- 0
THx2 <- THx1+dat$HospitalRenovation_total
THy1 <- 0
THy2 <- THy1+dat$HospitalRenovation_total

plot <- plot +geom_rect(mapping = aes(xmin=THx1, xmax=THx2, ymin=THy1, ymax=THy2, fill="Total hospital renovation cost 114.5 billion HUF"), alpha=0.5)

#cost of "Sport Hospital" renovation
SHx1 <- THx1
SHx2 <- dat$HospitalRenovation_Sport
SHy1 <- THy1
SHy2 <- SHy1+dat$HospitalRenovation_Sport

plot <- plot + geom_rect(mapping = aes(xmin=SHx1, xmax=SHx2, ymin=SHy1, ymax=SHy2, fill="Sport Hospital renovation 10 billion HUF"), alpha=0.5)

#cost of St. Imre Hospital renovation
SIHx1 <- SHx2
SIHx2 <- SIHx1+dat$HospitalRenovation_StImre
SIHy1 <- THy1
SIHy2 <- SIHy1+dat$HospitalRenovation_StImre

plot <- plot + geom_rect(mapping = aes(xmin=SIHx1, xmax=SIHx2, ymin=SIHy1, ymax=SIHy2, fill="Szent Imre renovation 6.8 billion HUF"), alpha=0.5)

#current cost of Puskas Football Stadium re-build per Q1 2019
PSx1 <- THx2+gap
PSx2 <- PSx1+dat$Puskas_now
PSy1 <- 0
PSy2 <- PSy1+dat$Puskas_now

plot <- plot + geom_rect(mapping = aes(xmin=PSx1, xmax=PSx2, ymin=PSy1, ymax=PSy2, fill="Puskas Football Stadium current cost - Q1 2019 143 billion HUF"), alpha=0.5)

#originally planned cost of Puskas Football Field re-build
PSOx1 <- PSx1
PSOx2 <- PSOx1+dat$Puskas_planned
PSOy1 <- PSy1
PSOy2 <- PSOy1+dat$Puskas_planned

plot <- plot + geom_rect(mapping = aes(xmin=PSOx1, xmax=PSOx2, ymin=PSOy1, ymax=PSOy2, fill="Puskas Football Stadium original planned cost - 100 billion HUF"), alpha=0.3)

#Cost of Budapest-Beograd railway (Chinese loan)
BBRx1 <- PSx2+gap
BBRx2 <- BBRx1 + dat$ChineseTrain
BBRy1 <- 0
BBRy2 <- BBRy1+dat$ChineseTrain

plot <- plot + geom_rect(mapping = aes(xmin=BBRx1, xmax=BBRx2, ymin=BBRy1, ymax=BBRy2,  fill="Budapest-Beograd railway from Chinese loan 750 billion HUF"), alpha=0.5)

#cost of Paks 2 nuclear powerplant (Russian loan)
P2x1 <- BBRx2+gap
P2x2 <- P2x1 + dat$Paks2
P2y1 <- 0
P2y2 <- P2y1+dat$Paks2

plot <- plot + geom_rect(mapping = aes(xmin=P2x1, xmax=P2x2, ymin=P2y1, ymax=P2y2, fill="Paks 2 nuclear powerplant from russian loan 3113 billion HUF"), alpha=0.5)

#cost of watersport world cup 2017 Budapest
WSx1 <- 0
WSx2 <- WSx1+dat$VizesVB
WSy1 <- THy2+gap
WSy2 <- WSy1+dat$VizesVB

plot <- plot + geom_rect(mapping = aes(xmin=WSx1, xmax=WSx2, ymin=WSy1, ymax=WSy2, fill="Watersports world cup 2017 Budapest 103 billion HUF"), alpha=0.5)

#cost of DUna Arena facility for watersports world cup 2017
DAx1 <- WSx1
DAx2 <- dat$DunaArena
DAy1 <- WSy1
DAy2 <- DAy1+dat$DunaArena

plot <- plot + geom_rect(mapping = aes(xmin=DAx1, xmax=DAx2, ymin=DAy1, ymax=DAy2, fill="Building of Duna Arena 46 billion HUF"), alpha=0.5)

#M6 highway building cost
M6x1 <- 0
M6x2 <- M6x1+dat$M6Highway_2010
M6y1 <- WSy2+gap
M6y2 <- M6y1+dat$M6Highway_2010

plot <- plot + geom_rect(mapping = aes(xmin=M6x1, xmax=M6x2, ymin=M6y1, ymax=M6y2, fill="Building of M6 highway 384.7 billion HUF"), alpha=0.5)

#M4 metro line final cost
M4x1 <- BBRx1
M4x2 <- M4x1+dat$M4Line_final
M4y1 <- BBRy2+gap
M4y2 <- M4y1+dat$M4Line_final

plot <- plot + geom_rect(mapping = aes(xmin=M4x1, xmax=M4x2, ymin=M4y1, ymax=M4y2, fill="Building of M4 metroline - final cost: 452 billion HUF"), alpha=0.5)

#M4 metro line planned original cost
M4Ox1 <- M4x1
M4Ox2 <- M4x1+dat$M4Line_planned
M4Oy1 <- M4y1
M4Oy2 <- M4y1+dat$M4Line_planned

plot <- plot + geom_rect(mapping = aes(xmin=M4Ox1, xmax=M4Ox2, ymin=M4Oy1, ymax=M4Oy2, fill="Building of M4 metroline - planned cost: 120 billion HUF"), alpha=0.5)

#renovation of Bp tunnel and chain bridge
TCx1 <- THx2+gap
TCx2 <- TCx1+dat$TunnelChaneBridge_planned
TCy1 <- PSy2+gap
TCy2 <- TCy1+dat$TunnelChaneBridge_planned

plot <- plot + geom_rect(mapping = aes(xmin=TCx1, xmax=TCx2, ymin=TCy1, ymax=TCy2, fill="Joint renovation of castle hill tunnel and Chain Bridge in Budapest- planned: 23 billion HUF"), alpha=0.7)

#Railway of Vali-valley (Felcsut) - Felcsut railway
FRx1 <- TCx2+gap
FRx2 <- FRx1+dat$FelcsuTrain
FRy1 <- TCy2+gap
FRy2 <- FRy1+dat$FelcsuTrain

plot <- plot + geom_rect(mapping = aes(xmin=FRx1, xmax=FRx2, ymin=FRy1, ymax=FRy2, fill="Vali-valley (Felcsut) railway 1.15 billion HUF"), alpha=0.5)

#Average welth of a Hungarian family
APx1 <- TCx2+gap
APx2 <- APx1+dat$AvgProperty
APy1 <- TCy2
APy2 <- APy1-dat$AvgProperty

plot <- plot + geom_rect(mapping = aes(xmin=APx1, xmax=APx2, ymin=APy1, ymax=APy2, fill="Average welth of a Hungarian family 27.1 million HUF"), alpha=1)

#Estimated value of Lorinc Meszaros' properties
LMx1 <- 0
LMx2 <- LMx1+dat$MeszarosLoLo
LMy1 <- M6y2+gap
LMy2 <- LMy1+dat$MeszarosLoLo

plot <- plot + geom_rect(mapping = aes(xmin=LMx1, xmax=LMx2, ymin=LMy1, ymax=LMy2, fill="Estimated value of Lorinc Meszaros' properties 381.3 billion HUF"), alpha=0.5)

plot <- plot + scale_fill_manual(name="Cost", values = c('lightgreen', 'springgreen', 'springgreen4', 'darkorange', 'aquamarine4', 'red', 'blue', 'gold', 'goldenrod3', 'cornsilk', 'green4', 'deepskyblue3', 'maroon4', 'red', 'green', 'burlywood2'))

plot
