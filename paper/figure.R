setwd("~/Documents/Bordeaux/equipe estuaire/CaN/can/paper/")
library(RCaN)
library(ggpubr)
load("sampleCaN.rdata")
series = ggSeries(SAMPLE, c("DemF","PelF_DemF"), facet=FALSE) + 
  theme_bw() +
  xlab("") +
  scale_color_discrete("")+
  scale_fill_discrete("") +
   theme(legend.position = c(0.23, 0.85),
         legend.background = element_rect(colour = "transparent", fill = "transparent"))+
  theme(axis.title=element_text(size=8),
        axis.text=element_text(size=7))
troph_rel=ggTrophicRelation(SAMPLE,species=c("DemF"))+
  theme(strip.text = element_blank())+
  ylab("Trophic fluxes to DemF") + 
  xlab("Biomass of DemF preys")+
  theme(axis.title=element_text(size=8),
        axis.text=element_text(size=7))
growth=ggGrowth(SAMPLE, "DemF") +
  theme(strip.text.x = element_blank())+
  ylab(expression(DemF~italic(B[t+1]/B[t+1])))+
  xlab("Biomass DemF")+
  theme(axis.title=element_text(size=8),
        axis.text=element_text(size=7),
        title=element_blank())
topdown=ggTopDownBottomUp(SAMPLE,list(DemF=NULL))+
  theme(legend.position = c(0.28, 0.85),
        legend.background = element_rect(colour = "transparent", fill = "transparent"))+
  theme(strip.text.x = element_blank())+
  theme(axis.title=element_text(size=8),
        axis.text=element_text(size=7))


ggarrange(plotlist=list(series,growth,troph_rel,topdown), ncol=2, nrow=2, labels = c("a", "b", "c","d"))
ggsave("figure3.png",width=16/2.54, height=16/2.54,dpi=300)
