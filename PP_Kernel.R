library(ggplot2)
library(scales)
library(grid)

#load and prepare data ----

NoEnso_depth = read.csv("DATA//NoEnso_PP.csv")
ElNino_depth = read.csv("DATA//ElNino_PP.csv")
LaNina_depth = read.csv("DATA//LaNina_PP.csv")

NoEnso_depth$ID = "No ENSO"
NoEnso_depth$style = 'dashed'

ElNino_depth$ID = "El Ni?o"
ElNino_depth$style = 'solid'

LaNina_depth$ID = "La Ni?a"
LaNina_depth$style = 'solid'


mydata = rbind(NoEnso_depth,ElNino_depth,LaNina_depth)

#start plot routine ----

p = ggplot(data=mydata,aes(x = PP_8D,weight=(..count..)/sum(..count..))) +
    geom_line(aes(color=mydata$ID,linetype=mydata$ID ),size=1, stat="density") +
    scale_linetype_manual(breaks=c("El Ni?o","La Ni?a","No ENSO"), values=c(1,1,4))

#start plot customisation ----

#limit y axis
p = p + coord_cartesian(ylim=c(0,0.004)) 

#limit x axis
p = p + coord_cartesian(xlim=c(0,3000)) 

#set x axis division
p = p + scale_x_continuous(breaks = pretty_breaks(n = 6)) 

#remove gray background
p = p + theme_bw()

#set greyscale
p = p + scale_color_grey(start = 0.1, end = .7)

#label axis
p = p + labs(x="PP")
p = p + labs(y="Density")

p = p + theme(axis.title.x = element_text(size=14, face="bold", vjust=-0.1))
p = p + theme(axis.title.y = element_text(size=14, face="bold", vjust=0.9))

p = p + theme(axis.text.x=element_text(size=12, face="bold",vjust=0.5))
p = p + theme(axis.text.y=element_text(size=12, face="bold",vjust=0.5))

#remove legend item border
p = p + theme(legend.key = element_rect(colour = NA))

#set legend icon width
p = p + theme(legend.key.width = unit(2,"cm"))

#set legend line width
p = p + guides(colour = guide_legend(override.aes = list(size=2.5)))

p = p + theme(legend.text=element_text(size=14))

#pick legend position
p = p + theme(legend.position=c(0.85, 0.85),legend.title=element_blank())

#remove x grid
p = p + theme(panel.grid.minor.x=element_blank(),panel.grid.major.x=element_blank())



#draw 
p

#output plot ----

#ggsave(p, file="c:\\users\\grs\\desktop\\Depth_KDE.svg")

p 