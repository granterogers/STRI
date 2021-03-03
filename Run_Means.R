#show current corrected velocities for all turtles and drifter

library(ggplot2)

#read in data -----

AllTurtles = read.csv("DATA//Merged_Velocities_with_Drifters.csv")

#remove green turtles ----
AllTurtles = AllTurtles[- grep("97799", AllTurtles$ID),]
AllTurtles = AllTurtles[- grep("97796", AllTurtles$ID),]
AllTurtles = AllTurtles[- grep("97797", AllTurtles$ID),]
AllTurtles = AllTurtles[- grep("101508", AllTurtles$ID),]

#reformat time column ----
AllTurtles$T.Time=as.numeric(as.POSIXct(as.character(AllTurtles$T.Time),format="%Y-%m-%d %H:%M:%S"))

#calculate means ----
Current_Speed_Mean = aggregate(AllTurtles$P_Mag, list(AllTurtles$ID), mean, na.rm=T)
Travelling_Speed_mean = aggregate(AllTurtles$Final_Mag, list(AllTurtles$ID), mean, na.rm=T)

#transfer to matrix and dataframe ----
MyMatrix = matrix(data=c(Current_Speed_Mean[,2],Travelling_Speed_mean[,2]), byrow=T, ncol=length(unique(AllTurtles$ID)))

MyDF<-as.data.frame(t(MyMatrix))

#calculate standard errors ----
Current_Speed_SE = aggregate(AllTurtles$P_Mag, list(AllTurtles$ID), mean, na.rm=T)[,2]/sqrt(aggregate(AllTurtles$P_Mag, list(AllTurtles$ID), length)[,2])
Travelling_Speed_SE = aggregate(AllTurtles$Final_Mag, list(AllTurtles$ID), mean, na.rm=T)[,2]/sqrt(aggregate(AllTurtles$Final_Mag, list(AllTurtles$ID), length)[,2])

Current_Speed_Mean$se<-Current_Speed_SE
Current_Speed_Mean$response<-"Current Speed"
Travelling_Speed_mean$se<-Travelling_Speed_SE
Travelling_Speed_mean$response<-"Current Corrected Travelling Speed"

#order dataframe by ENSO event ----
neworder = c("D41668","D42579","D71251","D75194","D75197","D75256","D88541","T101503","T101506","T101507",
             "T46207","T46238","T46247","T46248","T46249","T46252","T46256","T46257","T46258","T46259","T46260",
             "T64494","T64495","T64496","T52710","T52711","T97800","T97794","T97795","T97792","T97793","T101502",
             "T101504","T101505")

#ENSO label table ----
ENSO_Labels = c("Drifter","Drifter","Drifter","Drifter","Drifter","Drifter","Drifter","No ENSO","No ENSO","No ENSO",
               "No ENSO","No ENSO","No ENSO","No ENSO","No ENSO","No ENSO","No ENSO","No ENSO","No ENSO","No ENSO","No ENSO",
               "No ENSO","No ENSO","No ENSO","El Niño","El Niño","El Niño","El Niño","El Niño","El Niño","El Niño","La Niña",
               "La Niña","La Niña")

#get rid of turtle ID with a at the end ----
Current_Speed_Mean$Group.1=gsub("a(?=$)","", Current_Speed_Mean$Group.1, perl=TRUE)
Travelling_Speed_mean$Group.1=gsub("a(?=$)","", Travelling_Speed_mean$Group.1, perl=TRUE)

#order data frames for plotting ----
Current_Speed_Mean = Current_Speed_Mean[match(neworder, Current_Speed_Mean$Group.1),]
Travelling_Speed_mean = Travelling_Speed_mean[match(neworder, Travelling_Speed_mean$Group.1),]

row.names(Current_Speed_Mean) <- NULL
row.names(Travelling_Speed_mean) <- NULL

#add ENSO labels 

Current_Speed_Mean$ENSO = ENSO_Labels
Travelling_Speed_mean$ENSO = ENSO_Labels

#final data structure for plotting ----

MyDF = as.data.frame(rbind(Current_Speed_Mean, Travelling_Speed_mean))

#label data frame columns
names(MyDF)[1:2]<-c("ID","mean")

MyDF$response<-as.factor(MyDF$response)

#cleanup unused variables
remove(AllTurtles,MyMatrix,Current_Speed_Mean,Travelling_Speed_SE,Travelling_Speed_mean,Current_Speed_SE,ENSO_Labels,neworder)

#ensures plotting order follows index order

MyDF$ID <- factor(MyDF$ID, as.character(MyDF$ID))

#ensure facet ordering is as desired

MyDF$ENSO = factor(MyDF$ENSO,levels = c("Drifter","No ENSO","El Niño","La Niña"))

#start plot routine ----

p = ggplot(MyDF, aes(fill=response,y=mean,x=ID)) 

#set y axis limit
p = p + coord_cartesian(ylim=c(0,1))

#set color scheme grayscale
p = p + scale_fill_grey(start = .7, end = .1)

#remove gray background
p = p + theme_bw()

#legend text size
p = p + theme(legend.text = element_text(size = 12, face = "bold"))

#pick legend position
p = p + theme(legend.position=c(0.36, 0.96))

#adjust legend symbol size
p = p + guides(fill = guide_legend(keywidth = 0.7, keyheight = 0.7,title=NULL))

#x axis labelling customisation
p = p + theme(axis.text.x=element_text(angle=90, size=10, vjust=0.5))
p = p + theme(axis.title.y=element_text(size=14))

#offset axis labels
p = p + theme(axis.title.y = element_text(vjust=0.8))

#label axis
p = p + labs(x="Identification Tag", y="Average Speed (m/s)")
p = p + theme(axis.title.x = element_text(face="bold"))

#set y limits
p = p + ylim(c(0,1))

p = p + scale_y_continuous(expand = c(0, 0))

#remove x grid
p = p + theme(panel.grid.minor.x=element_blank(),panel.grid.major.x=element_blank())

#create bar plot
p = p + geom_bar(position="dodge",stat="identity", width=0.7,col="black") 

#add error bars
p = p + geom_errorbar(aes(ymax = mean + se, ymin=mean),
                      position=position_dodge(width=0.7), 
                      width=0.3)

p = p + facet_grid(. ~ ENSO,scales="free",space="free")  

#run plot
p

#output to image ----

output_folder = Sys.getenv("userprofile")

ggsave(p, file=file.path(output_folder, "\\Desktop\\CCmeans.svg"))
