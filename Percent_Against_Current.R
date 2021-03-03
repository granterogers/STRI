library(ggplot2)
library(scales)
#library(extrafont)

# load and prepare data ----
AllTurtles = read.csv("DATA//Merged_Velocities_with_Drifters.csv")

#remove green turtles
AllTurtles = AllTurtles[- grep("97799", AllTurtles$ID),]
AllTurtles = AllTurtles[- grep("97796", AllTurtles$ID),]
AllTurtles = AllTurtles[- grep("97797", AllTurtles$ID),]
AllTurtles = AllTurtles[- grep("101508", AllTurtles$ID),]

AllTurtles$T.Time=as.numeric(as.POSIXct(as.character(AllTurtles$T.Time),format="%Y-%m-%d %H:%M:%S"))

#create angular difference column
AllTurtles$Theta.Diff=abs(AllTurtles$Bearing_T - AllTurtles$Bearing_C)

##begin my custom function

calculate_percent_against_current = function(x)
{
  threshold = 80
  
  total_length = length(x)
  
  threshold_table = x[x>threshold]
  
  length_threshold = length(threshold_table)
  
  final_percent = (length_threshold/total_length)*100
  
  return(final_percent)
}

Against_Current = aggregate(AllTurtles$Theta.Diff, list(AllTurtles$ID), calculate_percent_against_current)

#fiddle with dataframe for plotting ----

#transfer to matrix
MyMatrix = matrix(data=c(Against_Current[,2]), byrow=T, ncol=length(unique(AllTurtles$ID)))

#convert matrix to data frame
MyDF = as.data.frame(t(MyMatrix))

MyDF = as.data.frame(rbind(Against_Current))

#label columns in data frame
names(MyDF)[1:2]<-c("ID","Percent_Against_Current")

#get rid of turtle ID with a at the end
MyDF$ID=gsub("a(?=$)","", MyDF$ID, perl=TRUE)

#order dataframe for Drifters/Chiriqui/Las Perlas ----

neworder = c("D41668","D42579","D71251","D75194","D75197","D75256","D88541","T101503","T101506","T101507",
             "T46207","T46238","T46247","T46248","T46249","T46252","T46256","T46257","T46258","T46259","T46260",
             "T64494","T64495","T64496","T52710","T52711","T97800","T97794","T97795","T97792","T97793","T101502",
             "T101504","T101505")

#ENSO label table ----
ENSO_Labels = c("Drifter","Drifter","Drifter","Drifter","Drifter","Drifter","Drifter","No ENSO","No ENSO","No ENSO",
                "No ENSO","No ENSO","No ENSO","No ENSO","No ENSO","No ENSO","No ENSO","No ENSO","No ENSO","No ENSO","No ENSO",
                "No ENSO","No ENSO","No ENSO","El Niño","El Niño","El Niño","El Niño","El Niño","El Niño","El Niño","La Niña",
                "La Niña","La Niña")

MyDF = MyDF[match(neworder, MyDF$ID),]

row.names(MyDF) <- NULL

MyDF$ID <- factor(MyDF$ID, as.character(MyDF$ID))

MyDF$ENSO = ENSO_Labels

MyDF$ENSO = factor(MyDF$ENSO,levels = c("Drifter","No ENSO","El Niño","La Niña"))

# create plot variables ----

# start plotting commands ----

p = ggplot(MyDF, aes(y=Percent_Against_Current, x=ID)) +
  coord_cartesian(ylim=c(0,80)) 
  
#remove gray background
p <- p + theme_bw()

#create color profile for plot
BlackWhite <- rep(c("black", "grey"), length(MyDF$ID)/2)

# axis labelling customisation
p = p + theme(axis.text.x=element_text(angle=90, size=10, vjust=0.5))
p = p + theme(axis.title.y=element_text(size=14))

#set y limits
p = p + ylim(c(0,80))

#label axis
p = p + labs(x="Identification Tag", y=expression("% Time Swimming Against Current"))
p = p + theme(axis.title.x = element_text(face="bold"), axis.title.y = element_text(face="bold"))

#remove x grid
p = p + theme(panel.grid.minor.x=element_blank(),panel.grid.major.x=element_blank())

# set up bar chart ----

#dodge = side by side plot  identiy-height = values
p = p + geom_bar(width=0.7, stat="identity",col="black",fill=BlackWhite) +
  scale_y_continuous(breaks = pretty_breaks(n = 6))

p = p + scale_y_continuous(expand = c(0, 0))

p = p + facet_grid(. ~ ENSO,scales="free",space="free")  

#plot the graph
p
 
output_folder = Sys.getenv("userprofile")

ggsave(p, file=file.path(output_folder, "\\Desktop\\PercentAgainstCurrents.svg"))


