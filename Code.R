
# -------------------------------------------
# Religious Intolerance, Fear, and Insecurity
# -------------------------------------------

# Get the data
path <- file.path("https://raw.githubusercontent.com/milesdwilliams15/Religious-Intolerance-Fear-and-Insecurity/master/data.csv")
dat <- read.csv(path)
write.csv(dat,"C:/Users/Miles/Documents/R/Religious Tolerance and Democracy/dat.csv")
path <- file.path("C:","Users","Miles","Documents","R","Religious Tolerance and Democracy","dat.csv")
relDem <- read.csv(path)

attach(relDem)


#-----------------------------------------------
# Create multiplots of the variables of interest

Democracy <- car::Recode(relDem$Democracy,"1='Democracy';else='Non-Democracy'")
Religious.Tolerance <- log((Complete.Religious.Tolerance + Some.Religious.Tolerance)/(Little.Religious.Tolerance+No.Religious.Tolerance))
Trust.in.Neighbors <- log((Complete.Trust.in.Neighbors + Some.Trust.in.Neighbors)/(Little.Trust.in.Neighbors+No.Trust.in.Neighbors))
Security.Worries <- log((((Much.Worry.about.War+Much.Worry.about.Civil.War)/2)+((Some.Worry.about.War+Some.Worry.about.Civil.War)/2))/(((Little.Worry.about.War+Little.Worry.about.Civil.War)/2)+((No.Worry.about.War+No.Worry.about.Civil.War)/2)))
Religious.Pluralism <- (-1*Pluralism)+100
Country <- relDem$Country
Religion <- relDem$Religion
library(ggplot2)
g1 <- ggplot(data.frame(Religious.Tolerance,Trust.in.Neighbors,Security.Worries,Country,Democracy),
       aes(x=Security.Worries,y=Religious.Tolerance,color=Democracy)) + geom_point(alpha=0) +
  geom_smooth() + geom_text(aes(label=Country),size=3) + theme_bw() +
  xlab("Security Worries") + ylab("Religious Tolerance") + theme(legend.position = c(0.2,0.2),legend.title=element_blank())
g2 <- ggplot(data.frame(Religious.Tolerance,Trust.in.Neighbors,Security.Worries,Country,Democracy),
             aes(x=Security.Worries,y=Trust.in.Neighbors,color=Democracy)) + geom_point(alpha=0) +
  geom_smooth() + geom_text(aes(label=Country),size=3) + theme_bw() +
  xlab("Security Worries") + ylab("Trust in Neighbors") + theme(legend.position = "none")
g3 <- ggplot(data.frame(Religious.Tolerance,Trust.in.Neighbors,Security.Worries,Country,Democracy),
             aes(x=Trust.in.Neighbors,y=Religious.Tolerance,color=Democracy)) + geom_point(alpha=0) +
  geom_smooth() + geom_text(aes(label=Country),size=3) + theme_bw() +
  xlab("Trust in Neighbors") + ylab("Religious Tolerance") + theme(legend.position = "none")
g4 <- ggplot(data.frame(Religious.Pluralism,Religious.Tolerance),
             aes(x=Religious.Pluralism,y=Religious.Tolerance,color=Democracy)) + geom_point(alpha=0) +
  geom_smooth() + geom_text(aes(label=Country),size=3) + theme_bw() +
  xlab("Religious Pluralism") + ylab("Religious Tolerance") + theme(legend.position = "none")
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
windows() # use quartz() for Mac
multiplot(g1,g2,g3,g4,cols=2,layout=matrix(c(1,2,3,4), nrow=2, byrow=TRUE))

g1 <- ggplot(data.frame(Religious.Tolerance,Trust.in.Neighbors,Security.Worries,Religion,Democracy),
             aes(x=Security.Worries,y=Religious.Tolerance,color=Democracy)) + geom_point(alpha=0) +
  geom_smooth() + geom_text(aes(label=Religion),size=3) + theme_bw() +
  xlab("Security Worries") + ylab("Religious Tolerance") + theme(legend.position = c(0.2,0.2),legend.title=element_blank())
g2 <- ggplot(data.frame(Religious.Tolerance,Trust.in.Neighbors,Security.Worries,Religion,Democracy),
             aes(x=Security.Worries,y=Trust.in.Neighbors,color=Democracy)) + geom_point(alpha=0) +
  geom_smooth() + geom_text(aes(label=Religion),size=3) + theme_bw() +
  xlab("Security Worries") + ylab("Trust in Neighbors") + theme(legend.position = "none")
g3 <- ggplot(data.frame(Religious.Tolerance,Trust.in.Neighbors,Security.Worries,Religion,Democracy),
             aes(x=Trust.in.Neighbors,y=Religious.Tolerance,color=Democracy)) + geom_point(alpha=0) +
  geom_smooth() + geom_text(aes(label=Religion),size=3) + theme_bw() +
  xlab("Trust in Neighbors") + ylab("Religious Tolerance") + theme(legend.position = "none")
g4 <- ggplot(data.frame(Religious.Tolerance,Trust.in.Neighbors,Security.Worries,Religion,Democracy,Religious.Pluralism),
             aes(x=Religious.Pluralism,y=Religious.Tolerance,color=Democracy)) + geom_point(alpha=0) +
  geom_smooth() + geom_text(aes(label=Religion),size=3) + theme_bw() +
  xlab("Religious Pluralism") + ylab("Religious Tolerance") + theme(legend.position = "none")
windows() # use quarz() for Mac
multiplot(g1,g2,g3,g4,cols=2,layout=matrix(c(1,2,3,4), nrow=2, byrow=TRUE))


# ----------------------------------
# Create Partial Correlation Network

pcor <- ppcor::pcor(na.omit(as.matrix(cbind(Religious.Tolerance,Trust.in.Neighbors,Security.Worries,Religious.Pluralism))))
spc4 <- pcor$estimate
spc4[lower.tri(spc4,diag=TRUE)]=NA # put NA
spc4<-as.data.frame(as.table(spc4)) # as a dataframe
spc4<-na.omit(spc4) # remove NA
spc4$estimate <- spc4$Freq
spc4 <- spc4[,-3]
spc4$Var1 <- c("Religious\nTolerance","Religious\nTolerance","Trust in\nNeighbors","Religious\nTolerance",
               "Trust in\nNeighbors","Security\nWorries")
spc4$Var2 <- c("Trust in\nNeighbors","Security\nWorries","Security\nWorries","Religious\nPluralism",
               "Religious\nPluralism","Religious\nPluralism")

library(igraph)
spc4 <- graph_from_data_frame(spc4, directed = F)
E(spc4)$width <- 10*E(spc4)$estimate
E(spc4)$color <- NA
E(spc4)$color[E(spc4)$estimate > 0] <- "darkblue"
E(spc4)$color[E(spc4)$estimate < 0] <- "red"

fr1 <- layout_with_fr(spc4)
windows()
set.seed(44)
plot(spc4,edge.width=abs(E(spc4)$width),edge.curved=0.15,layout=fr1,
     edge.color=E(spc4)$color,vertex.shape="sphere",vertex.color="lightgrey",vertex.frame.color="lightgrey",
     vertex.size=35,vertex.label.cex=.75,vertex.label.color="black")
legend("bottomleft",legend=c("(+) Correlation","(-) Correlation"),
       lty=1,lwd=2,col=c("darkblue","red"))


windows()
g1 <- ggplot(data=data.frame(Religion,Religious.Tolerance),
       aes(x=reorder(Religion,Religious.Tolerance),y=Religious.Tolerance)) + geom_boxplot() + theme_bw() +
  theme(text=element_text(family="serif")) +
  xlab("Dominant Religious Tradition") + ylab("Religious Tolerance")
g2 <- ggplot(data=data.frame(Religion,Religious.Tolerance),
       aes(x=Democracy,y=Religious.Tolerance)) + geom_boxplot() + theme_bw() +
  theme(text=element_text(family="serif")) +
  xlab("Political Regime") + ylab("Religious Tolerance")
multiplot(g1,g2,cols=2)

windows()
g1 <- ggplot(data=data.frame(Religion,Trust.in.Neighbors),
             aes(x=reorder(Religion,Trust.in.Neighbors),y=Trust.in.Neighbors)) + geom_boxplot() + theme_bw() +
  theme(text=element_text(family="serif")) +
  xlab("Dominant Religious Tradition") + ylab("Trust in Neighbors")
g2 <- ggplot(data=data.frame(Religion,Trust.in.Neighbors),
             aes(x=Democracy,y=Trust.in.Neighbors)) + geom_boxplot() + theme_bw() +
  theme(text=element_text(family="serif")) +
  xlab("Political Regime") + ylab("Trust in Neighbors")
multiplot(g1,g2,cols=2)

windows()
g1 <- ggplot(data=data.frame(Religious.Pluralism,Security.Worries,Country,Democracy),
             aes(x=Religious.Pluralism,y=Security.Worries,color=Democracy)) + geom_point(alpha=0) + theme_bw() +
  geom_smooth() +
  theme(legend.position=c(0.2,0.2)) +
  theme(legend.title=element_blank()) +
  geom_text(aes(label=Country)) +
  theme(text=element_text(family="serif")) + 
  xlab("Religious Pluralism") + ylab("Fear of Conflict")
g2 <- ggplot(data=data.frame(Religious.Tolerance,Security.Worries,Country,Democracy),
             aes(x=Religious.Tolerance,y=Security.Worries,color=Democracy)) + geom_point(alpha=0) + theme_bw() +
  geom_smooth() +
  geom_text(aes(label=Country)) +
  theme(legend.position="none") +
  theme(text=element_text(family="serif")) + 
  xlab("Religious Tolerance") + ylab("Fear of Conflict")
multiplot(g1,g2,cols=2)
