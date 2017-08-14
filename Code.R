
# -------------------------------------------
# Religious Intolerance, Fear, and Insecurity
# -------------------------------------------

# Get the data
path <- file.path("https://raw.githubusercontent.com/milesdwilliams15/Religious-Intolerance-Fear-and-Insecurity/master/data.csv")
relDem <- read.csv(path)

attach(relDem)


#-----------------------------------------------
# Create multiplots of the variables of interest

Democracy <- car::Recode(relDem$Democracy,"1='Democracy';else='Non-Democracy'")
Religious.Tolerance <- log((Complete.Religious.Tolerance + Some.Religious.Tolerance)/(Little.Religious.Tolerance+No.Religious.Tolerance))
Trust.in.Neighbors <- log((Complete.Trust.in.Neighbors + Some.Trust.in.Neighbors)/(Little.Trust.in.Neighbors+No.Trust.in.Neighbors))
Security.Worries <- log((((Much.Worry.about.War+Much.Worry.about.Civil.War)/2)+((Some.Worry.about.War+Some.Worry.about.Civil.War)/2))/(((Little.Worry.about.War+Little.Worry.about.Civil.War)/2)+((No.Worry.about.War+No.Worry.about.Civil.War)/2)))
library(ggplot2)
g1 <- ggplot(data.frame(Religious.Tolerance,Trust.in.Neighbors,Security.Worries,Country,Democracy),
       aes(x=Security.Worries,y=Religious.Tolerance,color=Democracy)) + geom_point(alpha=0) +
  geom_smooth() + geom_text(aes(label=Country),size=3) + theme_bw() +
  xlab("Security Worries") + ylab("Religious Tolerance") + theme(legend.position="none")
g2 <- ggplot(data.frame(Religious.Tolerance,Trust.in.Neighbors,Security.Worries,Country,Democracy),
             aes(x=Security.Worries,y=Trust.in.Neighbors,color=Democracy)) + geom_point(alpha=0) +
  geom_smooth() + geom_text(aes(label=Country),size=3) + theme_bw() +
  xlab("Security Worries") + ylab("Trust in Neighbors") + theme(legend.position = c(0.5,0.9),legend.title=element_blank())
g3 <- ggplot(data.frame(Religious.Tolerance,Trust.in.Neighbors,Security.Worries,Country,Democracy),
             aes(x=Trust.in.Neighbors,y=Religious.Tolerance,color=Democracy)) + geom_point(alpha=0) +
  geom_smooth() + geom_text(aes(label=Country),size=3) + theme_bw() +
  xlab("Trust in Neighbors") + ylab("Religious Tolerance") + theme(legend.position = "none")
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
multiplot(g1,g2,g3,cols=3)

g1 <- ggplot(data.frame(Religious.Tolerance,Trust.in.Neighbors,Security.Worries,Culture,Democracy),
             aes(x=Security.Worries,y=Religious.Tolerance,color=Democracy)) + geom_point(alpha=0) +
  geom_smooth() + geom_text(aes(label=Culture),size=3) + theme_bw() +
  xlab("Security Worries") + ylab("Religious Tolerance") + theme(legend.position="none")
g2 <- ggplot(data.frame(Religious.Tolerance,Trust.in.Neighbors,Security.Worries,Culture,Democracy),
             aes(x=Security.Worries,y=Trust.in.Neighbors,color=Democracy)) + geom_point(alpha=0) +
  geom_smooth() + geom_text(aes(label=Culture),size=3) + theme_bw() +
  xlab("Security Worries") + ylab("Trust in Neighbors") + theme(legend.position = c(0.5,0.9),legend.title=element_blank())
g3 <- ggplot(data.frame(Religious.Tolerance,Trust.in.Neighbors,Security.Worries,Culture,Democracy),
             aes(x=Trust.in.Neighbors,y=Religious.Tolerance,color=Democracy)) + geom_point(alpha=0) +
  geom_smooth() + geom_text(aes(label=Culture),size=3) + theme_bw() +
  xlab("Trust in Neighbors") + ylab("Religious Tolerance") + theme(legend.position = "none")
windows() # use quarz() for Mac
multiplot(g1,g2,g3,cols=3)


# ----------------------------------
# Create Partial Correlation Network

pcor <- ppcor::pcor(na.omit(as.matrix(cbind(Religious.Tolerance,Trust.in.Neighbors,Security.Worries))))
spc4 <- pcor$estimate
spc4[lower.tri(spc4,diag=TRUE)]=NA # put NA
spc4<-as.data.frame(as.table(spc4)) # as a dataframe
spc4<-na.omit(spc4) # remove NA
spc4$estimate <- spc4$Freq
spc4 <- spc4[,-3]
spc4$Var1 <- c("Religious Tolerance","Religious Tolerance","Trust in Neighbors")
spc4$Var2 <- c("Trust in Neighbors","Security Worries","Security Worries")

library(igraph)
spc4 <- graph_from_data_frame(spc4, directed = F)
E(spc4)$width <- 10*E(spc4)$estimate
E(spc4)$color <- NA
E(spc4)$color[E(spc4)$estimate > 0] <- "darkblue"
E(spc4)$color[E(spc4)$estimate < 0] <- "red"

fr1 <- layout_with_fr(spc4)
windows()
set.seed(44)
plot(spc4,edge.width=abs(E(spc4)$width),edge.curved=0.25,layout=fr1,
     edge.color=E(spc4)$color,vertex.color="white",vertex.frame.color="white",
     vertex.size=20,vertex.label.cex=1,vertex.label.color="black")
legend("bottomleft",legend=c("(+) Correlation","(-) Correlation"),
       lty=1,lwd=2,col=c("darkblue","darkred"))
