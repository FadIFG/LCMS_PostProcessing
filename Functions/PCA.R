library(ggplot2)


PCA.SCPlot<-function(x, Groups, title){ #Achtung Title in "", Groups ist Gruppenzuordnung
  pca <-prcomp(x, scale. = T, center = T)
  PCA <- as.data.frame(pca$x)
  percentage <- round((pca$sdev^2/sum(pca$sdev^2)) * 100, 2)
  percentage <- paste( colnames(PCA), "(", paste(as.character(percentage), "%", ")", sep="") )
  p<-ggplot(PCA,aes(x=PC1,y=PC2,color=Groups,label=row.names(x))) 
  p<-p+ ggtitle(title) #title
  p<-p+ geom_point(size = 7)   #Point size
  #p<-p+ geom_text(size=4,adj=0.3, nudge_x, nudge_y)     #label text size (of the points), adj=shifts the label positioning to the side of the point
  #p<-p+ geom_text(size=6 , nudge_x=1, nudge_y=-5)  
  p<-p+ theme(plot.title = element_text(color="darkblue", size=16, face="bold",hjust = 0.5))  #plot title modifications,hjust=position
  #######Labeling the axes#################
  p<-p+xlab(percentage[1]) + ylab(percentage[2])
  p<-p+ theme(axis.title.x = element_text(size =24)) + theme(axis.title.y = element_text(size = 24, angle = 90))
  p<-p+ theme(axis.text.x = element_text(size = 22))+ theme(axis.text.y = element_text(size = 22))
  ##############Legend Modification###############
  p<- p + theme(legend.title = element_text(colour="blue",size=20,face="bold") #legend Title modifications
                ,legend.text = element_text(colour="black", size=24) #legend text modifications
                ,legend.position="right") #"left","top", "right", "bottom"
  #The argument legend.position can be also a numeric vector c(x,y).
  #In this case it is possible to position the legend inside the plotting area.
  #x and y are the coordinates of the legend box. Their values should be between 0 and 1.
  #c(0,0) corresponds to the "bottom left" and c(1,1) corresponds to the "top right" position.
  p<-p+ guides(color = guide_legend(override.aes = list(size=8))) #Legend point size
  #Changing the background color of the legend box:
  
  p<- p+ theme(legend.background = element_rect(fill="lightblue",
                                                size=0.5, linetype="solid", 
                                                colour ="darkblue")) #you can delete this last line if no contour is desired.
  library(RColorBrewer)
  #nb.cols<-11  #number of the colors to be included(same as the number of your groups)
  #mycolors <- colorRampPalette(brewer.pal(8, "Dark2"))(nb.cols) 
  mycolors <- c("black","red3", "blue","forestgreen", "cyan", "green",  "gold2",  "darkorchid3",  "deeppink",  "gray48", "darksalmon", "orange" , "pink", "brown","yellow","orchid", "gold", "red1", "red2","gold3", "pink2", "brown2")
  #'Dark2' is the palette to be extended 
  #'8' is the number of the original colors in th palette
  p<- p + scale_color_manual(values = mycolors)
  
  #adding Elipses to the groups
  #library(ggalt)
  #p<- p + geom_encircle(aes(fill = mycolors), s_shape = 0.5, expand = 0.02,
  #              alpha = 0.2, color = "black", show.legend = FALSE,spread=0.02)
  #p
  #p<- p + geom_encircle(aes(group=Groups,colour=mycolors), s_shape = 0.5, expand = 0.02,
  #                      alpha = 0.2, color = "black", show.legend = FALSE,spread=0.02)
  p
  
  
}


