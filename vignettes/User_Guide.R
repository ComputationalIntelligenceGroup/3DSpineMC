## ----eval=F--------------------------------------------------------------
#  install.packages("path/to/file/SpineMC_0.1.tar.gz", repos = NULL, type="source")

## ----message=FALSE-------------------------------------------------------
library(SpineMC)

## ----eval=FALSE----------------------------------------------------------
#  model <- spineClustering(csvSpines = system.file("extdata", "data.csv", package = "SpineMC"), numClusters = c(2:10), scale = F)

## ----fig.width=7.2,fig.height=5------------------------------------------
  clusters<-paste("Cluster",1:model$G)
  distribution<-as.numeric(table(model$classification))
   dfDistribution<-data.frame(values=distribution,clusters=clusters)
  dfDistribution$values<-as.numeric(as.character(distribution))

  ggplot(dfDistribution,aes(clusters,values,fill=as.factor(clusters))) +
    geom_bar(stat="identity")+xlab("Cluster distribution") + ylab("Num spines")  + guides(fill=guide_legend(title=NULL)) + theme(plot.title = element_text(lineheight = .8, face = "bold", size=20), axis.title.x = element_text(face="bold",size=16), axis.title.y =element_text(face="bold",size=16), axis.text =element_text(size=12), legend.text =element_text(size=12))

## ------------------------------------------------------------------------
#Get membership probability
membership <- data.frame(model$z)
colnames(membership) <- paste("Cluster",1:model$G)
#Show ten first
membership[1:10, ]


## ---- eval=F-------------------------------------------------------------
#  #Export data of dendritic spines of cluster 1 to a temporal directory.
#  temp_path<-tempdir()
#  write.csv(model$data[model$classification==1,],file="temp_path/cluster1.csv")

## ----fig.width=7.2,fig.height=7.2----------------------------------------
MDS<-computeMDS(model,2)
plotMDS(model,MDS)

## ----eval=F--------------------------------------------------------------
#  computeOverlapping(model)

## ----kable, echo=F-------------------------------------------------------
library(knitr)
kable(computeOverlapping(model))

## ----fig.width=7.2,fig.height=7.2----------------------------------------
#Plot the BIC score by the number of cluster and their model name.
plotBIC(model)

#Show the number of clusters that maximize BIC
print(paste("The number of clusters is",model$G))

