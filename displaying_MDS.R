#Codigo para graficar MDS y guardar las graficas en .pdf


#Elegir el path - directorio donde se encuentran los archivos de las muestras filtradas.

setwd("C:/Codigos en R/Final code/Code filtering U Pb/samples/filtered files")



#archivo con datos de muestras
nombredelarchivo = "C:/Codigos en R/Final code/Displaying MDS data/sampledata_actualizada.csv"


#codigo para cargar los archivos .flt creados con el otro codigo y tomar la columna de edadpref
#para hacer un archivo .excel con los datos de todas las muestras


#elegir atributo para color, tiene que ser el nombre de una columna
atribute = "Formation"

#Elegir el nombre del archivo para el plot a color

plotname2 = "MDS_por_Formacion+color.pdf"

#Elegir el nombre del archivo para el plot blanco y negro

plotname1 = "MDS_por_Formacion+bw.pdf"

#Elegir directorio para guardar las graficas en pdf

savefilesto = "C:/Codigos en R/Final code/Code filtering U Pb/samples/final_graphs"

dir.create(savefilesto, showWarnings = FALSE)


library(lattice)


file = read.csv("DZages.csv",header = TRUE, check.names = FALSE)
n = length(file)
DZages=list()
for (i in 1:n)
  DZages[[names(file)[i]]] = file[!is.na(file[,i]),i]
end
save(DZages,file="DZages.Rdata")

library(MASS)          # load the "Modern Applied Statistics with S" package
load("DZages.Rdata")   # load the set of U-Pb ages
n = length(DZages)     # n = the number of samples

#A more sophisticated script including nearest-neighbour lines:
# define a function to calculate the dissimilarity matrix
dissimilarity <- function(data) {
  n = length(data)
  # instantiate the dissimilarity matrix as an empty data frame
  diss = as.data.frame(mat.or.vec(n,n))
  rownames(diss) = names(data)
  for (i in 1:n){   # loop through all possible pairs of samples
    for (j in 1:n){  # calculate the kolmogorov-smirnov statistic
      diss[i,j] = ks.test(data[[i]],data[[j]])$statistic
    }
  }
  return(diss)
}

# define a function to plot the configuration and (optionally) connect the nearest neighbours
plotmap <- function(conf,diss,nnlines=FALSE) {
  # create a new (empty) plot
  plot(conf[,1],conf[,2],type='n',xlab="",ylab="")
  # draw lines between closest neighbours
  if (nnlines) { plotlines(conf,diss) }
  # plot the configuration as labeled circles
  points(conf[,1],conf[,2],pch=21,cex=2.5,col='red',bg='white')
  text(conf[,1],conf[,2],row.names(as.matrix(diss)))
}

# a function to plot the nearest neighbour lines (feel free not to use if it clutters the MDS map)
plotlines <- function(conf,diss) {
  # rank the samples according to their pairwise proximity
  i = t(apply(as.matrix(diss),1,function(x) order(x))[2:3,])
  # coordinates for the lines
  x1 = as.vector(conf[i[,1],1]) # calculate (x,y)-coordinates ...
  y1 = as.vector(conf[i[,1],2]) # ... of nearest neighbours
  x2 = as.vector(conf[i[,2],1]) # calculate (x,y)-coordinates ...
  y2 = as.vector(conf[i[,2],2]) # ... of second nearest neighbours
  for (j in 1:nrow(conf)) {
    lines(c(conf[j,1],x1[j]),c(conf[j,2],y1[j]),lty=1) # solid line
    lines(c(conf[j,1],x2[j]),c(conf[j,2],y2[j]),lty=2) # dashed line
  }
}

# main body of the script:
load("DZages.Rdata")
diss = dissimilarity(DZages)
# 1. classical multidimensional scaling
plotmap(cmdscale(diss),diss) # without nearest neighbour lines (default)
# 2. nonmetric multidimensional scaling
library(MASS)  # load the "Modern Applied Statistics with S" package
dev.new()      # create a new plot window


# Graficas
setwd(savefilesto)

pdf(plotname1,width = 10, height = 10)

plotmap(isoMDS(as.matrix(diss))$points,diss,nnlines=TRUE) # includes nearest neighbour lines

dev.off()



#Codigo para plotear con ggplot

data = data.frame(isoMDS(as.matrix(diss)))



datatable <- read.csv(nombredelarchivo, row.names=1, header = TRUE,stringsAsFactors=FALSE,as.is=T, na.strings = "")








library(ggplot2)

for(i in 1:length(data[,1]))
{
  
  
  data$color[i] = datatable[as.numeric(row.names(data)[i]),atribute]
  
  
}

setwd(savefilesto)

pdf(plotname2,width = 10, height = 10)

p <- qplot(points.1,points.2,data = data, label = rownames(data))  + xlim(min(pmin(data[,1],data[,2])),max(pmax(data[,1],data[,2]))) + ylim(min(pmin(data[,1],data[,2])),max(pmax(data[,1],data[,2]))) 

p + aes(colour = color) +geom_point(size=5)+geom_text(vjust =2, color = "black") + theme(panel.background = element_rect(fill = 'white', colour = 'black'))

dev.off()

