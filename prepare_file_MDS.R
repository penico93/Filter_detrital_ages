#Este codigo toma los datos de las muestras filtradas y las organiza en un .csv llamado DZages, que sera utilizado para hacer el MDS

library(gtools)
library(plyr)
library(qpcR)



# Choose path to where filtered files are stored

setwd("C:/Codigos en R/Final code/Code filtering U Pb/samples/filtered files")

  #  "C:/Users/penico93/Google Drive/Llanos/NPC/Codigos en R/Final code/Code filtering U Pb/organizar_MDS.R"
filenames <- list.files(pattern="*.flt")

# path to the file that has all the sample info

nombredelarchivo = "C:/Codigos en R/Final code/Displaying MDS data/sampledata_actualizada.csv"

# Read filename

datatable <- read.csv(nombredelarchivo, row.names = 1, header = TRUE,stringsAsFactors=FALSE,as.is=T, na.strings = "undefined" )


#

data <- data.frame(matrix(NA))

#

for(i in 1:length(datatable[,1])){
  
  datatable$Nombre[i] = paste(c(toString(datatable$Nombre[i]),".flt"),collapse = '')
  
  datatable$Nombre[i] = toString(datatable$Nombre[i])
  from = datatable$Nombre[i]
  
}


for(i in 1:length(datatable[,1])){
  
  from = datatable$Nombre[i]
  
  if(file.exists(from)==TRUE){

    data1 = read.table(from, header = TRUE,stringsAsFactors=FALSE,as.is=T, na.strings = "undefined" )
    names(data1)[1] <- toString(datatable$Numero[i])

    data <- qpcR:::cbind.na(data, data1[1])
    
   
    

  }
  
  
  
}

data = data[,-1]

write.csv(data, file = paste(c("DZages",".csv"),collapse = ''), row.names = FALSE)

data1



