#R code for filtering files of U-Pb detrital Zircon dating on the repository specified below
library("WriteXLS")
library("gtools")
library("plyr")
library("qpcR")


# Set working directory - Set path where the sample files are located
setwd("C:/Codigos en R/Final code/Code filtering U Pb/samples")
                    
                
# Set the output directory for the filtered files
outputdirectory = getwd()

# filter_percentage defines the maximum value of discordance accepted for a sample.
filter_percentage = 0.1

# filter_agelimit which defines the value of age for which the discordance used as a test.
filter_agelimit = 300

# filter_age_choosing defines the limit age to decide wether to use the 206/207 or 206/238 age as the preferred age.
filter_age_choosing = 1000

# filter_discordance defines the value of tolerance of the discordance

filter_discordance = 15

# filenames obtains the filename of every file with .csv termination in the set working directory
filenames <- list.files(pattern="*.csv")


filterdata <- function(file1,filter_percentage,filter_agelimit, filter_age_choosing,outputtype) {
  
  
# Reads the filename fiel1 (type csv) and saves it into a dataframe called datafromsample.
  datafromsample <- read.csv(file1, header = TRUE,stringsAsFactors=FALSE,as.is=T, na.strings = "undefined" )[ ,c('u1', 'u1error','u2','u2error')]
# Erases all NA values from datafromsample  
  datafromsample <- na.omit(datafromsample)
# Reads the filename fiel1 (type csv) and saves it into a dataframe called datafromsample.

# Generates a new column of data (discordance) where the discordance value will be calculated.
  for(i in 1:nrow(datafromsample)){
    
    datafromsample$discordance[i] = 100*datafromsample$u1[i]/datafromsample$u2[i]
    
  }
  
# Selects the prefered age based on comparing the mean (i.e (u1+u2)/2) of u1 and u2 values with filter_age_choosing. If it the AVERAGE is lower than filter_age_choosing then the preferred age is u1 otherwise is u2.
  for(i in 1:nrow(datafromsample)){
    
    if(((datafromsample$u1[i]+datafromsample$u2[i])/2)< filter_age_choosing){datafromsample$edadpref[i] = datafromsample$u1[i]; datafromsample$edadpreferror[i] = datafromsample$u1error[i]}else{datafromsample$edadpref[i] = datafromsample$u2[i]; datafromsample$edadpreferror[i] = datafromsample$u2error[i]}
    
  }
  
  #= Create temporary data.frame where bad samples (meaning that they not pass the filter) are stored as well as two other that frames where good samples will go
  samples_with_error<- data.frame(matrix(NA))
  samples_good1<- data.frame(matrix(NA))
  samples_good2<- data.frame(matrix(NA))
  
  # Counts the number of rows in the datafromsample dataframe
  contador = nrow(datafromsample)
  #datafromsample$edadpreferror[1]
  
# The error from the preferred age is compared with a percentage (defined by the user e.g. 0.1-10% or 0.2-20%) of the preferred age, to decide wether or not the age needs to be filtered. In the case that the error is greater or equal than the specified percentage of
#the preferred age, this analysis will not be taken into account, but will be sent to samples_with_error dataframe. In the case
# that the error is smaller than the specified percentage, then the age is taken to the sample_good1 dataframe (i.e it passed the filter number 1)
  for(i in 1:contador){
    
    if(datafromsample$edadpreferror[i]>=(filter_percentage*datafromsample$edadpref[i])){samples_with_error<-smartbind(samples_with_error,datafromsample[i,])}else{samples_good1<-smartbind(samples_good1,datafromsample[i,])}
    
  }


# Erases the first row of the previously created dataframes because it is empty
  if(nrow(samples_good1)>1||ncol(samples_good1)>1){
    samples_good1 <-samples_good1[-1,]
    
  }
  samples_good1
  if(nrow(samples_with_error)>1||ncol(samples_with_error)>1){
    samples_with_error <-samples_with_error[-1,]
    
  }
  
  
  #Aqui se revisa si la edad es mayor o menor a 300, en caso de ser menor se pasa directamente al dataframe samples_good2
  # sie la edad es mayor a 300 se debe hacer un filtro de acuerdo a la discordancia! que se habia calculado previamente
  for(i in 1:nrow(samples_good1)){
    
    if(samples_good1$edadpref[i]< filter_agelimit){samples_good2 <- smartbind(samples_good2,samples_good1[i,])}
    else if((samples_good1$discordance[i]< (90-filter_discordance))||(samples_good1$discordance[i]>(90 + filter_discordance))){samples_with_error = smartbind(samples_with_error,samples_good1[i,])}
    else{samples_good2 = smartbind(samples_good2,samples_good1[i,])}
    
    
  }

 # Polishing the dataframes (not important)
  nrow(datafromsample)
  nrow(samples_with_error)
  if(nrow(samples_good2)>1||ncol(samples_good2)>1){
    samples_good2 <-samples_good2[-1,]
  }
  nrow(samples_good2)
  
  datos_filtrados<- data.frame(matrix(NA))
  if(nrow(samples_good2)>1||ncol(samples_good2)>1){
    samples_good2 <-samples_good2[,-1]
  }
  
  if(nrow(samples_good1)>1||ncol(samples_good1)>1){
    samples_good1 <-samples_good1[,-1]
  }
  
  if(nrow(samples_with_error)>1||ncol(samples_with_error)>1){
    samples_with_error <-samples_with_error[,-1]
  }
  

  datos_filtrados = cbind(datos_filtrados,samples_good2$edadpref)
  datos_filtrados = cbind(datos_filtrados,samples_good2$edadpreferror)
  datos_filtrados <- datos_filtrados[,-1]  
  

  #direccion del output directory y crea el output directory con el numero del filter. #show warnings = false evita que muestre warnings en caso de que este dir. ya exista
  setwd(outputdirectory)
  vd = c(getwd(),'/',"filtered files",'/')
  directory = paste(vd,collapse = '')
  dir.create(directory,showWarnings = FALSE)
  
  file1 = strsplit(file1,'.csv')
  fltfilename = c(file1,outputtype)
  finald = c(directory,fltfilename)
  
  
  #Code for creating output files
  write.table(datos_filtrados, file = paste(finald,collapse = ''))

}


filenames <- list.files(pattern="*.csv")

for(i in 1:length(filenames)){
  
  file1 = filenames[i]
    
  filterdata(file1,filter_percentage,filter_agelimit,filter_age_choosing,'.flt')
  
}


setwd(paste((c(getwd(),"/filtered files")),collapse = ''))


paste((c(getwd(),"/filtered files")),collapse = '')
filenames <- list.files(pattern="*.flt")
file_type = '.flt'


lista <- data.frame(matrix(NA, ncol = 0, nrow = 20))




for(i in 1:length(filenames)){
  name1 = filenames[i]
  
  
  
  
  data1 = read.table(name1, header = TRUE,stringsAsFactors=FALSE,as.is=T, na.strings = "undefined" )
  
  name1= strsplit(name1,'.flt')
  write.csv(data1, file = paste(c(name1,".csv"),collapse = ''))
}

