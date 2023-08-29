library(gdata) # needed for unmatrix()
library(pracma) # needed for cross()
# load the landmark file
filespath <- choose.dir(default = "Select landmark folder", caption = "Select landmark folder")
files.pre <- data.frame(filename = list.files(filespath,pattern = ".fcsv")) #only loads files of type .fcsv

setwd(filespath)
files=list.files(filespath)  

textprompt <- readline("Is your input supposed to be used for training a model or just prediction? Type in 'training' or 'prediction': ")

start.time <- Sys.time() # used to calculate total processing time
preprocessing = function(input){
  data <- read.csv(input, header=FALSE, sep=",")
  #index = which(files==input)
  # crop table (specifically for 3DSlicer markup output files)
  # column names of those columns you want to keep (predictor + coordinate columns)
  newdata <- data[,c("V12","V2","V3","V4")]
  newdata = newdata[-c(1,2,3),]
  rownames(newdata) <- newdata[,1]
  newdata = newdata[,-1]
  newdatacopy <- data.frame(t(newdata)) #generate transposed copy of cropped table to use for angle calculation
  
  # generate euclidian distance matrix
  dist <- dist(newdata,method="euclidean",diag=FALSE,upper=FALSE)
  
  # add combined vector names as column names
  dist2 <- as.matrix(dist,labels=TRUE)
  dist2[lower.tri(dist2,diag=TRUE)] <- NA
  
  dist3 <- unmatrix(dist2)
  dist3 <- as.matrix(dist3)
  dist4 <- dist3[complete.cases(dist3[,1]),]
  dist4 <- as.matrix(t(dist4))
  
  df<-apply(as.matrix.noquote(newdata),2,as.numeric)
  rownames(df) <- row.names(newdata)
  
  # angle between vectors between three 3d coordinates based on triarea function
  angledf <- combn(newdatacopy, 3, simplify=FALSE) #works!
  
  angle <- function(i){ 
    newangledf <- as.data.frame(angledf[i])
    ab <- as.numeric(newangledf[,2])-as.numeric(newangledf[,1])
    ac <- as.numeric(newangledf[,3])-as.numeric(newangledf[,1])
    acos( sum(ab*ac) / ( sqrt(sum(ab * ab)) * sqrt(sum(ac * ac)) ))
  }
  newcombn <- as.data.frame(t(sapply(seq_along(angledf), angle)))
  colnames(newcombn) <- combn(colnames(newdatacopy), 3, paste0, collapse=":")

  # triangle area - thanks to @Ric Villalba on StackOverFlow who helped me when I was stuck with this part!
  tridf <- combn(newdatacopy, 3, simplify=FALSE) #works!
  
  triarea <- function(i){ 
    newtridf <- as.data.frame(tridf[i])
    ab <- as.numeric(newtridf[,2])-as.numeric(newtridf[,1])
    ac <- as.numeric(newtridf[,3])-as.numeric(newtridf[,1])
    c <- as.data.frame(cross(ab,ac))
    area <- 0.5*sqrt(c[1,]^2+c[2,]^2+c[3,]^2)
  }
  
  newcombntri <- as.data.frame(t(sapply(seq_along(tridf), triarea)))
  colnames(newcombntri) <- combn(colnames(newdatacopy), 3, paste0, collapse=":")
  
  colnames(newcombn) <- paste(colnames(newcombn),"angle",sep="_")
  colnames(newcombntri) <- paste(colnames(newcombntri),"area",sep="_")
  colnames(dist4) <- paste(colnames(dist4),"distance",sep = "_")
  
  output <- cbind(dist4,newcombn)
  output <- cbind(output,newcombntri)
  
  # add the sex of the specimen to the first column of output, selects 4th character of filename; only if provided dataset will be used for training
  if(textprompt=="training"){
    sex <- substr(input,4,4)
    output <- cbind(sex,output)
  }
  
  # add specimen numbers as row name, selects first three characters of file name string
  ID <- substr(basename(input),0,3) #if you want to use this: uncomment this and next line. Possibly also change "row.names" to TRUE in both write.tables of the following loop if that approach is what you fancy
  output <- cbind(ID,output)
  
  # append "output" to database of classifiers - file needs to be existing (!) already
  # if not existing yet, generate new empty database by uncommenting the following:
  databasepath <- paste0(filespath,"\\","database")
  suppressWarnings({dir.create(databasepath)}) # creates new directory if not yet existing, suppresses "directory already exists" warnings for better readability
  databasepathandname <- paste0(databasepath,"\\","database.csv")
  
  # warning: if the landmark folder contains landmark sets already included in the database, it will reappend them if the script is ran again as this only checks if the database already exists, not its contents (reading a potentially large database would lead to the preprocessing taking considerably longer)!
  # Only rerun this script to append landmarks not formerly preprocessed! (look at IDs in first column)
  if (file.exists(databasepathandname)) {
    write.table(output, file = databasepathandname, sep = ";",
                append = TRUE, quote = FALSE,
                col.names = FALSE, row.names = FALSE)
  } else {
    write.table(output, file = databasepathandname, sep = ";",
                append = TRUE, quote = FALSE,
                col.names = TRUE, row.names = FALSE)}
}

for (i in 1:length(files)) {
  preprocessing(files[i])
}

end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
print(paste0("The preprocessing of ",length(files)," datasets took ",time.taken," seconds."))