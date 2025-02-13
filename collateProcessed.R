collateProcessed <- function(workDirs = scanFilesDirectories, writeFiles = T){
  currentDir <- getwd()
  setwd(scanFilesRootDirectory)
  output_all_spectra <- vector("list", length(workDirs))
  output_all_metadata <- vector("list", length(workDirs))
  for(i in 1:length(workDirs)){
    print(paste("Compiling", workDirs[i]))
    spectraFilename <- dir(workDirs[i])[which(str_detect(dir(workDirs[i]), "_spectra.csv"))]
    metadataFilename <- dir(workDirs[i])[which(str_detect(dir(workDirs[i]), "_metadata.csv"))]
    output_all_spectra[[i]] <- read.csv(paste0(workDirs[i], "/", spectraFilename))
    md <- read.csv(paste0(workDirs[i], "/", metadataFilename))
    md$dataset <- workDirs[i]
    output_all_metadata[[i]] <- md
  }
  spectra <- do.call(rbind, output_all_spectra)
  metadata <- do.call(rbind, output_all_metadata)
  
  if(writeFiles == T){
    write.csv(spectra, "output_mean_spectra_collated.csv", row.names = F)
    write.csv(metadata, "output_metadata_collated.csv", row.names = F)
  }
  setwd(currentDir)
  list(spectra, metadata)
}