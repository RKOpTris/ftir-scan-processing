processScans <- function(scanFilesDirectories, outputFilenameLeader, genericHeader, writeFiles = T, absorbanceThreshold = 0.3, includeDate = F, deleteScanFiles = F){
  outputFilenameLeaderWasNULL <- is.null(outputFilenameLeader)
  if(!is.null(outputFilenameLeader)){
    fileEquivalence <- length(scanFilesDirectories) == length(outputFilenameLeader)
    if(!fileEquivalence){
      stop("Each path in scanFilesDirectories requires a respective outputFilenameLeader!")
    }
  }
  for(i in 1:length(scanFilesDirectories)){
    scanFilesDirectory <- scanFilesDirectories[i]
    if(!is.null(outputFilenameLeader)){
      outputFilenameLeader <- outputFilenameLeader[i]
    }
    message("Processing ", i, " of ", length(scanFilesDirectories), " scan directories...")
    scansToProcess <- list.files(scanFilesDirectory, "\\.dat", full.names = T)
    message("Processing ", length(scansToProcess), " scan(s) using an absorbance threshold of ", absorbanceThreshold)
    message("=========================================================================")
    processingOutput <- new.env()
    
    op <- suppressWarnings(parallel::mclapply(scansToProcess, processSingleScan, absorbanceThreshold = absorbanceThreshold, includeDate = includeDate, deleteScanFiles = deleteScanFiles))
    
    processingOutput$spectra <- lapply(op, "[[", 1) %>% do.call(what = rbind)
    processingOutput$metadata <- lapply(op, "[[", 2) %>% do.call(what = rbind)
    
    opSpectra <- processingOutput$spectra
    processingOutput$spectra <- opSpectra[complete.cases(opSpectra), ]
    processingOutput$spectra <- data.frame(X = paste0("s", str_pad(1:nrow(processingOutput$spectra), pad = "0", width = 3)),
                                           processingOutput$spectra)
    
    opMetadata <- processingOutput$metadata
    processingOutput$failed <- opMetadata[!complete.cases(opMetadata), ]$filename
    processingOutput$metadata <- opMetadata[complete.cases(opMetadata), ]
    processingOutput$metadata <- data.frame(id = processingOutput$spectra$X,
                                            processingOutput$metadata)
    
    if(writeFiles){
      message("=========================================================================")
      message("Working in: ", scanFilesDirectory)
      if(is.null(outputFilenameLeader)){
        outputFilenameLeader <- str_split(scanFilesDirectory, "/") %>% unlist %>% last
      }
      opFilename <- paste0(scanFilesDirectory, "/", outputFilenameLeader, "_metadata.csv")
      write.csv(processingOutput$metadata, opFilename, row.names = F)
      message(outputFilenameLeader, "_metadata.csv", " created.")
      opFilename <- paste0(scanFilesDirectory, "/", outputFilenameLeader, "_spectra.csv")
      write.csv(processingOutput$spectra, opFilename, row.names = F)
      message(outputFilenameLeader, "_spectra.csv", " created.")
      if(length(processingOutput$failed) > 0){
        opFilename <- paste0(scanFilesDirectory, "/", outputFilenameLeader, "_failed.txt")
        writeLines(processingOutput$failed, opFilename)
        message(length(processingOutput$failed), " scan(s) could not be processed. ", outputFilenameLeader, "_failed.txt created.")
      }
      if(outputFilenameLeaderWasNULL){
        outputFilenameLeader <- NULL
      }
      message("=========================================================================")
    } else {
      processingOutput
    }
  }
}