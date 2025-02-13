processSingleScan <- function(filename, absorbanceThreshold = 0.3, includeDate = F, deleteScanFiles = F){
  currentScanFilename <- filename
  currentScanFilenameShort <- filename %>% str_split("/") %>% unlist %>% last
  message("Processing ", currentScanFilenameShort)
  scanHeaderFilename <- str_replace(currentScanFilename, "\\.dat", "\\.hdr")
  file.copy(genericHeader, scanHeaderFilename)
  
  currScan <- hyperSpec::read.ENVI(currentScanFilename, headerfile = scanHeaderFilename)
  
  if(nrow(currScan$spc) == 0){
    scanDetails <- data.frame(date = Sys.time(),
                              filename = currentScanFilenameShort, 
                              sample = NA,
                              object = NA,
                              window = NA,
                              method = NA,
                              mag = NA,
                              scans = NA,
                              res = NA,
                              pix.x = NA, 
                              pix.y = NA, 
                              pix.total = NA,
                              wn.min = NA,
                              wn.max = NA,
                              wn.target = "Deprecated"
    )
  } else {
    atomisedScanFilename <- str_replace(currentScanFilenameShort, "\\.dat", "") %>% str_split("_")%>% unlist
    names(atomisedScanFilename) <- c("sample", "method", "scans", "res", "object")
    scanDetails <- t(data.frame(atomisedScanFilename)) %>% data.frame
    
    if(tolower(scanDetails$method) == "atr"){
      scanDetails$window <- "None"
      scanDetails$mag <- "Norm"
      scanDetails$method <- "ATR"
    } else if(str_detect(tolower(scanDetails$method), "trans")){
      if(str_detect(tolower(scanDetails$method), "h")){
        scanDetails$mag <- "Norm"
      } else {
        scanDetails$mag <- "High"
      }
      scanDetails$window <- str_replace_all(scanDetails$method, "[A-Za-z]", "")
      scanDetails$method <- "Trans"
    } else if(str_detect(tolower(scanDetails$method), "ref")){
      if(str_detect(tolower(scanDetails$method), "h")){
        scanDetails$mag <- "Norm"
      } else {
        scanDetails$mag <- "High"
      }
      scanDetails$window <- "Gold"
      scanDetails$method <- "Ref"
    }
    scanDetails <- scanDetails[c("sample", "object", "window", "method", "mag", "scans", "res")]
    
    scanDetails <- data.frame(date = Sys.time(),
                              filename = currentScanFilenameShort, 
                              scanDetails, 
                              pix.x = max(currScan$x) + 1, 
                              pix.y = max(currScan$y) + 1, 
                              pix.total = nrow(currScan$spc),
                              wn.min = min(currScan@wavelength),
                              wn.max = max(currScan@wavelength),
                              wn.target = "Deprecated"
    )
    row.names(scanDetails) <- NULL
    scanDetails$scans <- strip_non_numbers(scanDetails$scans)
    scanDetails$res <- strip_non_numbers(scanDetails$res)
  }
  if(!includeDate){
    scanDetails$date <- NULL
  }
  currScan$mean <- rowMeans(currScan$spc)
  
  ## for dealing with which pixels to process if some are to high or too low.
  ## this could be a proportion or an absolute value, or values, with a lower and higher value definible
  currThreshold <- ofRange(range(currScan$mean), absorbanceThreshold)
  trimScanIndices <- which(currScan$mean >= currThreshold)
  
  trimDetails <- data.frame(min.abs = min(currScan$mean),
                            max.abs = max(currScan$mean),
                            threshold.abs = currThreshold,
                            threshold.pixels = length(trimScanIndices)
  )
  scanDetails <- cbind(scanDetails, trimDetails)
  
  trimScan <- currScan[trimScanIndices]
  trimScanMeanSpectrum <- colMeans(trimScan$spc)
  names(trimScanMeanSpectrum) <- names(trimScanMeanSpectrum) %>% as.numeric %>% round(1) %>% as.character
  trimScanMeanSpectrum <- trimScanMeanSpectrum %>% t %>% data.frame
  names(trimScanMeanSpectrum) <- str_replace(names(trimScanMeanSpectrum), "X", "wn_")
  if(deleteScanFiles){
    file.remove(currentScanFilename)
    file.remove(scanHeaderFilename)
  }
  list(trimScanMeanSpectrum, scanDetails)
}