oldDir <- getwd()
sourceFileDir <- "C:/FTIR/"
setwd(sourceFileDir)
source("processSingleScan.R")
source("processScans.R")
source("collateProcessed.R")
source("Misc.R")

#############################################################################
# The header file is vital for processing, save it into a dedicated directory
# The file is ENVI_generic_header_64x64_1584_normal-mag.hdr
# Other header files can be created in ResolutionsPro from the file menu once a scan has been acquired

# Set the header file (FPA size and number of wavenumbers is relevant)
genericHeader <- paste0(sourceFileDir, "ENVI_generic_header_64x64_1584_normal-mag.hdr")

# Make sure the file exists
file.exists(paste0(sourceFileDir, "ENVI_generic_header_64x64_1584_normal-mag.hdr"))

setwd(oldDir)

##################################################################
# Set the directory containing the scans interactively or manually
scanFilesDirectories <- rstudioapi::selectDirectory()
nestedDirs <- list.dirs(scanFilesDirectories, recursive = F)
scanFilesRootDirectory <- scanFilesDirectories
if(length(nestedDirs) > 0){
  scanFilesDirectories <- nestedDirs
}

############################################################################
# Set the output name of the metadata, spectra and any other files generated
outputFilenameLeader <- NULL

##############################
# Set the absorbance threshold
# Any scan pixels below the absorbance threshold will not be inluded in the average 
# Useful for excluding background, which weakens the average signal especially in transmission scans
absorbanceThreshold = 0.1

########################################
# Execute the function for one directory
I_want_to_delete_the_scans_files_afterwards <- TRUE

processScans(scanFilesDirectories, # the directories to process
             outputFilenameLeader, # gives a specific name for the output, else it uses the directory name
             genericHeader, # the header file for processing, the files must be of the same scan resolution and wavenumbers
             writeFiles = TRUE, # create output files
             absorbanceThreshold = absorbanceThreshold, 
             includeDate = FALSE, # hardcode the date of processing into the metadata
             deleteScanFiles = I_want_to_delete_the_scans_files_afterwards) # delete the scan files once they are processed

## OR ##

# Without saving but assigning as an object

# myScans <- processScans(scanFilesDirectories, 
#                         outputFilenameLeader, 
#                         genericHeader,
#                         absorbanceThreshold = absorbanceThreshold, 
#                         includeDate = FALSE, 
#                         deleteScanFiles = FALSE)

################################################################################
# Collate all the processed data respectively into one spectra and metadata file
# The files will be saved to the original directory
collateProcessed(workDirs = scanFilesDirectories, writeFiles = T)
