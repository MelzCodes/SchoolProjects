require("rstudioapi")
require("stringr")
require("raster")
require("rmarkdown")
require("MASS")

# Set the path to the folder containing the PGM files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
tsv_dir <- paste(getwd(), "/images/", sep="")
setwd(file.path(getwd(), "/images"))

# Get a list of all the PGM files in the folder
all_files <- list.files(path = getwd())
pgm_files <- list()

#filter out so its just pgms
for(file in all_files){
  if(str_detect(file, ".pgm")) {
    pgm_files <- append(pgm_files, file)
  }
}

# Initialize an empty list to store the matrices
pgm_matrices <- list()

# Loop through the PGM files
for(pgm in pgm_files){
  
  # Read the PGM file into a data frame
  pgm_data <- read.table(pgm, skip = 4, sep = "", header = F)
  
  # Convert the data frame into a matrix
  tsv_matrix <- matrix(as.matrix(pgm_data), nrow=45, byrow=TRUE)
  
  #Change all values from 255 to 0 and 0 to 1
  
  tsv_matrix[tsv_matrix<128] <- 1 #black pixels
  tsv_matrix[tsv_matrix>128] <- 0 #white pixels
  
  # Add the matrix to the list
  pgm_matrices[[pgm]] <- tsv_matrix
  
  filename <- str_replace(pgm, "pgm", "tsv")
  
  write.matrix(tsv_matrix, file=paste(tsv_dir, filename, sep=""))
  
}