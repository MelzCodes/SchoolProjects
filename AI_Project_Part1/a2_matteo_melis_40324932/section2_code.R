#setwd
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("section1_code.r")

#part2 feature lists
labels = c()
indexs = c()
nr_pix = c()
rows_with_1 = c()
cols_with_1 = c()
rows_with_2 = c()
cols_with_2 = c()
rows_with_3p = c()
cols_with_3p = c()
height = c()
width = c()
aspect_ratio = c()
maxrow = c()
maxcol = c()
connected_areas = c()
eyes = c()
hollowness = c()
custom = c()

#the following functions are used for the custom feature curvature
#they identify diagonal and straight patterns in 3x3 submatrices
diagonals <- function(mat, num_diag) {
  #check for top left to bottom right and top right to bottom left patterns
  if(mat[1,1] == 1 && mat[2,2] == 1 && mat[3,3] == 1){
    num_diag <- num_diag + 1
  } else if(mat[1,3] == 1 && mat[2,2] == 1 && mat[3,1] == 1){
    num_diag <- num_diag + 1
  }else{
  }
  return(num_diag)
}

straights <- function(mat, num_straights) {
  #check for vertical straight and horizontal straight patterns
  if(mat[1,2] == 1 && mat[2,2] == 1 && mat[3,2] == 1){
    num_straights <- num_straights + 1
  }else if(mat[2,1] == 1 && mat[2,2] == 1 && mat[2,3] == 1){
    num_straights <- num_straights + 1
  }else {
  }
  return(num_straights)
}

#first lets get the labels and indexs of each file
for(pgm in pgm_files){
  #str_detect checks if a string has an exact substr
  if(str_detect(pgm, "cherry")) {
    label <- "cherry"
  }
  else if(str_detect(pgm, "banana")) {
    label <- "banana"
  }
  else if(str_detect(pgm, "lemon")) {
    label <- "lemon"
  }
  else if(str_detect(pgm, "tree")) {
    label <- "tree"
  }
  else if(str_detect(pgm, "envelope")) {
    label <- "envelope"
  }
  else if(str_detect(pgm, "golfclub")) {
    label <- "golfclub"
  }
  else if(str_detect(pgm, "pencil")) {
    label <- "pencil"
  }
  else if(str_detect(pgm, "wineglass")) {
    label <- "wineglass"
  }
  labels <- append(labels, label)
  
  #index
  index = 0
  #takes the number before the .pgm part of string
  index <- str_sub(pgm, -6, -5)
  indexs <- append(indexs, index)
}


# Loop through the PGM files
for(tsv_matrix in pgm_matrices){
  
  #01 nr_pix
  nr_p = 0
  #loop through entire matrix and if an entry is 1 add it to a counter
  for(row in 1:nrow(tsv_matrix)){
    
    for(col in 1:ncol(tsv_matrix)){
      if(tsv_matrix[row, col] == 1){
        nr_p <- nr_p + 1
      }
    }
    
  }
  nr_pix <- append(nr_pix, nr_p)
  
  
  #02 rows_with_1
  black_pix = 0
  row_with_1 = 0
  for(row in 1:nrow(tsv_matrix)){
    
    for(col in 1:ncol(tsv_matrix)){
      #if theres a 1 add it to a temp variable
      if(tsv_matrix[row, col] == 1){
        black_pix <- black_pix + 1
      }
    }
    #if there was only one, row only had 1 black pixel
    if(black_pix == 1){
      row_with_1 <- row_with_1 + 1
    }
    #reset when row finishes
    black_pix = 0
  }
  rows_with_1 <- append(rows_with_1, row_with_1)
  
  #03 cols_with_1
  black_pix = 0
  col_with_1 = 0
  for(col in 1:ncol(tsv_matrix)){
    
    for(row in 1:nrow(tsv_matrix)){
      #if theres a 1 add it to a temp variable
      if(tsv_matrix[row, col] == 1){
        black_pix <- black_pix + 1
      }
    }
    #if there was only one, column only had 1 black pixel
    if(black_pix == 1){
      col_with_1 <- col_with_1 + 1
    }
    #reset when column finishes
    black_pix = 0
  }
  cols_with_1 <- append(cols_with_1, col_with_1)
  
  #next four features are identical logic to above just change the if statement
  #so no need to comment
  
  #04 rows_with_2
  black_pix = 0
  row_with_2 = 0
  for(row in 1:nrow(tsv_matrix)){
    
    for(col in 1:ncol(tsv_matrix)){
      
      if(tsv_matrix[row, col] == 1){
        black_pix <- black_pix + 1
      }
    }
    if(black_pix == 2){
      row_with_2 <- row_with_2 + 1
    }
    black_pix = 0
  }
  rows_with_2 <- append(rows_with_2, row_with_2)
  
  #05 cols_with_2
  black_pix = 0
  col_with_2 = 0
  for(col in 1:ncol(tsv_matrix)){
    
    for(row in 1:nrow(tsv_matrix)){
      
      if(tsv_matrix[row, col] == 1){
        black_pix <- black_pix + 1
      }
    }
    if(black_pix == 2){
      col_with_2 <- col_with_2 + 1
    }
    black_pix = 0
  }
  cols_with_2 <- append(cols_with_2, col_with_2)
  
  #06 rows_with_3p
  black_pix = 0
  row_with_3p = 0
  for(row in 1:nrow(tsv_matrix)){
    
    for(col in 1:ncol(tsv_matrix)){
      
      if(tsv_matrix[row, col] == 1){
        black_pix <- black_pix + 1
      }
    }
    if(black_pix >= 3){
      row_with_3p <- row_with_3p + 1
    }
    black_pix = 0
  }
  rows_with_3p <- append(rows_with_3p, row_with_3p)
  
  #07 cols_with_3p
  black_pix = 0
  col_with_3p = 0
  for(col in 1:ncol(tsv_matrix)){
    
    for(row in 1:nrow(tsv_matrix)){
      
      if(tsv_matrix[row, col] == 1){
        black_pix <- black_pix + 1
      }
    }
    if(black_pix >= 3){
      col_with_3p <- col_with_3p + 1
    }
    black_pix = 0
  }
  cols_with_3p <- append(cols_with_3p, col_with_3p)
  
  #08 height
  top = 46
  bottom = -1
  h= 0
  for(row in 1:nrow(tsv_matrix)){
    
    for(col in 1:ncol(tsv_matrix)){
      #if it has a black pixel, check if its the new highest/lowest row
      #if not, no need to continue, break and go to the next row
      if(tsv_matrix[row, col] == 1){
        if(row < top){
          top <- row
        }
        if(row > bottom){
          bottom <- row
        }
        break
      }
    }
  }
  #if it has a top and bottom get the abs difference else keep height as 0
  if(top != 46 && bottom != -1){
    h <- abs(top-bottom)
  }
  height <- append(height, h)
  
  
  #09 width
  left = 46
  right = -1
  w = 0
  for(col in 1:ncol(tsv_matrix)){
    
    for(row in 1:nrow(tsv_matrix)){
      #if it has a black pixel check if its the new leftmost/rightmost col
      #if not, no need to continue, break and go to the next col
      if(tsv_matrix[row, col] == 1){
        if(col < left){
          left <- col
        }
        if(col > right){
          right <- col
        }
        break
      }
    }
  }
  #if it has a left and right get the abs difference else keep width as 0
  if(left != 46 && right != -1){
    w <- abs(right-left)
  }
  width <- append(width, w)
  
  
  #10 aspect_ratio
  ratio = 1
  ratio <- round(w/h, digits = 3)
  
  aspect_ratio <- append(aspect_ratio, ratio)
  
  #11 maxrow
  mrow = 0
  black_pix = 0
  for(row in 1:nrow(tsv_matrix)){
    
    for(col in 1:ncol(tsv_matrix)){
      #count the black pixels in a row
      if(tsv_matrix[row, col] == 1){
        black_pix <- black_pix + 1
      }
    }
    #check if that row had more than our current max and reset the counter
    if(black_pix > mrow){
      mrow <- black_pix
    }
    black_pix = 0
  }
  maxrow <- append(maxrow, mrow)
  
  #12 maxcol
  mcol = 0
  black_pix = 0
  for(col in 1:ncol(tsv_matrix)){
    
    for(row in 1:nrow(tsv_matrix)){
      #count the black pixels in a col
      if(tsv_matrix[row, col] == 1){
        black_pix <- black_pix + 1
      }
    }
    #check if that col had more than our current max and reset the counter
    if(black_pix > mcol){
      mcol <- black_pix
    }
    black_pix = 0
  }
  maxcol <- append(maxcol, mcol)
  
  #13 connected_areas
  
  #converting the 45x45 matrix to raster
  r <- raster(tsv_matrix, xmn=0, xmx=45, ymn=0, ymx=45)
  clumps <- clump(r, directions=8)
  c_areas <- cellStats(clumps, max) #returns the max clump, 
  #they are 1-based indexed so if theres 2 clumps outputs 2 etc
  
  connected_areas <- append(connected_areas, c_areas)
  
  #14 eyes
  
  inverted_matrix <- ifelse(tsv_matrix==0,1,0)
  #converting the 45x45 matrix to raster
  r <- raster(inverted_matrix, xmn=0, xmx=45, ymn=0, ymx=45)
  
  clumps <- clump(r, directions=4) #orthogonal directions only
  
  eye <- cellStats(clumps, max) -1  #returns the max clump, 
  #-1 to get rid of white background case
  
  eyes <- append(eyes, eye)
  
  #15 hollowness
  hollow = 0
  #freq returns information on the number of pixels in clumps
  clumps_info <- freq(clumps)[,2]
  
  #if only two entries in info then no eyes hence no hollowness
  #else take white pixels in each eye and sum them up
  if(length(clumps_info) == 2){
    hollow = 0
  }else {
    index <- clumps_info[3:length(clumps_info)-1]
    white_pix <- sum(index)
    hollow = round(white_pix/nr_p, digits = 3)
  }
  
  hollowness <- append(hollowness, hollow)
  
  #16 custom - curvature
  col_i <- 1 #keeps track of left col
  col_j <- 3 #keeps track of right col
  row_i <- 1 #keeps track of top row
  row_j <- 3 #keeps track of bottom row
  diag <- 0
  strt <- 0
  for(ii in 1:length(tsv_matrix)){
    
    sub_matrix <- tsv_matrix[row_i:row_j,col_i:col_j] #make the current 3x3 mat
    diag <- diagonals(sub_matrix, diag) #check if current mat has diagonal
    strt <- straights(sub_matrix, strt) #check if current mat has straight
    
    #increment left and right column to move our 3x3 matrix over by 1
    col_i <- col_i + 1 
    col_j <- col_j + 1
    
    #this is when we have finished looping through all
    if(row_j == 45 && col_j == 45){
      #if there is very large number of diag or no straight, set curvature to 0
      #else calculate the ratio diag/strt and set it to curvature
      if(strt == 0 | diag >= 90){
        curvature <- 0
      }else {
        curvature <- round(diag/strt, digits=3)
      }
      custom <- append(custom, curvature)
      break
    }
    
    #if we reach the right edge of matrix, reset left and right col and 
    #move down the top and bottom row by one
    if(col_j == 45){
      col_i <- 1
      col_j <- 3 
      row_i <- row_i + 1
      row_j <- row_j + 1
    }
  }
}
csv_df <- data.frame(labels, indexs, nr_pix, rows_with_1, cols_with_1, rows_with_2, cols_with_2, rows_with_3p,
                     cols_with_3p, height, width, aspect_ratio, maxrow, maxcol, connected_areas, eyes, hollowness,
                     custom) 


directory_name = dirname(rstudioapi::getActiveDocumentContext()$path)
file_name = "/40324932_features.csv"

export_path <- paste(directory_name, file_name, sep = "")

write.table(csv_df, export_path, sep=",", row.names = FALSE, quote=FALSE)