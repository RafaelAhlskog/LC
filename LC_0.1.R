
# Necessary packages
if (!require("haven", character.only = TRUE, quietly = TRUE)) install.packages("haven")
if (!require("readxl", character.only = TRUE, quietly = TRUE)) install.packages("readxl")
if (!require("readr", character.only = TRUE, quietly = TRUE)) install.packages("readr")
if (!require("data.table", character.only = TRUE, quietly = TRUE)) install.packages("data.table")
if (!require("svMisc", character.only = TRUE, quietly = TRUE)) install.packages("svMisc")
if (!require("tools", character.only = TRUE, quietly = TRUE)) install.packages("tools")
library(haven)
library(readxl)
library(readr)
library(data.table)
library(svMisc)
library(tools)



######################
# Front-end function #
######################

lc <- function(file, variables, min_dist=0, max_dist=2000, chunk_size=FALSE, xvar="X", yvar="Y",
               sizevar="ruta", groupvar=NULL, includevar="include_grid", granularity=NULL,
               run=TRUE, jackknife=TRUE, merge_file=NULL, centroids=FALSE,
               inverse_order=FALSE, max_k=NULL) {
  
  # Load data and prepare dataset
  lc_dt <- lc_setup(file, variables, granularity, xvar=xvar, yvar=yvar, sizevar=sizevar,
                    centroids=centroids, inverse_order=inverse_order)
  
  # If we are to run the LC calculations, proceed, otherwise return the collapsed file
  if (run==1) {
    
    # If not specified, find correct granularity
    if (is.null(granularity)) {
      granularity <- min(lc_dt$size, na.rm=TRUE)/2
    }
    
    # If there is a separate file to jackknife in, auto-detect include_grids
    do_include <- FALSE
    if (is.null(merge_file)==0) {
      cat("Getting merge IDs...")
      lc_dt <- lc_detectincluded(lc_dt, merge_file, granularity, xvar, yvar, sizevar)
      lc_dt <- lc_dt[order(X,Y)]
      do_include <- TRUE
      cat("Done.\n")
    }
    
    # Run the LCs
    lc_output <- lc_run(lc_dt, variables, granularity, min_dist=min_dist,
                        max_dist=max_dist, chunk_size=chunk_size, do_include=do_include, max_k=max_k)
  
    # Merge and jackknife
    if (jackknife==1) {
      if (is.null(merge_file)) merge_file <- file
      lc_output <- lc_jackknife(lc_output, merge_file, variables, min_dist,
                                max_dist, granularity, xvar, yvar, sizevar, groupvar)
      return(lc_output)
    } else {
      return(lc_output)
    }
  } else {
    return(lc_dt)
  }
}





###########################################################
# This function loads the dataset and returns a collapsed #
# and cleaned version that can be used by lc_run.         #
###########################################################

lc_setup <- function(file, lc_vars, granularity, xvar="X", yvar="Y", sizevar="size", centroids=FALSE, inverse_order=FALSE) {
  
  # Read file
  cat("Reading file... ")
  dt <- lc_load_file(file)
  cat("Complete.\n")
  
  # Prepare file
  cat("Preparing file... ")
  dt <- lc_prep_file(dt, granularity, xvar, yvar, sizevar)
  xvar <- "X"
  yvar <- "Y"
  sizevar <- "size"
  
  # Remove all observations that are missing coordinates
  dt <- dt[!is.na(X) & !is.na(Y) & !is.na(size)]
  
  # Replace with centroids if not already set as centroids
  if (centroids==0) {
    dt[,X_old := X]
    dt[,Y_old := Y]
    dt[,X := X+size/2]
    dt[,Y := Y+size/2]
  }
  
  
  # Non-LC variables to keep
  identical_vars <- c(xvar, yvar, sizevar)
  all_vars <- c(lc_vars, identical_vars)
  
  # Calculate sum and count for specified variables
  dt <- dt[, c(lapply(.SD, mean, na.rm = TRUE), 
               lapply(.SD, function(x) sum(!is.na(x)))), 
           by = .(GRID_ID), .SDcols = all_vars]
  nlen <- ncol(dt)-length(identical_vars)
  dt <- dt[,1:nlen, with=FALSE]
  names(dt) <- c("GRID_ID",paste0(lc_vars,"_sum"),identical_vars,paste0(lc_vars,"_n"))
  for (v in 1:length(lc_vars)) {
    v1 <- paste0(lc_vars[v],"_sum")
    v2 <- paste0(lc_vars[v],"_n")
    dt[, (v1) := get(v1)*get(v2)]
  }
  dt[, include_grid := 1]
  
  # Order properly
  if (inverse_order==FALSE) {
    dt <- dt[order(X,Y)]
  } else {
    dt <- dt[order(Y,X)]
  }
  
  # Return dataset
  cat("Complete.\n")
  return(dt)

}



###################################
# FILE LOADING AND PREP FUNCTIONS #
###################################

# Loads a file
lc_load_file <- function(file) {
  if (file.exists(file)) {

    file_extension <- file_ext(file)
    
    dt <- switch(file_extension,
           dta = read_dta(file),
           xls = read_excel(file),
           xlsx = read_excel(file),
           csv = read_csv(file),
           rds = readRDS(file),
           stop("Not a supported file format")
           )
    dt <- setDT(dt)
    return(dt)
  } else {
    stop(paste("File", file, "does not exist!"))
  }
}


# Prepares a file
lc_prep_file <- function(dt, granularity, xvar, yvar, sizevar) {

  # If xvar or yvar is missing, stop
  if (!(xvar %in% names(dt))) { stop(paste("The variable", xvar, "does not exist in the dataset!")) }
  if (!(yvar %in% names(dt))) { stop(paste("The variable", yvar, "does not exist in the dataset!")) }
  
  # If sizevar is missing, warn and set it to 1 (i.e. assume precise coord)
  if (is.null(sizevar)) {
    cat(paste0("\n   Grid size variable is set to NULL; assuming precise coordinate structure."))
    sizevar <- "size"
    dt[,(sizevar) := 1]    
  } else if (!(sizevar %in% names(dt))) {
    cat(paste0("\n   Note that grid size variable \'",sizevar,"\' does not exist in dataset; assuming precise coordinate structure."))
    dt[,(sizevar) := 1]
  }
  
  # Set the correct names
  setnames(dt, xvar, 'X')
  setnames(dt, yvar, 'Y')
  setnames(dt, sizevar, 'size')
  
  # Drop observations that have missing X, Y or size
  dt <- dt[!is.na(size)]
  dt <- dt[!is.na(X)]
  dt <- dt[!is.na(Y)]
  
  # Check if there are invalid sizes
  if (min(dt$size)<=0) {
    cat("\n   Invalid grids present, dropping.\n")
    dt <- dt[size>0]
  }
  
  # If not specified, find correct granularity
  if (is.null(granularity)) {
    granularity <- min(dt$size, na.rm=TRUE)/2
  }
  
  # Course-graining
  if (granularity>min(dt$size, na.rm=TRUE)/2) {
    cat("\n   Granularity is high; course-graining to specified granularity.\n")
    dt[, size := granularity*2]
    dt[, X := floor(X/(granularity*2))*(granularity*2)]
    dt[, Y := floor(Y/(granularity*2))*(granularity*2)]
  }
  
  # Generate grid identifier
  dt[, GRID_ID := paste(X, Y, sep="_")]

  return(dt)
}


# Use only grids from the separate merge-file
lc_detectincluded <- function(lc_dt, file, granularity, xvar, yvar, sizevar) {
  
  # If include_grid already exists in master file
  if("include_grid" %in% names(lc_dt)) lc_dt[, include_grid := NULL]
  
  # Load and prep new file
  i_dt <- lc_load_file(file)
  i_dt <- lc_prep_file(i_dt, granularity, xvar, yvar, sizevar)
  i_dt <- i_dt[ , .(unique(GRID_ID))]
  setnames(i_dt, old = names(i_dt)[1], new = "GRID_ID")
  i_dt[ , include_grid := 1]
  
  # Merge
  i_dt <- merge(lc_dt, i_dt, by="GRID_ID", all.x=TRUE)
  i_dt[is.na(i_dt$include_grid), include_grid := 0]
  
  return(i_dt)
}



#############################################
# FUNCTIONS FOR RUNNING LCs AND JACKKNIFING #
#############################################

lc_run <- function(lc_dt, lc_vars, min_dist, max_dist, granularity, chunk_size=FALSE, separate_id="include_grid",
                   start_index=0, end_index=NULL, do_include=FALSE, max_k=NULL) {
  
  # Overall coordinate matrix
  cat("Preparing matrices... ")
  C <- cbind(as.matrix(lc_dt[,.(X)]),as.matrix(lc_dt[,.(Y)]))
  
  # Include matrix
  I <- as.matrix(lc_dt[,get(separate_id)])
  
  # If not specified, set chunk size
  chunk_base <- 30
  if (do_include==1) {
    if (chunk_size==FALSE) {
      chunk_size <- round(chunk_base/mean(I))
    } else {
      chunk_size <- round(chunk_size/mean(I))
    }
  } else {
    if (chunk_size==FALSE) chunk_size <- chunk_base
  }
  
  # Find total number of grids, as a constant N, and the total number of
  N <- nrow(lc_dt)
  N_include <- sum(I)
  
  # Set min and max grid range in increases of <granularity> meters
  max_range <- ceiling(max_dist/granularity)
  min_range <- min(max_range, floor(min_dist/granularity) + 1)
  
  # Calculate the maximum number of theoretically possible ordered coordinates
  # away we would ever have to search, as a constant M
  lc_dt[,maxY := uniqueN(Y), by=X]
  M <- max_range * (lc_dt[,max(maxY, na.rm=TRUE)] + 1)
  
  # Set the number of iterations
  iterations <- ceiling(N/chunk_size) - 1
  if (is.null(end_index)) end_index <- iterations
  
  # Output storage
  output_sums  <- array(0, dim=c(N, length(lc_vars), max_range))
  output_ns    <- array(0, dim=c(N, length(lc_vars), max_range))
  output_grids <- array(0, dim=c(N, length(lc_vars), max_range))
  if (do_include==1) {
    output_sums <- output_sums[I[,1],,, drop=FALSE]
    output_ns <- output_ns[I[,1],,, drop=FALSE]
    output_grids <- output_grids[I[,1],,, drop=FALSE]
  }
  
  cat("Complete.\n")
  
  # Timer
  start_time <- Sys.time()
  cat("Starting LC calculations...\n")
  ii_include <- 1
  
  # Iterate through every grid
  for (i in start_index:end_index) {
    
    # Progress output
    progress(i, end_index, progress.bar=TRUE)
    
    # What is the starting index for this iteration?
    ii <- 1 + (i * chunk_size)
    
    # Get the distance matrix for this particular range of indices
    D <- lc_dist_matrix(lc_i_matrix(ii, N, C, chunk_size), lc_j_matrix(ii, N, M, C, chunk_size))
    
    # Check if the next chunk contains anything, otherwise skip this cycle
    if (do_include==1) {
      iit <- min(ii + chunk_size - 1, N)
      if (sum(I[ii:iit,])==0) next
      II <- (as.matrix(I[ii:iit,1, drop=FALSE]))==1
      D <- as.matrix(D[II, , drop=FALSE])
      iit_include <- min(sum(II) + ii_include - 1, N_include)
    }
    
    # For each variable that we want to sum
    for (v in 1:length(lc_vars)) {
      
      # Get the sums and Ns we need
      SUMs <- lc_outcome_space(lc_dt, v, ii, lc_vars[v], "_sum", N, M, chunk_size )
      Ns   <- lc_outcome_space(lc_dt, v, ii, lc_vars[v], "_n", N, M, chunk_size   )
      
      # Drop non-included from both D, SUMs and Ns
      if (do_include==1) {
        SUMs <- as.matrix(SUMs[II, , drop=FALSE])
        Ns <- as.matrix(Ns[II, , drop=FALSE])
      }
      
      # Run over all the ranges
      for (r in min_range:max_range) {
        
        # Create a new matrix from D that contains a binary indicator of
        # whether a given coordinate is included in this range or not.
        # Note here that the threshold is squared. This is an optimization
        # based on the simple fact that the relational statement:
        #   sqrt( (x1-x2)^2 + (y1-y2)^2 ) < threshold
        # is always the same as
        #   (x1-x2)^2 + (y1-y2)^2 < threshold^2 .
        # The lc_dist_matrix_function (below) therefore also omits the sqrt:
        # it is always heavier to compute sqrts for an entire matrix than a
        # simple quare for a single constant.
        threshold <- (r*granularity + 1)^2
        D_bin <- D < threshold
        
        # Multiply the binary over the sums and Ns
        SUMs_mult <- SUMs * D_bin
        Ns_mult   <- Ns * D_bin
        
        # Aggregate
        out_sum   <- as.matrix(rowSums(SUMs_mult, na.rm=TRUE))
        out_n     <- as.matrix(rowSums(Ns_mult, na.rm=TRUE))
        out_grids <- as.matrix(rowSums(D_bin))
        
        # Store
        if (do_include==1) {
          output_sums[ii_include:iit_include, v, r]  <- out_sum
          output_ns[ii_include:iit_include, v, r]    <- out_n
          output_grids[ii_include:iit_include, v, r] <- out_grids
        } else {
          output_sums[ii:lc_i_max(ii, N, chunk_size), v, r]  <- out_sum
          output_ns[ii:lc_i_max(ii, N, chunk_size), v, r]    <- out_n
          output_grids[ii:lc_i_max(ii, N, chunk_size), v, r] <- out_grids
        }
        
        # Optional max_k stopping
        if (!is.null(max_k)) {
          if (min(out_n)>max_k) {
            break
          }
        }
      }
    }
    if (do_include==1) ii_include <- ii_include + sum(II)
  }

  # Print the elapsed time
  elapsed_time <- round(difftime(Sys.time(), start_time, units = "mins"), digits = 2)
  cat("\nDone running LCs! ")
  cat(paste("Running time:", elapsed_time, "minutes.\n"))
  
  # Output
  dt <- lc_dt[,.(GRID_ID)]
  if (do_include==1) {
    I <- as.vector(I)
    dt <- dt[I==1, ]
  }
  for (v in 1:length(lc_vars)) {
    for (r in min_range:max_range) {
      dt[ , (paste0(lc_vars[v],"_r",r,"_sum")) := output_sums[,v,r]]
      dt[ , (paste0(lc_vars[v],"_r",r,"_n")) := output_ns[,v,r]]
      dt[ , (paste0(lc_vars[v],"_r",r,"_grids")) := output_grids[,v,r]]
    }
  }
  
  return(dt)

}


# This function uses the output produced by lc_run and produces jack-knifed
# individual variables in a specified dataset (can be same!)
lc_jackknife <- function(lc_dt, file, lc_vars, min_dist, max_dist, granularity, xvar, yvar, sizevar, groupvar=NULL) {
  
  # Min/max range
  max_range <- ceiling(max_dist/granularity)
  min_range <- min(max_range, floor(min_dist/granularity) + 1)
  
  # Load new individual dataset
  cat("Merging back... ")
  i_dt <- lc_load_file(file)
  i_dt <- lc_prep_file(i_dt, granularity, xvar, yvar, sizevar)
  
  # Merge
  i_dt <- merge(lc_dt, i_dt, by="GRID_ID", all.x=FALSE, all.y=TRUE)
  cat("Complete.\n")
  
  # Create group IDs
  if (!is.null(groupvar)) {
    i_dt[, GROUP_ID := paste(GRID_ID, get(groupvar), sep="_")]
  } else {
    i_dt[, temp := .I]
    i_dt[, GROUP_ID := paste(GRID_ID, temp, sep="_")]
    i_dt[, temp := NULL]
  }
  
  # Jackknife
  cat("Performing jackknife... ")
  max <- max_range*length(lc_vars)
  i <-1
  
  # Go through all variables
  for (v in 1:length(lc_vars)) {
    
    # Correct variable name
    var <- lc_vars[v]
    
    # Group sums and Ns (identical to individual "var" and 1 if groups not used)
    group_sum <- paste0("GROUP_",var,"_sum")
    group_n <- paste0("GROUP_",var,"_n")
    i_dt[, c(group_sum, group_n) := .(sum(get(var), na.rm=TRUE),
                                      sum(!is.na(get(var)))),
          by = GROUP_ID]
    
    # Go through all ranges
    for (r in min_range:max_range) {
      
      # Correct variable name references
      vsum <- paste0(var,"_r",r,"_sum")
      vn <- paste0(var,"_r",r,"_n")
      vmean <- paste0(var,"_r",r,"_mean")
      
      i_dt[!is.na(get(group_sum)), (vsum) := get(vsum)-get(group_sum)]
      i_dt[!is.na(get(group_n)), (vn) := get(vn)-get(group_n)]
      i_dt[get(vn)<1, (vsum) := NA]
      i_dt[get(vn)<1, (vn) := NA]
      i_dt[, (vsum) := get(vsum) / get(vn)]
      setnames(i_dt, vsum, vmean)
      
      # Progress output
      progress(i, max, progress.bar=TRUE)
      i <- i+1
    }
    
    # Cleanup group variables
    i_dt[, (group_sum) := NULL]
    i_dt[, (group_n) := NULL]
  }
  
  # Finish and return
  i_dt[, GROUP_ID := NULL]
  cat("Complete.\n")
  return(i_dt)
}


# This function uses the output and adds a specified minimum knn value.
# This should work on both the "raw" grid level output data, and the jackknifed
# individual level data.
lc_mknn <- function(lc_dt, k, variables, min_dist=0, max_dist=2000, granularity=NULL, exclude_dist=0, exclude_k=0) {
  
  cat("Preparing mKNN calculation...")
  options(scipen = 999)
  
  # If not specified, find correct granularity
  if (is.null(granularity)) {
    granularity <- min(lc_dt$size, na.rm=TRUE)/2
  }
  
  # Min/max range
  max_range <- ceiling(max_dist/granularity)
  min_range <- min(max_range, floor(min_dist/granularity) + 1)
  min_range_n <- min_range + 1
  
  # Run for all specified variables
  cat("\nRunning calculation...")
  vars <- length(variables)
  for (v in 1:vars) {
    
    variable <- variables[v]
    
    # New variable names
    varname_mean  <- paste0("lc",k,"_",variable)
    varname_n     <- paste0("lc",k,"_",variable,"_n")
    varname_grids <- paste0("lc",k,"_",variable,"_grids")
    varname_dist  <- paste0("lc",k,"_",variable,"_dist")
    
    # Existing names
    old_mean  <- paste0(variable,"_r",min_range,"_mean")
    old_n     <- paste0(variable,"_r",min_range,"_n")
    old_grids <- paste0(variable,"_r",min_range,"_grids")
    
    # Create new variables
    lc_dt[, (varname_mean) := get(old_mean)]
    lc_dt[, (varname_n) := get(old_n)]
    lc_dt[, (varname_grids) := get(old_grids)]
    lc_dt[, (varname_dist) := granularity*min_range]
    
    # Go through all ranges
    for (r in min_range_n:max_range) {
      
      old_mean  <- paste0(variable,"_r",r,"_mean")
      old_n     <- paste0(variable,"_r",r,"_n")
      old_grids <- paste0(variable,"_r",r,"_grids")
      
      # Replace if still below threshold k
      lc_dt[ (get(varname_n)<k & !is.na(get(old_mean)) ) , (varname_mean) := get(old_mean)]
      lc_dt[ (get(varname_n)<k & !is.na(get(old_mean)) ) , (varname_grids) := get(old_grids)]
      lc_dt[ (get(varname_n)<k & !is.na(get(old_mean)) ) , (varname_dist) := granularity*r]
      lc_dt[ (get(varname_n)<k & !is.na(get(old_mean)) ) , (varname_n) := get(old_n)]
    }
  }
  
  cat("\nComplete.\n")
  return(lc_dt)
  
}


# Function for getting a specified area size LC
lc_area <- function(lc_dt, radius, variables, granularity=NULL) {
  
  cat("Fetching area...")
  options(scipen = 999)
  
  # If not specified, find correct granularity
  if (is.null(granularity)) {
    granularity <- min(lc_dt$size, na.rm=TRUE)/2
  }
  
  # Correct range
  correct_range <- ceiling(radius/granularity)
  
  # Check if this distance exists
  if (!(paste0(variables[1],"_r",correct_range,"_mean") %in% names(lc_dt))) {
    stop("Specified radius not available in dataset!")
  }
  
  vars <- length(variables)
  for (v in 1:vars) {
  
    varname_mean  <- paste0("r",radius,"_",variable)
    old_mean      <- paste0(variables[1],"_r",correct_range,"_mean")
    lc_dt[, (varname_mean) := get(old_mean)]
    varname_n     <- paste0("r",radius,"_",variable,"_n")
    old_n         <- paste0(variables[1],"_r",correct_range,"_n")
    lc_dt[, (varname_n) := get(old_n)]
    varname_grids <- paste0("r",radius,"_",variable,"_grids")
    old_grids     <- paste0(variables[1],"_r",correct_range,"_grids")
    lc_dt[, (varname_grids) := get(old_grids)]
  
  }
  
  cat("\nComplete.\n")
  return(lc_dt)
  
}



#############################
# Other necessary functions #
#############################

lc_dist_matrix <- function(matrix1,matrix2)     {  return( outer(matrix1[,1],matrix2[,1],"-")^2 + outer(matrix1[,2],matrix2[,2],"-")^2 ) }
lc_i_matrix <- function(start, N, C, chunk)     {  return( C[start:lc_i_max(start, N, chunk), , drop=FALSE] ) }
lc_j_matrix <- function(start, N, M, C, chunk)  {  return( C[lc_j_min(start, M):lc_j_max(start, N, M, chunk), , drop=FALSE] ) }
lc_i_max <-    function(start, N, chunk)        {  return( min(start + chunk - 1, N) ) }
lc_j_min <-    function(start, M)               {  return( max(1, start - M) ) }
lc_j_max <-    function(start, N, M, chunk)     {  return( min(start + chunk + M - 1, N) ) }

# The outcome space is the horizontal outcome-values for all j-dimension grids,
# repeated by row to produce an ixj dimension matrix. Suffix should be "_sum" or "_n".
lc_outcome_space <- function(lc_dt, varnum, start, var, suffix, N, M, chunk) {
  this_mat <- matrix(lc_dt[[ paste0(var,suffix) ]], ncol=1)
  repeats <- min(chunk,N-start+1)
  this_mat <- matrix(1, nrow=repeats, ncol=1) %*% this_mat[ lc_j_min(start, M):lc_j_max(start, N, M, chunk), ]
  return(this_mat)
}

