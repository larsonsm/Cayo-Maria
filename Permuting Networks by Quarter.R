
# load libraries
library(dplyr)
library(igraph)
library(tnet)
library(ineq)

# load in pedigree
bigped <- cheetah.spkin(read.csv("bigpedv4.csv"))

# load in data
P1 <- A1

# Calculate Date Information
P1 <- calcDates(P1)   

A1$dateL   <- lubridate::mdy(A1$date)
A1$quarter <- lubridate::quarter(A1$dateL)
P1$quarter <- A1$quarter

# Create dataframe
netIDs <- data.frame(groups = c(rep("groupV", 8), rep("groupKK", 8)), postH = c(rep(1, 4), rep(0,4)), quarter = rep(seq(1:4), 4))
netIDs <- netIDs[-c(8, 13, 16),]


fullFileByQuarter <- NULL   
for(p in 1:nrow(netIDs)){      
  # Reduce file to only what is needed
  scans <- createScans(P1, as.character(netIDs[p,1]), netIDs[p,2], netIDs[p,3])
  
  # Get obervation table
  obsTable <- calcObsTable(scans)
  
  # Get unique IDs
  unqIDs <- calcUnqIDs(scans)
  
  # Get Master Edgelist
  EL   <- calcMasterEL(unqIDs)
  
  # Reduce Pedigree
  ped <- reducePed(bigped, unqIDs)
  
  # Permute 20 Networks
  metrics <- NULL
  runningEL <- NULL
  for(k in 1:500){
    # calculate random scans
    minTimeBlock <-2; maxTimeBlock <- 6; hrs <- sample(1:10, 1)    
    P2 <- calcRandomScansQ(scans, minTimeBlock, maxTimeBlock, hrs, unqIDs)
    
    # Generate edgelist
    options(warn = -1)
    P3 <- calcEdgeList(randomScans = P2, masterEdgeList = EL)
    options(warn = 0, digits = 5)
    
    P3$weight <- round(P3$count / hrs, 5)
    P3$count <- NULL; P3$conc <- NULL
    
    # Update Running edgelist (for drawing the figure)
    # runningEL <- calcRunningEdgeList(runningEL = runningEL, newEL = P3)
    
    # Generate graphs
    netList <- createIG(P3, unqIDs)
    
    # Calculate global network statistics
    dfG <- calcGenStats(netList) 
    
    # Generate Sex-based joint-counts
    dfS <- calcSexProps(netList)
    
    # Generate kin-based joint counts
    dfK <- calcKinProps(netList, ped)    
    
    # Combine metrics   
    df <- bind_cols(dfG, dfS, dfK)
    
    # Load into running dataframe
    metrics <- bind_rows(metrics, df)
  }
  
  metrics$group    <- netIDs[p,1]
  metrics$isPost   <- netIDs[p,2]
  metrics$quarter  <- netIDs[p,3]
  
  metrics$netID <- paste(metrics$group, metrics$isPost, metrics$quarter, sep=".")   
  
  fullFileByQuarter <- bind_rows(fullFileByQuarter, metrics)
}    

write.csv(file = "permutedNetsQ.csv", fullFileByQuarter)      
















