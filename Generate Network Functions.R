# Generating the Network Functions
    # reduce the pedigree to only what is needed
    reducePed       <- function(matrix, unqIDs){
      reduce  <- match(as.character(unqIDs), colnames(matrix))
      matrix2 <- matrix[reduce, reduce]
      return(matrix2)
    }
    
    # calculate date information
    calcDates       <- function(dataframe){
      dataframe$date     <- lubridate::mdy(dataframe$date)
      
      dataframe$month    <- lubridate::month(dataframe$date[i])
      dataframe$day      <- lubridate::day(dataframe$date[i])
      dataframe$julDay   <- lubridate::yday(dataframe$date[i])
      dataframe$wDay     <- lubridate::wday(dataframe$date[i])
      dataframe$quarter  <- lubridate::quarter(dataframe$date[i]) 
      dataframe$week     <- lubridate::week(dataframe$date[i])
      
      return(dataframe)
    }
  
    # Reduce Dataframe to create scans file, use only what's needed
    createScans     <- function(dataframe, group, isPost, quarter){
    HLP1 <- group
    HLP2 <- isPost 
    HLP3 <- quarter
      
    scans <-
      dataframe %>% 
      filter(dataframe$group == HLP1 & dataframe$isPost == HLP2 & dataframe$quarter == HLP3)
      

    scans  <- scans[c("date", "focalID", "timeBlock", "quarter", "activity2", "adult1",  
                      "adult2", "adult3",  "adult4",  "adult5",    "adult6", 
                      "adult7", "adult8",  "adult9")]
        
    return(scans)
    }
    
    # calc Observation Table
    calcObsTable <- function(dataframe){
      unqIDs <-
        dataframe %>%
        group_by(focalID, timeBlock) %>% 
        summarise(count = length(focalID)) %>% 
        as.data.frame()
      
      return(unqIDs)
    }
    
    # Calculate Unique IDs
    calcUnqIDs      <- function(dataframe){
      
      unqIDs <-
        dataframe %>%
        group_by(focalID, timeBlock) %>% 
        summarise(count = length(focalID)) %>% 
        as.data.frame()
      
      unqIDs$hlp <- 1
      
      unqIDs <-
        unqIDs %>% 
        group_by(focalID) %>% 
        summarise(hlp = sum(hlp)) %>%
        filter(!hlp <= 4) %>% 
        as.data.frame()
      
      unqIDs <- as.character(unqIDs$focalID)
      
      return(unqIDs)
      
    }
    
    # Calculate Random Scans
    calcRandomScans <- function(scans ,minTimeBlock, maxTimeBlock, hrs, unqIDs){
        P2 <- NULL
        for(i in 1:length(unqIDs)){
          for(p in 2:6){
            S1 <- scans %>%  filter(focalID == unqIDs[i] & timeBlock == p)
            
            if(nrow(S1) < 6){S1 <- S1[sample(1:nrow(S1), hrs , replace=T),]} else { S1 <- S1[sample(1:nrow(S1), 6 , replace=F),]}
          
            P2 <- bind_rows(P2, S1) 
          }
        }
        return(P2)
    }
    
    # Calculate Master Edgelist
    calcMasterEL    <- function(unqIDs){
      alter <- NULL; ego <- NULL
      for(i in 1:length(unqIDs)){
        alter <- append(alter, rep(unqIDs[i], length(unqIDs) - i))
        ego   <- append(ego  , unqIDs[(i+1):length(unqIDs)])
      }
      ego <- ego[1:length(alter)]
      
      mastEL <- data.frame(alter, ego)
      
      return(mastEL)
    }
    
    # Calculate Edgelist
    calcEdgeList    <- function(randomScans, masterEdgeList){
    
      P2       <- randomScans
      masterEL <- masterEdgeList
      
      P3 <- NULL
      for(i in 1:nrow(P2)){
        for(p in 5:13){
          if(P2[i,p] != ""){
            S1 <- data.frame(ego = as.character(P2[i, 2]), alter = as.character(P2[i, p]))
            P3 <- dplyr::bind_rows(P3, S1)
          }
        }
      }
      
      P3$conc1 <- paste(P3$ego,   P3$alter, sep=".")
      P3$conc2 <- paste(P3$alter, P3$ego  , sep=".")
      masterEL$conc  <- paste(masterEL$alter, masterEL$ego, sep=".")
      masterEL$count <- 0
      head(P3)
      
      head(masterEL)  
      
      for(i in 1:nrow(P3)){
        if(P3$conc1[i] %in% masterEL$conc){
          masterEL$count[which(masterEL$conc == P3$conc1[i])] <- masterEL$count[which(masterEL$conc == P3$conc1[i])] +1
        }
        
        if(P3$conc2[i] %in% masterEL$conc){
          masterEL$count[which(masterEL$conc == P3$conc2[i])] <- masterEL$count[which(masterEL$conc == P3$conc2[i])] +1
        }
      }
      
      return(masterEL)
    }
    
    # Calculate Running Edgelist (For creating summary networks)
    calcRunningEdgeList <- function(runningEL, newEL){
      
      if(k == 1){
        runningEL <- newEL
      } else{
        for(i in 1:nrow(runningEL)){
          runningEL$weight[i] <- round((runningEL$weight[i] + newEL$weight[i]) / 2, 3)
        }
      }
    
    return(runningEL)
    }
    
    # Create igraph and tnet objects
    createIG        <- function(edgelist, unqIDs){
      # create igraph object
      ig <- simplify(graph.data.frame(d=edgelist, directed = F), remove.loops = T)
      ig <- delete.edges(ig, which(E(ig)$weight == 0))
      
      # Add in isolated individuals
      ig <- add.vertices(ig, length(unqIDs[which(!unqIDs %in% V(ig)$name)]), attr = list(name = as.character(unqIDs[which(!unqIDs %in% V(ig)$name)])))
      
      # Add Sex to IG
      ig <- set.vertex.attribute(ig, name = "isFemale", value = sexage$isFemale[match(V(ig)$name, as.character(sexage$focalID))])
      
      # Add Age to IG
      sexage$age <- round(as.numeric(lubridate::mdy("6/1/2018") - sexage$dob) / 365, 2)
      ig <- set.vertex.attribute(ig, name = "age", value = sexage$age[match(V(ig)$name, as.character(sexage$focalID))])
      
      # Create tnet object
      tnet <- cbind(get.edgelist(ig, names=FALSE), E(ig)$weight)
      if(!is.directed(ig)){tnet <- symmetrise_w(tnet)}
      tnet  <- as.tnet(tnet, type="weighted one-mode tnet") 
      
      # create female only networks
      igFem <-  igraph::delete.vertices(ig, which(V(ig)$isFemale==0))    
      igFem <-  delete_vertex_attr(igFem, name="isFemale")
      
      tnetFem <- cbind(get.edgelist(igFem, names=FALSE), E(igFem)$weight)
      if(!is.directed(igFem)){tnetFem <- symmetrise_w(tnetFem)}
      tnetFem  <- as.tnet(tnetFem, type="weighted one-mode tnet") 
      
      # create male only networks
      igMal <-  igraph::delete.vertices(ig, which(V(ig)$isFemale==1))    
      igMal <-  delete_vertex_attr(igMal, name="isFemale")
      
      tnetMal <- cbind(get.edgelist(igMal, names=FALSE), E(igMal)$weight)
      if(!is.directed(igMal)){tnetMal <- symmetrise_w(tnetMal)}
      tnetMal  <- as.tnet(tnetMal, type="weighted one-mode tnet") 
      
      netList <- list(ig, tnet, igFem, tnetFem, igMal, tnetMal)
      return(netList)
    }
    
    # Calculate generic network values
    calcGenStats <- function(netList){
      # total network
      dens    <- length(E(netList[[1]])) / (length(V(netList[[1]]))^2 - length(V(netList[[1]])))
      dens.w  <- sum(E(netList[[1]])$weight) * (length(E(netList[[1]])) / (length(V(netList[[1]]))^2 - length(V(netList[[1]]))))
      gini    <- ineq::ineq(as.numeric(degree(netList[[1]]))  , "gini")     
      gini.w  <- ineq::ineq(as.numeric(strength(netList[[1]])), "gini") 
      kcomm   <- length(fastgreedy.community(delete.vertices(as.undirected(netList[[1]]), degree(netList[[1]]) == 0))[])
      clust.w <- as.numeric(tnet::clustering_w(netList[[2]]))
      
      # female network
      dens.f    <- length(E(netList[[3]])) / (length(V(netList[[3]]))^2 - length(V(netList[[3]])))
      dens.f.w  <- sum(E(netList[[3]])$weight) * (length(E(netList[[3]])) / (length(V(netList[[3]]))^2 - length(V(netList[[3]]))))
      gini.f    <- ineq::ineq(as.numeric(degree(netList[[3]]))  , "gini")     
      gini.f.w  <- ineq::ineq(as.numeric(strength(netList[[3]])), "gini") 
      kcomm.f   <- length(fastgreedy.community(delete.vertices(as.undirected(netList[[3]]), degree(netList[[3]]) == 0))[])
      clust.f.w <- as.numeric(tnet::clustering_w(netList[[4]]))
      
      # male network
      dens.m    <- length(E(netList[[5]])) / (length(V(netList[[5]]))^2 - length(V(netList[[5]])))
      dens.m.w  <- sum(E(netList[[5]])$weight) * (length(E(netList[[5]])) / (length(V(netList[[5]]))^2 - length(V(netList[[5]]))))
      gini.m    <- ineq::ineq(as.numeric(degree(netList[[5]]))  , "gini")     
      gini.m.w  <- ineq::ineq(as.numeric(strength(netList[[5]])), "gini") 
      kcomm.m   <- length(fastgreedy.community(delete.vertices(as.undirected(netList[[5]]), degree(netList[[5]]) == 0))[])
      clust.m.w <- as.numeric(tnet::clustering_w(netList[[6]]))
      
      df <- data.frame(dens, dens.w, gini, gini.w, kcomm, clust.w,
                       dens.f, dens.f.w, gini.f, gini.f.w, kcomm.f, clust.f.w,
                       dens.m, dens.m.w, gini.m, gini.m.w, kcomm.m, clust.m.w)
      
      return(df)
    }

    # Calculate sex-based joint-counts
    calcSexProps <- function(netList){
            # Calculating Sex Pairs
            el            <- data.frame(get.edgelist(netList[[1]]), E(netList[[1]])$weight); colnames(el) <- c("ego", "alter", "weight")
            el$isFemEgo   <- sexage$isFemale[match(as.character(el$ego),   sexage$focalID)]
            el$isFemAlter <- sexage$isFemale[match(as.character(el$alter), sexage$focalID)]
            el$pairClass  <- "opp"; el$pairClass[which(el$isFemEgo == 1 & el$isFemAlter == 1)] <- "bothFem"; el$pairClass[which(el$isFemEgo == 0 & el$isFemAlter == 0)] <- "bothMal"
            
            weight.FF     <- sum(el$weight[el$pairClass=="bothFem"])
            weight.MM     <- sum(el$weight[el$pairClass=="bothMal"])                       
            weight.cross  <- sum(el$weight[el$pairClass=="opp"])                           
            
            possFemPairs        <-    (length(V(netList[[3]]))^2)      - length(V(netList[[3]]))
            possMalPairs        <-    (length(V(netList[[5]]))^2)      - length(V(netList[[5]]))
            allPossiblePairs    <-    (length(V(netList[[1]]))^2)    - length(V(netList[[1]]))
            possCrossPairs      <-    allPossiblePairs - (possFemPairs + possMalPairs)
            
            exp.FF      <- (possFemPairs/allPossiblePairs)    * sum(el$weight)
            exp.MM      <- (possMalPairs/allPossiblePairs)    * sum(el$weight)
            exp.cross   <- (possCrossPairs/allPossiblePairs)  * sum(el$weight)
            
            eo.FF     <- weight.FF/exp.FF
            eo.MM     <- weight.MM/exp.MM
            eo.cross  <- weight.cross/exp.cross
    
            sexPairStats <- data.frame(eo.FF, eo.MM, eo.cross)
            return(sexPairStats)        
}

    # Calculate kin-based joint-counts
    calcKinProps <- function(netList, pedigree){

    el <- data.frame(get.edgelist(netList[[3]]), E(netList[[3]])$weight); colnames(el) <- c("ego", "alter", "weight")
    
    KC      <- NULL; for(i in 1:length(el[,1])){ KC[i] <-  ped[which(rownames(ped)==as.character(el$ego[i])) , which(colnames(ped)==as.character(el$alter[i]))]}
    el$KC   <- round(KC, 4)
    el$pairClass <- "unrelated"
    el$pairClass[which(el$KC >= .125 & el$KC < .25)] <- "dRel"
    el$pairClass[which(el$KC >= .25)] <- "rel"
    
    obs.ck     <- sum( el$weight[el$pairClass =="rel"])
    obs.dk     <- sum( el$weight[el$pairClass =="dRel"])
    obs.u      <- sum( el$weight[el$pairClass =="unrelated"])

    ckPairs    <- length(which(ped  >= .25)) - length(V(netList[[3]]))  ; exp.ck      <- (ckPairs   / length(ped))   * sum(el$weight)
    dkPairs    <- length(which(ped  >= .125 & ped <.25))                ; exp.dk      <- (dkPairs   / length(ped))   * sum(el$weight)
    uPairs     <- length(which(ped  <= .125))                           ; exp.u       <- (uPairs    / length(ped))   * sum(el$weight)

    eo.ck      <- obs.ck    /   exp.ck  
    eo.dk      <- obs.dk    /   exp.dk  
    eo.u       <- obs.u     /   exp.u   

    el$weightKC    <- el$weight * el$KC
    kinDegree      <- sum(el$weightKC) / sum(el$weight)
    
    kinPairStats <- data.frame(eo.ck, eo.dk, eo.u, kinDegree)
    
    return(kinPairStats)
    }

