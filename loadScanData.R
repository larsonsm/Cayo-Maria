      
      # set working directory
      setwd("C:/Users/samml/Dropbox/Hurricane Current")
      options(digits=10, round=5)
      # Load libraries
      library(dplyr)
      library(glmmTMB)
      library(missForest)
      
      # Load and clean data
          # Scan data
          allScans    <- read.csv("allScans.csv")
          A1          <- allScans 
          
          allScans    <- allScans[-which(grepl("adult", colnames(allScans)))]
          A1          <- A1[,which(grepl("adult", colnames(A1)))]
          
          for(i in 1:ncol(A1)){A1[,i] <- as.character(A1[,i])}
          
              # Remove Duplicates in scans, recombine data, remove no longer needed data
              for(i in 1:nrow(A1)){
                partnerIDs <-    as.character(A1[i, which(!A1[i,] == "")])
                unqPartIDs <-    sort(unique(partnerIDs))
                length     <-    length(unqPartIDs)
                A1[i,]     <-    ""
                if(length > 0){A1[i,1:length(unqPartIDs)]    <-    unqPartIDs}
              }; A1 <- dplyr::bind_cols(allScans, A1); rm(partnerIDs, unqPartIDs, length, allScans)
        
              A2 <- A1
          
              save.image("Hurricane Workspace.RData")
              
              # Add total, alone, and social counts, both including and excluding their grooming partners
              A2 <- 
                A2 %>% 
                mutate(isSocial = ifelse(activity2 == "Social", 1, 0)) %>% 
                mutate(isRest   = ifelse(activity2 == "Rest"  , 1, 0)) %>% 
                mutate(isTravel = ifelse(activity2 == "Travel", 1, 0)) %>% 
                mutate(isFeed   = ifelse(activity2 == "Feed"  , 1, 0))  
                   
              countI <- numeric(nrow(A2)); for(i in 1:nrow(A2)){countI[i] <- length(which(A2[i,13:21] != ""))  }
              
              A2 <-
                A2 %>% 
                mutate(countI = countI) %>% 
                mutate(countE = ifelse(activity2 == "Social", countI - 1, countI))
              
              A2 <-
                A2 %>% 
                mutate(isAloneI    = ifelse(countI == 0, 1 , 0))  %>% 
                mutate(isAloneE    = ifelse(countE == 0, 1 , 0))  %>% 
                mutate(isNotAloneI = ifelse(isAloneI == 1, 0 ,1)) %>% 
                mutate(isNotAloneE = ifelse(isAloneE == 1, 0 ,1))
              
              A2 <-
                A2 %>%
                mutate(isFeedTime = ifelse(timeBlock == 1, 1, 0))
              
              # Remove white space for focal ID; make sure they are all uppercase
              A2$focalID <- gsub(" ", "", A2$focalID , fixed = TRUE) 
              A2$focalID <- toupper(A2$focalID)
              
              # Add date information
              A2$date  <- lubridate::mdy(as.character(A2$date))
              A2$month <- lubridate::month(A2$date)
              A2$year  <- lubridate::year(A2$date)
              A2$week  <- lubridate::week(A2$date)
              A2$jDay  <- lubridate::yday(A2$date)
              A2$Q     <- lubridate::quarter(A2$date)
              A2$maria <- lubridate::mdy("9/20/2017")
              
              ## Create unique IDs for binning data
              A2$bin    <- paste(A2$focalID, A2$group, A2$Q, A2$isFeedTime, A2$isPost, sep=".")
                
        # Load demographic data
        sexage      <- read.csv("sexage.csv")
        sexage$dob  <- lubridate::mdy(sexage$dob)
        
        # Load and impute behavioral data
        behav       <- read.csv("behav.csv")
        vars <- c("Year", "sdb.ra", "vig.ra", "age", "sex", "rank", "rank2", "rc")
        behavForIMP <- behav[,vars]
        
        behavIMP <- missForest(behavForIMP)
        
        behav$sdb.ra <- behavIMP$ximp$sdb.ra
        behav$vig.ra <- behavIMP$ximp$vig.ra
        behav$rank   <- behavIMP$ximp$rank
        behav$rank2  <- behavIMP$ximp$rank2
        behav$rc     <- behavIMP$ximp$rc
        
        
            # add sex and age information
            A2$isFemale <- sexage$isFemale[match(as.character(A2$focalID), sexage$focalID)]
            A2$age      <- round(as.numeric(A2$date - sexage$dob[match(as.character(A2$focalID), sexage$focalID)])/365, 2)
            
            # add behavioral information
            A2$Find <- paste(A2$focalID, "|" , A2$year, sep="")
            
            A2$rank2 <- behav$rank2[match(A2$Find, behav$Find)]
            A2$rc    <- behav$rc[match(A2$Find, behav$Find)]
          
            head(A2)
        # summarizing bins (creating the A3 file)
            A3 <-
              A2 %>% 
              dplyr::group_by(bin) %>% 
              dplyr::summarise(focalID    = first(focalID)    ,
                               group      = first(group)      ,
                               year       = first(year)       ,
                               observer   = first(observer)   ,
                               isFemale   = first(isFemale)   ,
                               age        = median(age)       ,
                               rank       = first(rank2)      ,
                               rc         = first(rc)         ,
                               Q          = first(Q)          ,
                               isFeed     = first(isFeedTime) ,
                               isPost     = first(isPost)     ,
                               count      = length(bin)       ,
                               isNotAlone = sum(isNotAloneE)  ,
                               isSocial   = sum(isSocial)     ,
                               pNotAlone  = isNotAlone / count,
                               pSocial    = isSocial   / count) %>%
              as.data.frame() %>% 
              dplyr::select(c(-bin))
              
              head(A3)


          