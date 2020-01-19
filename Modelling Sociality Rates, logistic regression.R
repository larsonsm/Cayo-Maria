######## Modelling Logistic Regressions on A2
library(glmmTMB); library(bbmle); set.seed(20); library(DHARMa)

        ### Reduce File to group V andgroup KK, exclude 2013
        T2 <- A2
        T2 <- T2[T2$group == "groupKK" | T2$group == "groupV",]
        T2 <- T2[T2$year  != 2013,]
        
        ### some variables might need to be scaled.
        T2$age <- scale(T2$age)

        ### create training and test sets
        train  <- sample (1:nrow (T2), round (.85 * nrow (T2)))
        T2tr   <- T2[ train,]
        T2te   <- T2[-train,]
        

        ## Combined, P(Accompanied)
        isNotAloneC    <- glmmTMB(isNotAloneE ~ isFemale + age + timeBlock + isPost + poly(Q, 2)*isPost + (1 | year)  + (1 | focalID) + (1 | group),          data = T2tr, ziformula= ~ 0, family = binomial)
        isNotAloneC2   <- glmmTMB(isNotAloneE ~ isFemale + age + timeBlock + isPost + poly(Q, 2):isPost + (1 | year)  + (1 | focalID) + (1 | group),          data = T2tr, ziformula= ~ 0, family = binomial)
        isNotAloneC3   <- glmmTMB(isNotAloneE ~ isFemale + age + poly(timeBlock, 2) + isPost + poly(Q, 2):isPost + (1 | year)  + (1 | focalID) + (1 | group), data = T2tr, ziformula= ~ 0, family = binomial)
        isNotAloneC4   <- glmmTMB(isNotAloneE ~ isFemale + age + poly(timeBlock, 2) + isPost + poly(Q, 2)*isPost + (1 | year)  + (1 | focalID) + (1 | group), data = T2tr, ziformula= ~ 0, family = binomial)

        ## Combined, P(Accompanied)
        isNotAloneC.2    <- glmmTMB(isNotAloneE ~ isFemale + age + timeBlock + isPost + poly(Q, 2)*isPost + (1 | year)  + (1 | focalID) + (1 | group),          data = T2, ziformula= ~ 0, family = binomial)
        isNotAloneC2.2   <- glmmTMB(isNotAloneE ~ isFemale + age + timeBlock + isPost + poly(Q, 2):isPost + (1 | year)  + (1 | focalID) + (1 | group),          data = T2, ziformula= ~ 0, family = binomial)
        isNotAloneC3.2   <- glmmTMB(isNotAloneE ~ isFemale + age + poly(timeBlock, 2) + isPost + poly(Q, 2):isPost + (1 | year)  + (1 | focalID) + (1 | group), data = T2, ziformula= ~ 0, family = binomial)
        isNotAloneC4.2   <- glmmTMB(isNotAloneE ~ isFemale + age + poly(timeBlock, 2) + isPost + poly(Q, 2)*isPost + (1 | year)  + (1 | focalID) + (1 | group), data = T2, ziformula= ~ 0, family = binomial)

                # C4 performs best on the test set, rerunning models with full data
                pred1 <- predict(isNotAloneC, T2te) ; summary(lm(T2te$isNotAloneE ~ pred1))
                pred2 <- predict(isNotAloneC2, T2te); summary(lm(T2te$isNotAloneE ~ pred2))
                pred3 <- predict(isNotAloneC3, T2te); summary(lm(T2te$isNotAloneE ~ pred3))
                pred4 <- predict(isNotAloneC4, T2te); summary(lm(T2te$isNotAloneE ~ pred4))
                
        ### Evaluate the best performing model
        summary(isNotAloneC4.2)
        simres1 <- simulateResiduals(isNotAloneC4.2, n = 1000)
        testResiduals(simres1) 
        MuMIn::r.squaredGLMM(isNotAloneC4.2)
            
        
          
      ## Combined P(Grooming)
      isSocialC    <- glmmTMB(isSocial ~ isFemale + age + timeBlock + isPost + poly(Q, 2)*isPost + (1 | year) + (1 | focalID) + (1 | group),           data = T2tr, ziformula= ~ 0, family = binomial)
      isSocialC2   <- glmmTMB(isSocial ~ isFemale + age + timeBlock + isPost + poly(Q, 2):isPost + (1 | year) + (1 | focalID) + (1 | group),           data = T2tr, ziformula= ~ 0, family = binomial)
      isSocialC3   <- glmmTMB(isSocial ~ isFemale + age + poly(timeBlock, 2) + isPost + poly(Q, 2):isPost + (1 | year)  + (1 | focalID) + (1 | group), data = T2tr, ziformula= ~ 0, family = binomial)
      isSocialC4   <- glmmTMB(isSocial ~ isFemale + age + poly(timeBlock, 2) + isPost + poly(Q, 2)*isPost + (1 | year)  + (1 | focalID) + (1 | group), data = T2tr, ziformula= ~ 0, family = binomial)
      
      isSocialC.2    <- glmmTMB(isSocial ~ isFemale + age + timeBlock + isPost + poly(Q, 2)*isPost + (1 | year) + (1 | focalID) + (1 | group),           data = T2, ziformula= ~ 0, family = binomial)
      isSocialC2.2   <- glmmTMB(isSocial ~ isFemale + age + timeBlock + isPost + poly(Q, 2):isPost + (1 | year) + (1 | focalID) + (1 | group),           data = T2, ziformula= ~ 0, family = binomial)
      isSocialC3.2   <- glmmTMB(isSocial ~ isFemale + age + poly(timeBlock, 2) + isPost + poly(Q, 2):isPost + (1 | year)  + (1 | focalID) + (1 | group), data = T2, ziformula= ~ 0, family = binomial)
      isSocialC4.2   <- glmmTMB(isSocial ~ isFemale + age + poly(timeBlock, 2) + isPost + poly(Q, 2)*isPost + (1 | year)  + (1 | focalID) + (1 | group), data = T2, ziformula= ~ 0, family = binomial)
      
      # C4 performs best on the test set, rerunning models with full data
      pred1 <- predict(isSocialC,  T2te) ; summary(lm(T2te$isSocial ~ pred1))
      pred2 <- predict(isSocialC2, T2te); summary(lm(T2te$isSocial ~ pred2))
      pred3 <- predict(isSocialC3, T2te); summary(lm(T2te$isSocial ~ pred3))
      pred4 <- predict(isSocialC4, T2te); summary(lm(T2te$isSocial ~ pred4))
      
      AICtab(isSocialC, isSocialC2, isSocialC3, isSocialC4)
      summary(isSocialC4)
      simres2 <- simulateResiduals(isSocialC3, n = 1000)
      testResiduals(simres2) 
      soc <- MuMIn::r.squaredGLMM(isSocialC3)
              




      