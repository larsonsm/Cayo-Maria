#### visualizing changes in social rates pre and post hurricane

  fudge <- .15
  col1 <- "#356b86"; col2 <- "#f49634"
  pdf(file="pNotAlone.pdf", width=3.75, height=4.25, onefile = T)
  
    {
      # Group V Stripcharts
      # Post Hurricane
      stripchart(A3$pNotAlone[A3$group=="groupKK" & A3$Q == 1 & A3$isPost == 1], col=adjustcolor(col1, .075), method= "jitter", jitter= .15, vertical =T,
                 xlim=c(0,5), pch = 19, ylim=c(0,1), add=F, at=1 + fudge)
      stripchart(A3$pNotAlone[A3$group=="groupKK" & A3$Q == 2 & A3$isPost == 1], col=adjustcolor(col1, .075), method= "jitter", jitter= .15, vertical =T,
                 xlim=c(0,5), pch = 19, ylim=c(0,1), add=T, at=2 + fudge, axes=F)  
      stripchart(A3$pNotAlone[A3$group=="groupKK" & A3$Q == 3 & A3$isPost == 1], col=adjustcolor(col1, .075), method= "jitter", jitter= .15, vertical =T,
                 xlim=c(0,5), pch = 19, ylim=c(0,1), add=T, at=3 + fudge, axes=F)
      stripchart(A3$pNotAlone[A3$group=="groupKK" & A3$Q == 4 & A3$isPost == 1], col=adjustcolor(col1, .075), method= "jitter", jitter= .15, vertical =T,
                 xlim=c(0,5), pch = 19, ylim=c(0,1), add=T, at=4 + fudge, axes=F)
      
      # Pre Hurricane
      stripchart(A3$pNotAlone[A3$group=="groupKK" & A3$Q == 1 & A3$isPost == 0], col=adjustcolor( col2, .075), method= "jitter", jitter= .15, vertical =T,
                 xlim=c(0,5), pch = 19, ylim=c(0,1), add=T, at=1 - fudge, axes=F)
      stripchart(A3$pNotAlone[A3$group=="groupKK" & A3$Q == 2 & A3$isPost == 0], col=adjustcolor( col2, .075), method= "jitter", jitter= .15, vertical =T,
                 xlim=c(0,5), pch = 19, ylim=c(0,1), add=T, at=2 - fudge, axes=F)  
      stripchart(A3$pNotAlone[A3$group=="groupKK" & A3$Q == 3 & A3$isPost == 0], col=adjustcolor( col2, .075), method= "jitter", jitter= .15, vertical =T,
                 xlim=c(0,5), pch = 19, ylim=c(0,1), add=T, at=3 - fudge, axes=F)
      stripchart(A3$pNotAlone[A3$group=="groupKK" & A3$Q == 4 & A3$isPost == 0], col=adjustcolor( col2, .075), method= "jitter", jitter= .15, vertical =T,
                 xlim=c(0,5), pch = 19, ylim=c(0,1), add=T, at=4 - fudge, axes=F)
      
      # Group V boxplots  
      # Post Hurricane
      boxplot(A3$pNotAlone[A3$group=="groupKK" & A3$Q == 1 & A3$isPost == 1], col=adjustcolor(col1, .75), outline=F, xlim=c(0,5), ylim=c(0,1), add=T, at=1 + fudge, axes=F)
      boxplot(A3$pNotAlone[A3$group=="groupKK" & A3$Q == 2 & A3$isPost == 1], col=adjustcolor(col1, .75), outline=F, xlim=c(0,5), ylim=c(0,1), add=T, at=2 + fudge, axes=F)
      boxplot(A3$pNotAlone[A3$group=="groupKK" & A3$Q == 3 & A3$isPost == 1], col=adjustcolor(col1, .75), outline=F, xlim=c(0,5), ylim=c(0,1), add=T, at=3 + fudge, axes=F)
      boxplot(A3$pNotAlone[A3$group=="groupKK" & A3$Q == 4 & A3$isPost == 1], col=adjustcolor(col1, .75), outline=F, xlim=c(0,5), ylim=c(0,1), add=T, at=4 + fudge, axes=F)
      
      # Pre Hurricane
      boxplot(A3$pNotAlone[A3$group=="groupKK" & A3$Q == 1 & A3$isPost == 0], col=adjustcolor( col2, .8), outline=F, xlim=c(0,5), ylim=c(0,1), add=T, at=1 - fudge, axes=F)
      boxplot(A3$pNotAlone[A3$group=="groupKK" & A3$Q == 2 & A3$isPost == 0], col=adjustcolor( col2, .8), outline=F, xlim=c(0,5), ylim=c(0,1), add=T, at=2 - fudge, axes=F)
      boxplot(A3$pNotAlone[A3$group=="groupKK" & A3$Q == 3 & A3$isPost == 0], col=adjustcolor( col2, .8), outline=F, xlim=c(0,5), ylim=c(0,1), add=T, at=3 - fudge, axes=F)
      boxplot(A3$pNotAlone[A3$group=="groupKK" & A3$Q == 4 & A3$isPost == 0], col=adjustcolor( col2, .8), outline=F, xlim=c(0,5), ylim=c(0,1), add=T, at=4 - fudge, axes=F)
    } # Group KK Boxplots
    {
    # Group V Stripcharts
          # Post Hurricane
          stripchart(A3$pNotAlone[A3$group=="groupV" & A3$Q == 1 & A3$isPost == 1], col=adjustcolor(col1, .075), method= "jitter", jitter= .15, vertical =T,
                     xlim=c(0,5), pch = 19, ylim=c(0,1), add=F, at=1 + fudge)
          stripchart(A3$pNotAlone[A3$group=="groupV" & A3$Q == 2 & A3$isPost == 1], col=adjustcolor(col1, .075), method= "jitter", jitter= .15, vertical =T,
                     xlim=c(0,5), pch = 19, ylim=c(0,1), add=T, at=2 + fudge, axes=F)  
          stripchart(A3$pNotAlone[A3$group=="groupV" & A3$Q == 3 & A3$isPost == 1], col=adjustcolor(col1, .075), method= "jitter", jitter= .15, vertical =T,
                     xlim=c(0,5), pch = 19, ylim=c(0,1), add=T, at=3 + fudge, axes=F)
          stripchart(A3$pNotAlone[A3$group=="groupV" & A3$Q == 4 & A3$isPost == 1], col=adjustcolor(col1, .075), method= "jitter", jitter= .15, vertical =T,
                     xlim=c(0,5), pch = 19, ylim=c(0,1), add=T, at=4 + fudge, axes=F)
          
          # Pre Hurricane
          stripchart(A3$pNotAlone[A3$group=="groupV" & A3$Q == 1 & A3$isPost == 0], col=adjustcolor( col2, .075), method= "jitter", jitter= .15, vertical =T,
                     xlim=c(0,5), pch = 19, ylim=c(0,1), add=T, at=1 - fudge, axes=F)
          stripchart(A3$pNotAlone[A3$group=="groupV" & A3$Q == 2 & A3$isPost == 0], col=adjustcolor( col2, .075), method= "jitter", jitter= .15, vertical =T,
                     xlim=c(0,5), pch = 19, ylim=c(0,1), add=T, at=2 - fudge, axes=F)  
          stripchart(A3$pNotAlone[A3$group=="groupV" & A3$Q == 3 & A3$isPost == 0], col=adjustcolor( col2, .075), method= "jitter", jitter= .15, vertical =T,
                     xlim=c(0,5), pch = 19, ylim=c(0,1), add=T, at=3 - fudge, axes=F)
          stripchart(A3$pNotAlone[A3$group=="groupV" & A3$Q == 4 & A3$isPost == 0], col=adjustcolor( col2, .075), method= "jitter", jitter= .15, vertical =T,
                     xlim=c(0,5), pch = 19, ylim=c(0,1), add=T, at=4 - fudge, axes=F)
      
    # Group V boxplots  
            # Post Hurricane
            boxplot(A3$pNotAlone[A3$group=="groupV" & A3$Q == 1 & A3$isPost == 1], col=adjustcolor(col1, .75), outline=F, xlim=c(0,5), ylim=c(0,1), add=T, at=1 + fudge, axes=F)
            boxplot(A3$pNotAlone[A3$group=="groupV" & A3$Q == 2 & A3$isPost == 1], col=adjustcolor(col1, .75), outline=F, xlim=c(0,5), ylim=c(0,1), add=T, at=2 + fudge, axes=F)
            boxplot(A3$pNotAlone[A3$group=="groupV" & A3$Q == 3 & A3$isPost == 1], col=adjustcolor(col1, .75), outline=F, xlim=c(0,5), ylim=c(0,1), add=T, at=3 + fudge, axes=F)
            boxplot(A3$pNotAlone[A3$group=="groupV" & A3$Q == 4 & A3$isPost == 1], col=adjustcolor(col1, .75), outline=F, xlim=c(0,5), ylim=c(0,1), add=T, at=4 + fudge, axes=F)
    
            # Pre Hurricane
            boxplot(A3$pNotAlone[A3$group=="groupV" & A3$Q == 1 & A3$isPost == 0], col=adjustcolor( col2, .8), outline=F, xlim=c(0,5), ylim=c(0,1), add=T, at=1 - fudge, axes=F)
            boxplot(A3$pNotAlone[A3$group=="groupV" & A3$Q == 2 & A3$isPost == 0], col=adjustcolor( col2, .8), outline=F, xlim=c(0,5), ylim=c(0,1), add=T, at=2 - fudge, axes=F)
            boxplot(A3$pNotAlone[A3$group=="groupV" & A3$Q == 3 & A3$isPost == 0], col=adjustcolor( col2, .8), outline=F, xlim=c(0,5), ylim=c(0,1), add=T, at=3 - fudge, axes=F)
            boxplot(A3$pNotAlone[A3$group=="groupV" & A3$Q == 4 & A3$isPost == 0], col=adjustcolor( col2, .8), outline=F, xlim=c(0,5), ylim=c(0,1), add=T, at=4 - fudge, axes=F)
    } # Group V Boxplots
  
  dev.off(); browseURL("pNotAlone.pdf")
    
    
  
  pdf(file="pSocial.pdf", width=3.75, height=4.25, onefile = T)
  
  {
    # Group V Stripcharts
    # Post Hurricane
    stripchart(A3$pSocial[A3$group=="groupKK" & A3$Q == 1 & A3$isPost == 1], col=adjustcolor(col1, .075), method= "jitter", jitter= .15, vertical =T,
               xlim=c(0,5), pch = 19, ylim=c(0,.3), add=F, at=1 + fudge)
    stripchart(A3$pSocial[A3$group=="groupKK" & A3$Q == 2 & A3$isPost == 1], col=adjustcolor(col1, .075), method= "jitter", jitter= .15, vertical =T,
               xlim=c(0,5), pch = 19, ylim=c(0,.3), add=T, at=2 + fudge, axes=F)  
    stripchart(A3$pSocial[A3$group=="groupKK" & A3$Q == 3 & A3$isPost == 1], col=adjustcolor(col1, .075), method= "jitter", jitter= .15, vertical =T,
               xlim=c(0,5), pch = 19, ylim=c(0,.3), add=T, at=3 + fudge, axes=F)
    stripchart(A3$pSocial[A3$group=="groupKK" & A3$Q == 4 & A3$isPost == 1], col=adjustcolor(col1, .075), method= "jitter", jitter= .15, vertical =T,
               xlim=c(0,5), pch = 19, ylim=c(0,.3), add=T, at=4 + fudge, axes=F)
    
    # Pre Hurricane
    stripchart(A3$pSocial[A3$group=="groupKK" & A3$Q == 1 & A3$isPost == 0], col=adjustcolor( col2, .075), method= "jitter", jitter= .15, vertical =T,
               xlim=c(0,5), pch = 19, ylim=c(0,.3), add=T, at=1 - fudge, axes=F)
    stripchart(A3$pSocial[A3$group=="groupKK" & A3$Q == 2 & A3$isPost == 0], col=adjustcolor( col2, .075), method= "jitter", jitter= .15, vertical =T,
               xlim=c(0,5), pch = 19, ylim=c(0,.3), add=T, at=2 - fudge, axes=F)  
    stripchart(A3$pSocial[A3$group=="groupKK" & A3$Q == 3 & A3$isPost == 0], col=adjustcolor( col2, .075), method= "jitter", jitter= .15, vertical =T,
               xlim=c(0,5), pch = 19, ylim=c(0,.3), add=T, at=3 - fudge, axes=F)
    stripchart(A3$pSocial[A3$group=="groupKK" & A3$Q == 4 & A3$isPost == 0], col=adjustcolor( col2, .075), method= "jitter", jitter= .15, vertical =T,
               xlim=c(0,5), pch = 19, ylim=c(0,.3), add=T, at=4 - fudge, axes=F)
    
    # Group V boxplots  
    # Post Hurricane
    boxplot(A3$pSocial[A3$group=="groupKK" & A3$Q == 1 & A3$isPost == 1], col=adjustcolor(col1, .75), outline=F, xlim=c(0,5), ylim=c(0,1), add=T, at=1 + fudge, axes=F)
    boxplot(A3$pSocial[A3$group=="groupKK" & A3$Q == 2 & A3$isPost == 1], col=adjustcolor(col1, .75), outline=F, xlim=c(0,5), ylim=c(0,1), add=T, at=2 + fudge, axes=F)
    boxplot(A3$pSocial[A3$group=="groupKK" & A3$Q == 3 & A3$isPost == 1], col=adjustcolor(col1, .75), outline=F, xlim=c(0,5), ylim=c(0,1), add=T, at=3 + fudge, axes=F)
    boxplot(A3$pSocial[A3$group=="groupKK" & A3$Q == 4 & A3$isPost == 1], col=adjustcolor(col1, .75), outline=F, xlim=c(0,5), ylim=c(0,1), add=T, at=4 + fudge, axes=F)
    
    # Pre Hurricane
    boxplot(A3$pSocial[A3$group=="groupKK" & A3$Q == 1 & A3$isPost == 0], col=adjustcolor( col2, .8), outline=F, xlim=c(0,5), ylim=c(0,1), add=T, at=1 - fudge, axes=F)
    boxplot(A3$pSocial[A3$group=="groupKK" & A3$Q == 2 & A3$isPost == 0], col=adjustcolor( col2, .8), outline=F, xlim=c(0,5), ylim=c(0,1), add=T, at=2 - fudge, axes=F)
    boxplot(A3$pSocial[A3$group=="groupKK" & A3$Q == 3 & A3$isPost == 0], col=adjustcolor( col2, .8), outline=F, xlim=c(0,5), ylim=c(0,1), add=T, at=3 - fudge, axes=F)
    boxplot(A3$pSocial[A3$group=="groupKK" & A3$Q == 4 & A3$isPost == 0], col=adjustcolor( col2, .8), outline=F, xlim=c(0,5), ylim=c(0,1), add=T, at=4 - fudge, axes=F)
  } # Group KK Boxplots
  {
    # Group V Stripcharts
    # Post Hurricane
    stripchart(A3$pSocial[A3$group=="groupV" & A3$Q == 1 & A3$isPost == 1], col=adjustcolor(col1, .075), method= "jitter", jitter= .15, vertical =T,
               xlim=c(0,5), pch = 19, ylim=c(0,.3), add=F, at=1 + fudge)
    stripchart(A3$pSocial[A3$group=="groupV" & A3$Q == 2 & A3$isPost == 1], col=adjustcolor(col1, .075), method= "jitter", jitter= .15, vertical =T,
               xlim=c(0,5), pch = 19, ylim=c(0,1), add=T, at=2 + fudge, axes=F)  
    stripchart(A3$pSocial[A3$group=="groupV" & A3$Q == 3 & A3$isPost == 1], col=adjustcolor(col1, .075), method= "jitter", jitter= .15, vertical =T,
               xlim=c(0,5), pch = 19, ylim=c(0,1), add=T, at=3 + fudge, axes=F)
    stripchart(A3$pSocial[A3$group=="groupV" & A3$Q == 4 & A3$isPost == 1], col=adjustcolor(col1, .075), method= "jitter", jitter= .15, vertical =T,
               xlim=c(0,5), pch = 19, ylim=c(0,1), add=T, at=4 + fudge, axes=F)
    
    # Pre Hurricane
    stripchart(A3$pSocial[A3$group=="groupV" & A3$Q == 1 & A3$isPost == 0], col=adjustcolor( col2, .075), method= "jitter", jitter= .15, vertical =T,
               xlim=c(0,5), pch = 19, ylim=c(0,1), add=T, at=1 - fudge, axes=F)
    stripchart(A3$pSocial[A3$group=="groupV" & A3$Q == 2 & A3$isPost == 0], col=adjustcolor( col2, .075), method= "jitter", jitter= .15, vertical =T,
               xlim=c(0,5), pch = 19, ylim=c(0,1), add=T, at=2 - fudge, axes=F)  
    stripchart(A3$pSocial[A3$group=="groupV" & A3$Q == 3 & A3$isPost == 0], col=adjustcolor( col2, .075), method= "jitter", jitter= .15, vertical =T,
               xlim=c(0,5), pch = 19, ylim=c(0,1), add=T, at=3 - fudge, axes=F)
    stripchart(A3$pSocial[A3$group=="groupV" & A3$Q == 4 & A3$isPost == 0], col=adjustcolor( col2, .075), method= "jitter", jitter= .15, vertical =T,
               xlim=c(0,5), pch = 19, ylim=c(0,1), add=T, at=4 - fudge, axes=F)
    
    # Group V boxplots  
    # Post Hurricane
    boxplot(A3$pSocial[A3$group=="groupV" & A3$Q == 1 & A3$isPost == 1], col=adjustcolor(col1, .75), outline=F, xlim=c(0,5), ylim=c(0,1), add=T, at=1 + fudge, axes=F)
    boxplot(A3$pSocial[A3$group=="groupV" & A3$Q == 2 & A3$isPost == 1], col=adjustcolor(col1, .75), outline=F, xlim=c(0,5), ylim=c(0,1), add=T, at=2 + fudge, axes=F)
    boxplot(A3$pSocial[A3$group=="groupV" & A3$Q == 3 & A3$isPost == 1], col=adjustcolor(col1, .75), outline=F, xlim=c(0,5), ylim=c(0,1), add=T, at=3 + fudge, axes=F)
    boxplot(A3$pSocial[A3$group=="groupV" & A3$Q == 4 & A3$isPost == 1], col=adjustcolor(col1, .75), outline=F, xlim=c(0,5), ylim=c(0,1), add=T, at=4 + fudge, axes=F)
    
    # Pre Hurricane
    boxplot(A3$pSocial[A3$group=="groupV" & A3$Q == 1 & A3$isPost == 0], col=adjustcolor( col2, .8), outline=F, xlim=c(0,5), ylim=c(0,1), add=T, at=1 - fudge, axes=F)
    boxplot(A3$pSocial[A3$group=="groupV" & A3$Q == 2 & A3$isPost == 0], col=adjustcolor( col2, .8), outline=F, xlim=c(0,5), ylim=c(0,1), add=T, at=2 - fudge, axes=F)
    boxplot(A3$pSocial[A3$group=="groupV" & A3$Q == 3 & A3$isPost == 0], col=adjustcolor( col2, .8), outline=F, xlim=c(0,5), ylim=c(0,1), add=T, at=3 - fudge, axes=F)
    boxplot(A3$pSocial[A3$group=="groupV" & A3$Q == 4 & A3$isPost == 0], col=adjustcolor( col2, .8), outline=F, xlim=c(0,5), ylim=c(0,1), add=T, at=4 - fudge, axes=F)
  } # Group V Boxplots
  
  dev.off(); browseURL("pSocial.pdf")
  