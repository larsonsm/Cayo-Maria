cheetah.mm <- function (formula, data, family = NULL, REML = TRUE, pedigree = list(), 
                    control = list(), start = NULL, verbose = FALSE, subset, 
                    weights, na.action, offset, contrasts = NULL, model = TRUE, 
                    x = TRUE, ...) 
{
  require(pedigreemm)
  options(lmerControl=list(check.nobs.vs.nlev="ignore",check.nobs.vs.nRE="ignore"))#))
  gaus <- FALSE
  if (is.null(family)) {
    gaus <- TRUE
  }
  else {
    if (is.character(family)) 
      family <- get(family, mode = "function", envir = parent.frame())
    if (is.function(family)) 
      family <- family()
    if (!inherits(family, "family")) 
      stop("unknown family type")
    gaus <- family$family == "gaussian" && family$link == 
      "identity"
  }
  mc <- match.call()
  lmerc <- mc
  lmerc[[1]] <- if (gaus) 
    as.name("lmer")
  else as.name("glmer")
  lmerc$pedigree <- NULL
  if (!gaus) 
    lmerc$REML <- NULL
  if (!length(pedigree)) 
    return(eval.parent(lmerc))
  
  lmf <- eval(lmerc, parent.frame())
  relfac <- pedigree
  pnms <- names(pedigree)
  pp <- lmf@pp
  resp <- lmf@resp
  fl <- lmf@flist
  stopifnot(all(pnms %in% names(fl)))
  asgn <- attr(fl, "assign")
  Zt <- pp$Zt
  for (i in seq_along(pedigree)) {
    
    if (is.data.frame(pedigree[[i]]))
      pedigree[[i]] <- cheetah.spkin(pedigree[[i]])
    
    tn <- which(match(pnms[i], names(fl)) == asgn)
    if (length(tn) > 1) 
      stop("a pedigree factor must be associated with only one r.e. term")
    ind <- (lmf@Gp)[tn:(tn + 1L)]
    rowsi <- (ind[1] + 1L):ind[2]
    shuffler <- match(rownames(Zt[rowsi, ]), rownames(pedigree[[i]]))
    tmp <- as(pedigree[[i]][shuffler,shuffler],"dsCMatrix")
    relfac[[i]] <- tryCatch( chol(tmp),
         error = function(err) {
                                  s <- svd(tmp)
                                  return(t(s$u %*% diag(sqrt(s$d))))
         })
#     relfac[[i]] <- t(svd(tmp)$u %*% diag(sqrt(svd(tmp)$d)))
    Zt[rowsi, ] <- relfac[[i]] %*% Zt[rowsi, ]
  }
  
  reTrms <- list(Zt = Zt, theta = lmf@theta, Lambdat = pp$Lambdat, 
                 Lind = pp$Lind, lower = lmf@lower, flist = lmf@flist, 
                 cnms = lmf@cnms, Gp = lmf@Gp)
  dfl <- list(fr = lmf@frame, X = pp$X, reTrms = reTrms, start = lmf@theta)
  if (gaus) {
    dfl$REML = resp$REML > 0L
    devfun <- do.call(mkLmerDevfun, dfl)
    opt <- optimizeLmer(devfun, optimizer = "Nelder_Mead", 
                        ...)
  }
  else {
    dfl$family <- family
    devfun <- do.call(mkGlmerDevfun, dfl)
    opt <- optimizeGlmer(devfun, optimizer = "Nelder_Mead", 
                         ...)
  }
  mm <- mkMerMod(environment(devfun), opt, reTrms, lmf@frame, 
                 mc)
  cls <- if (gaus) 
    "lmerpedigreemm"
  else "glmerpedigreemm"
  ans <- do.call(new, list(Class = cls, relfac = relfac, frame = mm@frame, 
                           flist = mm@flist, cnms = mm@cnms, Gp = mm@Gp, theta = mm@theta, 
                           beta = mm@beta, u = mm@u, lower = mm@lower, devcomp = mm@devcomp, 
                           pp = mm@pp, resp = mm@resp, optinfo = mm@optinfo))
  ans@call <- evalq(mc)
  ans
}

cheetah.mc <- function(formula,data,X,n=500,method="fwe",family=NULL,stepdown=T,pedigree,mc.cores=detectCores())
{
  if (!is.list(pedigree) | is.data.frame(pedigree))
    pedigree <- list(ID=pedigree)
  
  if (!is.data.frame(X)) X <- as.data.frame(X)
  
  # convert pedigrees into relatedness matricies
  for (i in 1:length(pedigree))
    if (is.data.frame(pedigree[[i]]))
      pedigree[[i]] <- cheetah.spkin(pedigree[[i]])
  
  # get the mapping from X to y
  if (!is.null(rownames(X)))
  {
    overlap <- intersect(rownames(X),data$ID)
    X <- X[rownames(X) %in% overlap,]
    data <- data[data$ID %in% overlap,]
  }
  
  if (nrow(X)<nrow(data)) {
    x2y <- match(data$ID,rownames(X))
  } else 
    x2y <- 1:nrow(X)
  
  # get observed test statistics
  #   obsOut <- mclapply(X[x2y,],FUN=cheetah.fitnrun,
  #            data=data,formula=formula,pedigree=pedigree,
  #            mc.cores=mc.cores,mc.preschedule=T)
  #   obsT <- sapply(obsOut,function(x) x$tstat)
  #   obsR <- sapply(obsOut,function(x) x$reml)
  obsfit <- mclapply(X[x2y,],FUN=cheetah.fitnrun,
                   data=data,formula=formula,pedigree=pedigree,family=family,
                   mc.cores=mc.cores,mc.preschedule=T)
  xloc <- which(rownames(obsfit[[1]]$coef)=="x")
  obsT <- sapply(obsfit,function(x) x$coef[xloc,3])
  
  maxnullT <- vector()
  if (stepdown)
  {
    ordT <- sort(abs(obsT),index.return=T)
    u <- matrix(nrow=n,ncol=ncol(X))
    colnames(u) <- names(ordT$x)
  }
  
  # sample null model
  for (i in 1:n)
  {
    cat("\rSample ",i)
    perm <- sample(1:nrow(X))
    Xperm <- X[perm,]
    nullOut <- mclapply(Xperm[x2y,],FUN=cheetah.fitnrun,
                     data=data,formula=formula,pedigree=pedigree,family=family,
                     mc.cores=mc.cores,mc.preschedule=T)
    nullT <- sapply(nullOut,function(x) x$coef[xloc,3])
    maxnullT[i] <- max(abs(nullT))
    if (stepdown)
    {
      u[i,1] <- abs(nullT[ordT$ix[1]])
      for (j in 2:length(nullT))
        u[i,j] <- max( abs(nullT[ordT$ix[j]]), u[i,j-1] )
    }
  }
  
  # calculate p-values
  # single-step
  fwe.p.ss <- sapply(abs(obsT),function(x) mean(x<abs(maxnullT)))
  # step-down
  if (stepdown)
  {
    desort <- sort(ordT$ix,index.return=T)$ix
    fwe.p.sd <- sapply(1:ncol(X),function(j) mean(ordT$x[j] < u[,j] ))
    names(fwe.p.sd) <- names(ordT$x)
    for (i in (length(fwe.p.sd)-1):1)
      fwe.p.sd[i] <- max(fwe.p.sd[i],fwe.p.sd[i+1])
    fwe.p.sd <- fwe.p.sd[desort]
  }
  
  out <- list(tstat=obsT,nullstat=maxnullT,fwe.p.ss=fwe.p.ss)
  if (stepdown)
    out <- c(out,list(nullstat.sd=u,fwe.p.sd=fwe.p.sd))
  return(out)
}

cheetah.spkin <- function(pedigree)
{
  require(kinship2)
  
  if (all(c("id","sire","dam") %in% names(pedigree)))
  {
    pedigree <- as(2*kinship(pedigree$id,dadid=pedigree$sire,momid=pedigree$dam),"dsCMatrix")
  } else
    pedigree <- as(2*kinship(pedigree[,1],dadid=pedigree[,2],momid=pedigree[,3]),"dsCMatrix")
  
  return(pedigree)
}

cheetah.parwrap <- function(X,data,formula,pedigree=list(),REML=F,mc.cores=detectCores(),...)
{  
  out <- mclapply(X,FUN=cheetah.fitnrun,formula=formula,data=data,
           pedigree=pedigree,mc.cores=mc.cores,mc.preschedule=T,...)
  
}

cheetah.fitnrun <- function(x,data,formula,pedigree,REML=F,...)
{
  data$x <- x
  out <- list()
  fit <- do.call(cheetah.mm,c(list(formula=formula,data=data,
                            pedigree=pedigree,subset=!is.na(x),REML=REML),list(...)))
  out$coeff <- summary(fit)$coefficients
  out$LL <- summary(fit)$logLik
#   coef <- subset(fit$coefficients,rownames(fit$coefficients) == "x")
#   names(coef) <- colnames(fit$coefficients)
#   out <- c(coef,LL=fit$logLik)
#   if (!is.null(LL0))
#   {
#     p <- 1-pchisq(2*(LL-LL0),1)
#     out <- c(out,pval=p)
#   }
#   return(out)
  return(out)
}
