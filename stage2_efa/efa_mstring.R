get_mstring <- function(l, t=.2, bifactor=F, biallvars=T, highord=F) {

  v_max <- length(row.names(l))
  
  Fs_tmp <- colnames(l)[is.element(colnames(l), paste("F", 1:99, sep=""))]
  
  # find and remove cross-loadings
  crossloading <- which(apply(abs(l[,Fs_tmp]) > t, 1, sum) > 1)
  if (length(crossloading) > 0) l <- l[!is.element(rownames(l), names(crossloading)), ]
  
  v <- row.names(l)
  tab <- matrix(nrow=0, ncol=v_max+1)
  
  if (bifactor == T) {
    Fs <- colnames(l)[-1]
    vars <- row.names(l)[which(apply(l >= t, 1, sum) >=1)]
    #vt <- v[(apply(abs(l[,Fs]), 1, max) >= t)]
    
    if (biallvars == F) {
      tab <- rbind(tab, c("R = ", c(paste(vars[1:(length(vars)-1)], " +", sep=""), tail(vars, 1)), rep("", ncol(tab)-length(vars)-1)))
    }
    
    if (biallvars == T) {
      tab <- rbind(tab, c("R = ", c(paste(v[1:(length(v)-1)], " +", sep="")), tail(v, 1)))
    }
    
    model <- paste("R =~ ", paste(v, collapse=" + "), sep="")
  }
  else {
    model <- ""
    Fs <- colnames(l)
  }
  
  # get factor on which each measure has the highest loading
  highest <- apply(l[,Fs], 1, which.max)
  
  for (i in 1:length(Fs)) {
    current <- l[,Fs[i]]
    # remove cross-loadings
    # current[i != highest] <- 0
    vars_incl <- names(which(abs(current) >= t))
    vars_incl <- c(paste(vars_incl[1:(length(vars_incl)-1)], " +"), tail(vars_incl, 1))
    vars_incl[length(vars_incl)] <- gsub(" + ", "", tail(vars_incl, 1), fixed=T)
    row <- c(paste("F", i, " = ", sep=""),
             vars_incl,
             rep("", ncol(tab)-length(vars_incl)-1))
    tab <- rbind(tab, row)
    string <- paste("\nF", i, " =~ ", paste(vars_incl, collapse=""), sep="")
    model <- paste(model, string)
  }
  
  model <- paste(model, "\n")
  return(list(str=model, tab=tab))
}
