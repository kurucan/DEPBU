plotQF <- function(quantilef, obs, taus, id, only.future = FALSE, ...)
{
  qf <- quantilef[taus, id]
  medianf <- 	quantilef["50%", id]
  future <- obs[id]
  
  # Plotting
  matplot(id, cbind(future, t(qf)), type = "n", ...)
  if(!only.future){
    x <- seq(ncol(qf))
    xx <- c(x, rev(x))
    
    nbrow <- nrow(qf)
    colorvar <- c("grey", "lightblue", "grey")
    idcol <- 1
    for(row in seq(nbrow, 2, by =-1)){
      yy <- c(qf[row, ], rev(qf[row - 1,]))         
      polygon(xx, yy, col=colorvar[idcol],border=NA)
      idcol <- idcol +1
    }
  }
  points(id, future, pch = 20)
  if(!only.future){
    lines(medianf)
  }
}

# Create nice R figures
savepdf <- function(file, width=16, height=10)
{
  .._fname <- paste(file,".pdf",sep="")
  pdf(.._fname, width=width/2.54, height=height/2.54,
      pointsize=10)
  par(mgp=c(2.2,0.45,0), tcl=-0.4, mar=c(3.3,3.6,1.1,1.1))
  assign(".._fname",.._fname,envir=.GlobalEnv)
}
endpdf <- function()
{
  dev.off()
  system(paste("pdfcrop",.._fname,.._fname))
}
