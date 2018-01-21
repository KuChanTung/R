# ref : http://www.cs.colostate.edu/~anderson/cs545/assignments/digitsInteractiveStart.R
# ref : http://www.cs.colostate.edu/~anderson/cs545/assignments/solutionsGoodExamples/assignment6Muriel.pdf

load("deepnet01.RData")

library(cairoDevice)

drawMatrix <- function(x) {
  image(matrix(x,28,28)[,28:1],col=rev(gray((0:100)/100)),xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
}

drawImage <- function() {
  img <- matrix(0,28,28)
  if (!is.null(coords)) {
    coordsI <- ceiling(coords)
    coordsI[coordsI > 28] <- 28
    coordsI[coordsI < 1] <- 1
    coordsI[,2] <- 29-coordsI[,2]
    counts <- table(coordsI[,1],coordsI[,2])
    img <- matrix(0,28,28)
    #img[as.numeric(rownames(counts)), as.numeric(colnames(counts))] <- 1
    img[as.numeric(rownames(counts)), as.numeric(colnames(counts))] <- counts
    mx <- max(img)
    mn <- min(img)
    img <- (img-mn)*5 / (mx-mn) 
    img[img>=1]=1
  }
  drawMatrix(img)  
  img
}

plotEmpty <- function(a=0,b=1) {
  plot(c(a,b),c(a,b),type="n",bty="n",xaxt="n",yaxt="n",xlab="",ylab="")
}


#x11(type="Xlib",width=3,height=3)
windows()


Cairo(width=3,height=3)  ## for drawing
par(mar=c(0,0,0,0))

coords <- NULL
drawingG <- FALSE
plotEmpty(0,28)

x=c()
getGraphicsEvent("Hold left down to draw. Click right to restart. press <Return/Enter> to read.",
                 onMouseDown = function(buttons,x,y) {
                   if (buttons == 0) {
                     drawingG <<- TRUE
                     NULL
                   } else if (buttons == 2) {
                     coords <<- NULL
                     plotEmpty(0,28)
                     NULL
                   } else if (buttons == 1) {
                     return(TRUE)
                   }
                 },
                 onMouseUp = function(buttons,x,y) {
                   drawingG <<- FALSE
                   # processAndClassify()
                   NULL
                 },
                 onMouseMove = function(buttons,x,y) {
                   if (drawingG) {
                     #print(c(x,y))
                     px <- grconvertX(x, "ndc", "user")
                     py <- grconvertY(y, "ndc", "user")    
                     coords <<- rbind(coords,c(px,py))
                     # print(px,py)
                     points(px,py,pch=19,cex=3)
                   }
                   NULL
                 },
                 onKeybd = function(key) {
                   if (key == "Return") { 
                     dev.set(which=dev.next())
                     x <- t(matrix(drawImage()))*255
                     x <- matrix(c(x), nrow=1)                    
                     #colnames(x) <- colnames(test_h2o)
                     #save(x, file="x.RData")                     
                     #x.h2o <- as.h2o(localH2O, key="x.h2o",x)
                     x.pred.v <- nn.predict(nn, x)    
                     #colnames(x.pred.v)=0:9
                     #barplot(x.pred.v)
                     x.pred <- which(max(x.pred.v)==x.pred.v)-1
                     print(paste("The Number is : ",x.pred, sep=""))
           
                     dev.set(which=dev.prev())                                                            
                   }
                   NULL
                 }
)


dev.off(); dev.off()
