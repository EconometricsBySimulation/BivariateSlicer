
# x, y, z: data
# slices: number of horizontal slices through the data
# lboost: coefficient to increase the height of the lines
# gboost: coefficient to increase the height of the graph (ylim)
# xinc: horizontal offset for each succesive slice
# (typically something like 1/80)
# yinc: vertical offset for each succesive slice
# bcol: background color
# fcol: fill color for each slice (polygon)
# lcol: line color for each slice
# lwidth: line width
# cutprop: 
# Vary transarency 
# transprop=FALSE
# tmax = .9
# tmin = .2

slicedens<-function(x,y,z=NULL,slices=50,lboost=1,gboost=1,
                    xinc=0,yinc=0.01, bcol="black", fcol="black",
                    lcol="white",lwidth=1, cutprop=FALSE,
                    transprop=FALSE, tmax = .8, tmin = .05,
                    heightprop=FALSE, xlab=FALSE) {
  
  # This function takes a matrix of one or more rgb sets
  # as well as a degree [0,1] and returns a combined
  # color.
  color.mix <- function(cols, degree=0) {
    if (is.null(nrow(cols))) {
      if (class(cols)=="numeric") 
        return(rgb(cols[1],cols[2],cols[3],cols[4]))
      return(cols)
    }
    # Define a function to find elementwise minimum 
    (deg <- degree*(nrow(cols)-1)+1)
    emin <- function(x, y=0) apply(cbind(x, y), 1, min)
    (r <- 1-emin(abs(1:nrow(cols)-deg),1))
    (comb <- apply(cols*r,2,sum))
    mm <- function(x) max(min(x,1),0)
    rgb(mm(comb[1]),
        mm(comb[2]),
        mm(comb[3]),
        mm(comb[4]))
  }
  
  ycut<-min(y)+((0:(slices))*(max(y)-min(y))/slices)
  
  height<-gboost*((slices*yinc)+max(density(x)$y))
  
  plot( c(min(x)-((max(x)-min(x))/10),max(x)+((max(x)-min(x))/10)),
        c(0,height),
        xaxt="n",yaxt="n",ylab="",xlab="")
  rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col=bcol)
  
  # Calcuate the 'degree' for each z value which will be used to
  # choose the color of each slice.
  if (length(z)==length(y)) {
    zmin <- min(z)
    zmax <- max(z)
    zrange <- max(zmax-zmin)
  }
  
  # Define ifcol and ilfol for later reference.
  # Unless noted otherwise, degree=0
  # Meaning the first color will be selected from
  # the rgb color matrix.
  ifcol <- fcol; ilcol <- lcol;  zdeg <- 0
  
  # Define the degree which is the color degree that each slice will
  # contain
  if (length(z)==length(y)){
    meanz <- NULL
    for(i in slices:1)
      meanz[i]<- mean(z[y>=min(ycut[i]) & y<max(ycut[i+1])])
    zdegree<-(meanz-min(meanz, na.rm=TRUE))/
      (max(meanz, na.rm=TRUE)-min(meanz, na.rm=TRUE))
  }
    
  # Loop through and plot slices
  for(i in slices:1) {
    miny<-ycut[i]
    maxy<-ycut[i+1]
    

    gx<-(i-1)*(max(x)-min(x))*xinc
    
    if (cutprop) {
      slLength <- slices*sum(y>=miny & y<maxy)/length(y)
      if (i==slices) gy <- (i-1)*(height)*yinc
      if (i<slices)  gy <- gyLast-(height)*yinc*slLength
      gyLast <- gy
    }
    else gy<-(i-1)*(height)*yinc
    
    if (transprop) {
      trange <- tmax-tmin
      if (is.null(nrow(ifcol))) 
        ifcol[4] <- min(trange*slices*sum(y>=miny & y<maxy)/length(y)+tmin,tmax)
      if (!is.null(nrow(ifcol))) 
        ifcol[,4] <- min(trange*slices*sum(y>=miny & y<maxy)/length(y)+tmin,tmax)
    }
      
    
    
    # If z is an input vector then grab the color degree from it
    if (length(z)==length(y)) zdeg<-zdegree[i]
    
    # Added the try because distributions without defined upper
    # and lower bounds can give the density function trouble.
    try({
      # Use the color.mixer function to select a color
      fcol<-color.mix(ifcol, zdeg);
      lcol<-color.mix(ilcol, zdeg);
      # Calculte density curves and plot them
      dd<-density(x[y>=miny & y<maxy]);
      if (heightprop) vscale <- lboost*slices*sum(y>=miny & y<maxy)/length(y)
      if (!heightprop) vscale <- lboost
      polygon(dd$x+gx,vscale*dd$y+gy,col=fcol, border=fcol);
      lines(dd$x+gx,vscale*dd$y+gy,col=lcol,lwd=lwidth);
    })
  }
}

n <- 500000; y<-rnorm(n); x<-3*rnorm(n)+y^2
plot(x,y, cex =.2, col=rgb(0,0,0,.1))

par(mar=c(.1,.1,.1,.1))
par(mfrow = c(2,2))

# Plot out the distribution using MASS density mapping
library(MASS)
den3d <- kde2d(x, y)
persp(den3d, box=FALSE, expand=.6)
persp(den3d, box=FALSE, expand=.6, theta=-90)
persp(den3d, box=FALSE, expand=.6, theta=180)
persp(den3d, box=FALSE, expand=.6, theta=90)

# fcol can either be a color or rgb vector
fcol <- c(.6,0,0,.35)
lcol <- c(.7,1,1,.35)

slicedens(x,y,
          fcol=fcol, bcol='white', lcol=lcol,
          gboost=1)

slicedens(y,x,
          fcol=fcol, bcol='white', lcol=lcol,
          gboost=1.6)

slicedens(-x,-y,
          fcol=fcol, bcol='white', lcol=lcol,
          gboost=1)

slicedens(-y,-x,
          fcol=fcol, bcol='white', lcol=lcol,
          gboost=1.6)

# Proportionally distanced cuts
slicedens(x,y, cutprop=T,
          fcol=fcol, bcol='white', lcol=lcol,
          gboost=1)

slicedens(y,x, cutprop=T,
          fcol=fcol, bcol='white', lcol=lcol,
          gboost=1.6)

slicedens(-x,-y, cutprop=T,
          fcol=fcol, bcol='white', lcol=lcol,
          gboost=1)

slicedens(-y,-x, cutprop=T,
          fcol=fcol, bcol='white', lcol=lcol,
          gboost=1.6)

# Transparency weighted cuts
slicedens(x,y, transprop=T,
          fcol=fcol, bcol='white', lcol=lcol,
          gboost=1)

slicedens(y,x, transprop=T,
          fcol=fcol, bcol='white', lcol=lcol,
          gboost=1.6)

slicedens(-x,-y, transprop=T,
          fcol=fcol, bcol='white', lcol=lcol,
          gboost=1)

slicedens(-y,-x, transprop=T,
          fcol=fcol, bcol='white', lcol=lcol,
          gboost=1.6)

# Height weighted cuts
fcol <- c(.6,0,0,.2)
lcol <- c(.7,1,1,.4)

slicedens(x,y, heightprop=T,
          fcol=fcol, bcol='white', lcol=lcol,
          gboost=1.3)

slicedens(y,x, heightprop=T,
          fcol=fcol, bcol='white', lcol=lcol,
          gboost=3)

slicedens(-x,-y, heightprop=T,
          fcol=fcol, bcol='white', lcol=lcol,
          gboost=1.3)

slicedens(-y,-x, heightprop=T,
          fcol=fcol, bcol='white', lcol=lcol,
          gboost=4)

# Three variables
z <- -(abs(x)+abs(y))+rnorm(n)*3
fcol <- rbind(c(0,.1,.5,.5), c(.3,.8,.8,.5), c(1,1,0,.5))
lcol <- rbind(c(0,.3,.3,.8), c(.1,.1,.2,.7), c(0,0,1,.65))


slicedens(x,y,z,
          fcol=fcol, bcol='white', lcol=lcol,
          gboost=1)

slicedens(y,x,z, 
          fcol=fcol, bcol='white', lcol=lcol,
          gboost=1.6)

slicedens(z,y,x,
          fcol=fcol, bcol='white', lcol=lcol,
          gboost=1)

slicedens(z,x,y,
          fcol=fcol, bcol='white', lcol=lcol,
          gboost=1)

# Example:
fcol <- rgb(0,.5,.5,.5)
n <- 500000; y<-rnorm(n); x<-rnorm(n)
slicedens(x,y,
          fcol=fcol, bcol='white', lcol=rgb(0,0,.75,.5),
          gboost=1.6)
# 
n <- 500000; y<-rnorm(n); x<-rnorm(n)+y*.5
slicedens(x,y,
          fcol=fcol, bcol='white', lcol=rgb(0,0,.75,.5),
          gboost=1.6)

n <- 500000; y<-rnorm(n); x<-rnorm(n)-y*.5
slicedens(x,y,
          fcol=fcol, bcol='white', lcol=rgb(0,0,.75,.5),
          gboost=1.6)

n <- 500000; y<-x<-rnorm(n)
slicedens(x,y,
          fcol=fcol, bcol='white', lcol=rgb(0,0,.75,.5),
          gboost=15)

# I downloaded the IPUMS data from https://www.ipums.org/
# I selected age wage income and person weight. 
# You will need to download your own data if you want to do this part.
# The challenge with the data is that it is in fixed width 
# format so you may need to customize this read.fwf command.
ipums <- read.fwf("C:/Data/usa_00028.dat/usa_00028.dat", c(10,3,6))
head(ipums)
names(ipums) <- c('pwght', 'age', 'wginc')

ipums <- ipums[ipums$wginc!=999999 & ipums$wginc!=0,]
plot(density(log(ipums$wginc[ipums$age==16])))

par(mar=c(.1,.1,.1,.1))
par(mfrow = c(2,2))


fcol <- c(.3,.8,.8,.1); lcol <- c(.5,1,1,.2)

slicedens(log(ipums$wginc), ipums$age,
          fcol=fcol, bcol='black', lcol=lcol,
          gboost=.8)

slicedens(log(ipums$wginc), -ipums$age,
          fcol=fcol, bcol='black', lcol=lcol,
          gboost=.8)

slicedens(ipums$age, log(ipums$wginc),
          fcol=fcol, bcol='black', lcol=lcol,
          gboost=.2)

slicedens(log(ipums$wginc), ipums$age, heightprop=TRUE,
          fcol=fcol, bcol='black', lcol=lcol,
          gboost=2)
