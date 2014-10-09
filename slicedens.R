# This function was originally written by Robert Grant
# http://robertgrantstats.wordpress.com/2014/10/08/slice-bivariate-densities-or-the-joy-division-waterfall-plot/
# I have added some features such as heat map as well as resistence to errors generated form sparse distributions.

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
# NB if you want to cycle slice colors through vectors, you
# need to change the function code; it sounds like a
# pretty bad idea to me, but each to their own.

slicedens<-function(x,y,z=NULL,slices=50,lboost=1,gboost=1,
                    xinc=0,yinc=0.01, bcol="black",fcol="black",
                    lcol="white",lwidth=1) {
  
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
  plot( c(min(x),max(x)+((max(x)-min(x))/4)),
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
    gy<-(i-1)*(height)*yinc
    
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
      polygon(dd$x+gx,lboost*dd$y+gy,col=fcol);
      lines(dd$x+gx,lboost*dd$y+gy,col=lcol,lwd=lwidth);
      })
  }
}


###############################################
# Examples:

n <- 500000; y<-rnorm(n); x<-3*rnorm(n)+y^2
plot(x,y)

par(mar=c(.1,.1,.1,.1))
par(mfrow = c(2,2))

# fcol can either be a color or rgb vector
fcol <- c(.6,0,0,.35)
lcol <- c(.7,1,1,.35)

slicedens(x,y,
          fcol=fcol, bcol='white', lcol=lcol,
          gboost=1)

slicedens(y,x,
          fcol=fcol, bcol='white', lcol=lcol,
          gboost=1.6)

slicedens(x,-y,
          fcol=fcol, bcol='white', lcol=lcol,
          gboost=1)

slicedens(y,-x,
          fcol=fcol, bcol='white', lcol=lcol,
          gboost=1.6)

# Example:
n <- 500000; y<-rnorm(n); x<-rnorm(n)
fcol <- rgb(.5,.5,.5,.5)
slicedens(x,y,
          fcol=fcol, bcol='white', lcol=rgb(0,0,.75,.5),
          gboost=1.6)
slicedens(y,x,
          fcol=fcol, bcol='white', lcol=rgb(0,0,.75,.5),
          gboost=1.6)


n <- 500000; y<-rnorm(n); x<-rnorm(n)-y*.5
fcol <- rgb(.5,.5,0,.5)
slicedens(x,y,
          fcol=fcol, bcol='white', lcol=rgb(0,0,.75,.5),
          gboost=1.6)
slicedens(y,x,
          fcol=fcol, bcol='white', lcol=rgb(0,0,.75,.5),
          gboost=1.6)

n <- 500000; y<-rnorm(n); x<-rnorm(n)+y*.5
fcol <- rgb(0,.5,.5,.5)
slicedens(x,y,
          fcol=, bcol='white', lcol=rgb(0,0,.75,.5),
          gboost=1.6)
slicedens(y,x,
          fcol=fcol, bcol='white', lcol=rgb(0,0,.75,.5),
          gboost=1.6)

fcol <- rbind(c(.6,0,0,.5), c(0,.6,0,.5), c(0,0,.6,.5))
