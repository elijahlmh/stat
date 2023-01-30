#Phase I##########
pfull=read.csv('RESULTS_20617183_2020-08-12.csv',header=T)
plen=pfull$Prev.Length
psize=pfull$Prev.Size
tsize=pfull$Tile.Size
y=pfull$Browse.Time
pmodel=lm(y~plen*psize*tsize)
plen=factor(pfull$Prev.Length,levels=c(-1,1),labels=c('30s','90s'))
psize=factor(pfull$Prev.Size,levels=c(-1,1),labels=c('30%','50%'))
tsize=factor(pfull$Tile.Size,levels=c(-1,1),labels=c('10%','30%'))
summary(pmodel)

#Phase II#########
library(plot3D)
blue_palette=colorRampPalette(c(rgb(247,251,255,maxColorValue = 255),
                                rgb(8,48,107,maxColorValue = 255)))
convert.N.to.C <- function(U,UH,UL){
  x<- (U-(UH+UL)/2) / ((UH-UL)/2)
  return(x)
}

convert.C.to.N <- function(x,UH,UL){
  U <- x*((UH-UL)/2) + (UH+UL)/2
  return(U)
}

data=read.csv('RESULTS_20617183_2020-08-13-1.csv',header=T)
table(data$Prev.Length,data$Prev.Size)
# Calculate beta_PQ and p-value
ph1=data.frame(y=data$Browse.Time,
               x1=convert.N.to.C(U=data$Prev.Length,UH=90,UL=30),
               x2=convert.N.to.C(U=data$Prev.Size,UH=0.5,UL=0.3))
ph1$xPQ=(ph1$x1^2 + ph1$x2^2)/2

aggregate(ph1$y, by=list(x1=ph1$x1, x2=ph1$x2),FUN = mean)
mean(ph1$y[ph1$xPQ != 0]) - mean(ph1$y[ph1$xPQ == 0])

m=lm(y~x1+x2+x1*x2+xPQ, data=ph1)
summary(m)
#p-value >0.01 thus there is no curvature at the center point thus we will steepest method of descent
# Method of steepest Descent
m.fo=lm(y~x1+x2,data=ph1)
summary(m.fo)
beta0=coef(m.fo)[1]
beta1=coef(m.fo)[2]
beta2=coef(m.fo)[3]
g=matrix(c(beta1,beta2),nrow=1)

PL.step = convert.N.to.C(U=60+5, UH=90, UL=30)
lamda=PL.step/abs(beta1)


grd<- mesh(x=seq(convert.N.to.C(U=30,UH=90,UL=30),
                convert.N.to.C(U=120,UH=90,UL=30),
                length.out=100),
          y=seq(convert.N.to.C(U=0.1,UH=0.5,UL=0.3),
                convert.N.to.C(U=0.8,UH=0.5,UL=0.3),
                               length.out=100))
#calcu the est first order response
x1=grd$x
x2=grd$y
eta.fo=beta0+beta1*x1+beta2*x2

# draw the cotour plot
contour(x=seq(convert.N.to.C(U=30,UH=90,UL=30),
              convert.N.to.C(U=120,UH=90,UL=30),
              length.out=100),
        y=seq(convert.N.to.C(U=0.1,UH=0.5,UL=0.3),
              convert.N.to.C(U=0.8,UH=0.5,UL=0.3),
              length.out=100),
        z=eta.fo,xlab='Preview Length',ylab='Preview Size',
        nlevels=15,col=blue_palette(20),labcex = 0.9,asp=1)
abline(a=0,b=beta2/beta1,lty=2)
points(x=0,y=0,col='red',pch=16)

#step0
x.old=matrix(0,nrow=1,ncol=2)
text(x=0,y=0.25,labels='0')
step0=data.frame(Prev.Length=convert.C.to.N(x=0,UH=90,UL=30),
                 Prev.Size=convert.C.to.N(x=0,UH=0.5,UL=0.3))
#step1
x.new=x.old-lamda*g
points(x=x.new[1,1],y=x.new[1,2],col='red',pch=16)
text(x=x.new[1,1],y=x.new[1,2]+0.25,labels='1')
step1=data.frame(Prev.Length=convert.C.to.N(x=x.new[1,1],UH=90,UL=30),
                 Prev.Size=convert.C.to.N(x=x.new[1,2],UH=0.5,UL=0.3))

x.old=x.new
x.new=x.old-lamda*g
points(x=x.new[1,1],y=x.new[1,2],col='red',pch=16)
text(x=x.new[1,1],y=x.new[1,2]+0.25,labels='2')
step2=data.frame(Prev.Length=convert.C.to.N(x=x.new[1,1],UH=90,UL=30),
                 Prev.Size=convert.C.to.N(x=x.new[1,2],UH=0.5,UL=0.3))

x.old=x.new
x.new=x.old-lamda*g
points(x=x.new[1,1],y=x.new[1,2],col='red',pch=16)
text(x=x.new[1,1],y=x.new[1,2]+0.25,labels='3')
step3=data.frame(Prev.Length=convert.C.to.N(x=x.new[1,1],UH=90,UL=30),
                 Prev.Size=convert.C.to.N(x=x.new[1,2],UH=0.5,UL=0.3))

x.old=x.new
x.new=x.old-lamda*g
points(x=x.new[1,1],y=x.new[1,2],col='red',pch=16)
text(x=x.new[1,1],y=x.new[1,2]+0.25,labels='4')
step4=data.frame(Prev.Length=convert.C.to.N(x=x.new[1,1],UH=90,UL=30),
                 Prev.Size=convert.C.to.N(x=x.new[1,2],UH=0.5,UL=0.3))

x.old=x.new
x.new=x.old-lamda*g
points(x=x.new[1,1],y=x.new[1,2],col='red',pch=16)
text(x=x.new[1,1],y=x.new[1,2]+0.25,labels='5')
step5=data.frame(Prev.Length=convert.C.to.N(x=x.new[1,1],UH=90,UL=30),
                 Prev.Size=convert.C.to.N(x=x.new[1,2],UH=0.5,UL=0.3))

x.old=x.new
x.new=x.old-lamda*g
points(x=x.new[1,1],y=x.new[1,2],col='red',pch=16)
text(x=x.new[1,1],y=x.new[1,2]+0.25,labels='6')
step6=data.frame(Prev.Length=convert.C.to.N(x=x.new[1,1],UH=90,UL=30),
                 Prev.Size=convert.C.to.N(x=x.new[1,2],UH=0.5,UL=0.3))

x.old=x.new
x.new=x.old-lamda*g
points(x=x.new[1,1],y=x.new[1,2],col='red',pch=16)
text(x=x.new[1,1],y=x.new[1,2]+0.25,labels='7')
step7=data.frame(Prev.Length=convert.C.to.N(x=x.new[1,1],UH=90,UL=30),
                 Prev.Size=convert.C.to.N(x=x.new[1,2],UH=0.5,UL=0.3))

x.old=x.new
x.new=x.old-lamda*g
points(x=x.new[1,1],y=x.new[1,2],col='red',pch=16)
text(x=x.new[1,1],y=x.new[1,2]+0.25,labels='8')
step8=data.frame(Prev.Length=convert.C.to.N(x=x.new[1,1],UH=90,UL=30),
                 Prev.Size=convert.C.to.N(x=x.new[1,2],UH=0.5,UL=0.3))

x.old=x.new
x.new=x.old-lamda*g
points(x=x.new[1,1],y=x.new[1,2],col='red',pch=16)
text(x=x.new[1,1],y=x.new[1,2]+0.25,labels='9')
step9=data.frame(Prev.Length=convert.C.to.N(x=x.new[1,1],UH=90,UL=30),
                 Prev.Size=convert.C.to.N(x=x.new[1,2],UH=0.5,UL=0.3))

pstd.cond=data.frame(step=0:9,rbind(step0,step1,step2,step3,step4,step5,
                                    step6,step7,step8,step9))
pstd.cond

data.ph2=read.csv('RESULTS_20617183_2020-08-13-2.csv',header=T)
pstd.means=aggregate(data.ph2$Browse.Time,
                     by=list(Prev.Length=data.ph2$Prev.Length,
                             Prev.Size=data.ph2$Prev.Size),
                     FUN=mean)
plot(x=0:9,y=pstd.means$x,
     type='l',xlab='Step Number',ylab='Avg Browsing Time')
points(x=0:9,y=pstd.means$x,
       col='red',pch=16)

#find conditions for step 8
pstd.cond[8,]
#optimal area 85-105, prev size 0.4-0.6

# Step 3
data.ph2.5=read.csv('RESULTS_20617183_2020-08-14-1.csv',header=T)
ph2.5=data.frame(y= data.ph2.5$Browse.Time,
                 x1= convert.N.to.C(U=data.ph2.5$Prev.Length,UH=110,UL=80),
                 x2= convert.N.to.C(U=data.ph2.5$Prev.Size,UH=0.7,UL=0.4))
ph2.5$xPQ=(ph2.5$x1^2 + ph2.5$x2^2)/2
m=lm(y~x1+x2+x1*x2+xPQ,data=ph2.5)
summary(m)
# means pq sig diff from 0, there is curvature, the region contain opt value.
# the p value indicates curvature in the experiment region, 
# we can now conduct RSM on this region

# phase 3#################
library(plot3D)
blue_palette=colorRampPalette(c(rgb(247,251,255,maxColorValue = 255),
                                rgb(8,48,107,maxColorValue = 255)))

convert.N.to.C <- function(U,UH,UL){
  x<- (U-(UH+UL)/2) / ((UH-UL)/2)
  return(x)
}

convert.C.to.N <- function(x,UH,UL){
  U <- x*((UH-UL)/2) + (UH+UL)/2
  return(U)
}

convert.C.to.N(1.41,110,80)
convert.C.to.N(1.41,0.7,0.3)
convert.C.to.N(-1.41,110,80)
convert.C.to.N(-1.41,0.7,0.3)

data3=read.csv('RESULTS_20617183_2020-08-14-2.csv',header=T)
table(data3$Prev.Length,data3$Prev.Size)

condition=data.frame(x1=convert.C.to.N(x=c(-1,-1,1,1,0,1.41,-1.41,0,0),UH=110,UL=80),
                     x2=convert.C.to.N(x=c(-1,1,-1,1,0,0,0,1.41,-1.41),UH=0.7,UL=0.3))
pihat=aggregate(x=data3$Browse.Time,by=list(condition.num=kronecker(1:9,rep(1,250))),FUN=mean)
data.frame(Condition.Num=pihat$condition.num,
           PLen=condition$x1,
           PSize=condition$x2,
           Browse.Time=pihat$x)

ph3=data.frame(y= data3$Browse.Time,
                 x1= convert.N.to.C(U=data3$Prev.Length,UH=110,UL=80),
                 x2= convert.N.to.C(U=data3$Prev.Size,UH=0.7,UL=0.3))

m3=lm(y~x1+x2+x1*x2+I(x1^2)+I(x2^2),data=ph3)
summary(m3)

beta0=coef(m3)[1]
beta1=coef(m3)[2]
beta2=coef(m3)[3]
beta12=coef(m3)[6]
beta11=coef(m3)[4]
beta22=coef(m3)[5]

grd<- mesh(x=seq(convert.N.to.C(U=80,UH=110,UL=80),
                 convert.N.to.C(U=110,UH=110,UL=80),
                 length.out=100),
           y=seq(convert.N.to.C(U=0.3,UH=0.7,UL=0.3),
                 convert.N.to.C(U=0.7,UH=0.7,UL=0.3),
                 length.out=100))

x1=grd$x
x2=grd$y
eta.so=beta0+beta1*x1+beta2*x2+beta12*x1*x2+beta11*x1^2+beta22*x2^2

#2d contour plot
contour(x=seq(convert.N.to.C(U=80,UH=110,UL=80),
              convert.N.to.C(U=110,UH=110,UL=80),
              length.out=100),
        y=seq(convert.N.to.C(U=0.3,UH=0.7,UL=0.3),
              convert.N.to.C(U=0.7,UH=0.7,UL=0.3),
              length.out=100),
        z=eta.so,xlab='Preview Length',ylab='Preview Size',
        nlevels=20,col=blue_palette(20),labcex = 0.9,asp=0.5)
b=matrix(c(beta1,beta2),ncol=1)
B=matrix(c(beta11,0.5*beta12,0.5*beta12,beta22),nrow=2,ncol=2)
x.s <- -0.5*solve(B)%*%b
points(x=x.s[1],y=x.s[2],col='red',pch=16)

# opt point MOI
eta.so.opt =beta0+beta1*x.s[1]+beta2*x.s[2]+beta12*x.s[1]*x.s[2]+beta11*x.s[1]^2+beta22*x.s[2]^2
eta.so.opt
convert.C.to.N(x=x.s[1],UH=110,UL=80)
convert.C.to.N(x=x.s[2,1],UH=0.7,UL=0.3)

# now plot nature units of the opt value
contour(x=seq(80,110,length.out=100),
        y=seq(0.3,0.7,length.out=100),
        z=eta.so,xlab='Preview length',ylab='Preview Size',
        nlevels=20,col=blue_palette(20),labcex=0.9)
points(x=convert.C.to.N(x=x.s[1,1],UH=110,UL=80),
       y=convert.C.to.N(x=x.s[2,1],UH=0.7,UL=0.3),
       col='red',pch=16)

