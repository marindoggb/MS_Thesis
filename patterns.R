
v<-nrow(gognmNA) # number of rows

# the numbers in freqqq come from md.pattern(gognmNA[c(5,6,7,10,18)], rotate.names = TRUE)
freqqq<-c( 2335/v,1015/v,126/v,64/v,642/v,35/v,129/v,22/v,71/v,12/v,31/v)

# Recreate the pattern, #x are how many observations follow said pattern
p1<-c(1,1,1,1,1) 
p2<-c(1,1,1,1,0) #2335
p3<-c(1,1,1,0,1) #1015
p4<-c(1,1,1,0,0) #126
p5<-c(1,1,0,1,1) #64
p6<-c(1,1,0,1,0) #642
p7<-c(1,1,0,0,1) #6
p8<-c(1,1,0,0,0) #35
p9<-c(1,0,1,1,1) #129
p10<-c(1,0,1,1,0) #22
p11<-c(1,0,1,0,1) #7
p12<-c(1,0,0,1,1) #2
p13<-c(1,0,0,1,0)#4
p14<-c(0,1,1,1,1)#71
p15<-c(0,1,1,1,0)#12
p16<-c(0,1,1,0,1)#31
p17<-c(0,1,1,0,0)#4
p18<-c(0,1,0,1,1)#1
p19<-c(0,1,0,1,0)#3
p20<-c(0,1,0,0,1)#1
p21<-c(0,1,0,0,0)#3
p22<-c(0,0,1,1,1)#2
p23<-c(0,0,1,0,1)#3
p24<-c(0,0,0,0,0)#3

#choose the patterns where #>=5
patt<-t(data.frame(p2,p3,p4,p5,p6,p8,p9,p10,p14,p15,p16))



