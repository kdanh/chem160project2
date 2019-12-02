alpha.m<-0.000006 #(person-1day-1)
alpha.f<-0.0000009
gamma.m<-0.05
gamma.f<-0.007 #(day-1)
Sm<-14000
Sf<-9000
Im<-1000
If<-1000
Sm.hist<-c()
Sf.hist<-c()
Im.hist<-c()
If.hist<-c()
for (day in 1:2000) {
    Sm.hist[day]<-Sm
    Sf.hist[day]<-Sf
    Im.hist[day]<-Im
    If.hist[day]<-If
    delta.Sm<-(gamma.m*Im-alpha.m*Sm*If)
    delta.Im<-(alpha.m*Sm*If-gamma.m*Im)
    delta.Sf<-(gamma.f*If-alpha.f*Sf*Im)
    delta.If<-(alpha.f*Sf*Im-gamma.f*If)
    Sm<-Sm+delta.Sm
    Im<-Im+delta.Im
    Sf<-Sf+delta.Sf
    If<-If+delta.If
    ##Ensure S,I,R>0
    Sm<-max(Sm,0) 
    Sf<-max(Sf,0) 
    Im<-max(Im,0)
    If<-max(If,0)
}
plot(Sm.hist,type="l",ylim=c(0,14000))
lines(Sf.hist,col=4)
lines(Im.hist,col=2)
lines(If.hist,col=3)

