#Set number of frames
N=300

#Read the data
t=read.table("../results/t.txt")
x=read.table("../results/x.txt")
y=read.table("../results/y.txt")
vx=read.table("../results/vx.txt")
vy=read.table("../results/vy.txt")
ax=read.table("../results/ax.txt")
ay=read.table("../results/ay.txt")

#Create a list of names and colors
names = c("Sol","Mercurio","Venus","Tierra","Marte","Júpiter","Saturno","Urano","Neptuno","Plutón")
colrs = c("yellow","brown","orange","blue","coral1","bisque1","burlywood2","cadetblue3","cornflowerblue","burlywood4")
sizes = c(3.2,0.5,0.95,1,0.53,2.4,2.4,2.4,2,0.7)
for(i in 1:length(sizes)) {
  sizes[i] = sizes[i]*1.1^i
}


#This is optional but will help with the visualization. We will make the
#animation zoom out by setting a different xlim and ylim in each iteration.
#We will also make the size of each planet smaller as we zoom out.
limit=seq(1,38,len=N)
diminish = seq(1,0.5,len=N)
diminish = diminish^3 #Qubic scale factor

#Create a directory to save the frames
dir.create("../dynamic")

#Make a big enough blank plot for all the trajectories
for(i in 1:N) { #Number of frames
  if(i%%1 == 0) { #Sample every X frames in case there are too many of them
    
    #Create the .png file for each frame
    png(paste("../dynamic/frame",i,".png",sep=""),width=500,height=500)
    
    #Set the aesthetics for the animation
    par(bg="black",col.lab="white",col.axis="white",col.main="white",cex.main=1.3,pch=16)
    
    #Plot an empty graph
    plot(0,0,type="p",xlim=c(-limit[i],limit[i]),ylim=c(-limit[i],limit[i]),asp=1,main="Simulación: sistema solar (heliocéntrico)",xlab="x (ua)",ylab="y (ua)",col="yellow",cex=2.2)
    
    #Show the time
    mtext(paste("Tiempo (años terrestres): ",round(t[i,]*0.1592,2),sep=""),side=3,line=0.2,col="white",cex=1.2)
    
    #Wrap the animation with a box
    box(which="plot",lty="solid",col="white")
    
    for(j in 1:10) { #Plot every body in the Solar System
      points(x[i,j]-x[i,1],y[i,j]-y[i,1],type="p",cex=sizes[j]*diminish[i],col=colrs[j]) #Plots position of the planet
      if(i<50*j) { #Plots trail. Not enough points at first, so we just plot whatever we have.
        for(k in 1:(i-1)) {
          points(x[i-k,j],y[i-k,j],cex=0.3*diminish[i],col=colrs[j])
        }
      }
      if(i>=50*j) {
        for(k in 1:(50*j)) {
          points(x[i-k,j],y[i-k,j],cex=0.3*diminish[i],col=colrs[j])
        }
      }
    }
    dev.off()
  }
}
