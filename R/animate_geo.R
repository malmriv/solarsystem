#Define number of frames
N=300

#Read the data
t=read.table("../results/t.txt")
x=read.table("../results/x.txt")
y=read.table("../results/y.txt")
vx=read.table("../results/vx.txt")
vy=read.table("../results/vy.txt")
ax=read.table("../results/ax.txt")
ay=read.table("../results/ay.txt")

#Create a list of names, colors and sizes
names = c("       Sol","Mercurio","Venus","Tierra","Marte","Júpiter","Saturno","Urano","Neptuno","Plutón")
colrs = c("yellow","brown","orange","blue","coral1","bisque1","burlywood2","cadetblue3","cornflowerblue","burlywood4")
sizes = c(4,0.38,0.95,1,0.53,3,3,2.5,2.2,0.2)

#Create a directory for the frames
dir.create("../geocentric")

#Make a big enough blank plot for all the trajectories
for(i in 1:N) { #Number of frames
  if(i%%2 == 0) { #Sample every X frames in case there are too many of them
    #Create each .png frame
    png(paste("../geocentric/frame",i,".png",sep=""),width=500,height=500)
    
    #(Optional but nice) set aesthetics for the animation
    par(bg="black",col.lab="white",col.axis="white",col.main="white",cex.main=1.3,pch=16)
    
    #Plot an empty graph
    plot(0,0,type="n",xlim=c(-1.2,1.2),ylim=c(-1.2,1.2),asp=1,main="Simulación: sistema solar (geocéntrico).",xlab="x (ua)",ylab="y (ua)")
    
    #Show the time
    mtext(paste("Tiempo (años terrestres): ",round(t[i,]*0.1592,2),sep=""),side=3,line=0.2,col="white",cex=1.2)
    
    #Wrap the animation with a box
    box(which="plot",lty="solid",col="white")
    
    for(j in 1:10) { #Plot every body in the Solar System
      points(x[i,j]-x[i,4],y[i,j]-y[i,4],type="p",cex=sizes[j],col=colrs[j]) #Plots position of the planet
      text(x[i,j]-x[i,4],y[i,j]-y[i,4]-0.05,labels=names[j],col="white",cex=0.9)
      if(i<20*j) { #Plots trail. Not enough points at first, so we just plot whatever we have.
        for(k in 1:(i-1)) {
          points(x[i-k,j]-x[i-k,4],y[i-k,j]-y[i-k,4],cex=0.4,col=colrs[j])
        }
      }
      if(i>=20*j) {
        for(k in 1:(20*j)) {
          points(x[i-k,j]-x[i-k,4],y[i-k,j]-y[i-k,4],cex=0.4,col=colrs[j])
        }
      }
    }
    dev.off()
  }
}
