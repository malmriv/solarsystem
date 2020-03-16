#Read the data
datos = read.csv("../data.csv")

#Import necessary columns
masa = datos$mass_kg
distancias = datos$distance_km

#Convert to right units and normalize
masa = masa/(1.989*10^30)
distancias = distancias*1000
distancias = distancias/(1.496*10^11)

#Save in data_norm.txt file in working directory
normaliz = data.frame(masa,distancias,datos$eccentricity)
write.table(normaliz,file="../data_norm.txt",sep="\t",dec=".",col.names=FALSE,row.names=FALSE)