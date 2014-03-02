# Read in all the data files for the traffic counts on the M50
rm(list=ls())

# File of eastings and northings of each camera
loc = read.csv('M50_locations.csv', header=T)

# list file names containing the data
files = list.files(pattern="M50-")

# Loop through each file and read in the data
# save the data into one dataframe called M50
for (f in 1:length(files)) {
  tmp = strsplit(files[f],"[-()]")
  road = tmp[[1]][1]
  site = tmp[[1]][2]
  name = tmp[[1]][3]
  tmp = read.csv(files[f])
  names(tmp) <- c('day','date','hour','total.south','HGV.south','total.north','HGV.north','tot','HGV.tot')
  tmp$date = strptime(paste(as.character(tmp$date),tmp$hour/100-1,tmp$day), format='%t%h %e %Y %H %a')
  tmp1 = cbind(tmp[,c(1:3)],road,site,name)
  tmp2 = rbind(tmp1,tmp1)
  tmp2$direction = factor(rep(c('north','south'),each=length(tmp1[,1])))
  tmp2$total = c(tmp$total.north, tmp$total.south)
  tmp2$hgv = c(tmp$total.north*tmp$HGV.north/100, tmp$total.south*tmp$HGV.south/100)
  tmp2$hgv2 = c(tmp$HGV.north, tmp$HGV.south)
  tmp2$light = tmp2$total - tmp2$hgv
  if (f==1) {
    M50 = tmp2
  } else {
    names(tmp2) <- names(M50)
    M50 = rbind(M50, tmp2)
  }
}


summary(M50)

# Write the data frame as a csv file
write.csv(M50,'CombinedTraffic_M50.csv',row.names=F,quote=F)
