
rm(list=ls())

loc = read.csv('M50_locations.csv', header=T)

files = list.files(pattern="M50-")

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

write.csv(M50,'CombinedTraffic_M50.csv',row.names=F,quote=F)

require(ggplot2)

format = theme(text = element_text(size=20))
ggplot(data=M50,aes(x=date,y=site, colour=name)) + geom_point(size=10, shape=15) + ggtitle('M50 Data Coverage') + format

site = subset(M50, site=='21' & format(date,'%Y')>2011)
site$hour = as.numeric(format(site$date,'%H'))
site$month = (format(site$date,'%b'))
tit.text = paste('M50: count site=',site$name[1])

ggplot(data=site, aes(x=hour,y=light, colour=direction)) + geom_smooth(method=loess,span=0.2,size=2)  + theme_bw() +ggtitle(tit.text) + format 
 
ggplot(data=site, aes(x=hour,y=light, colour=month)) + geom_smooth(method=loess,span=0.2, size=2)  + theme_bw() + ylab('Count of light vehicles per hour')+ xlab('Hour of Day') +ggtitle(tit.text) + format 

ggplot(data=site, aes(x=hour,y=light, colour=day)) + geom_smooth(method=loess,span=0.2, size=2)  + theme_bw() + ylab('Count of light vehicles per hour')+ xlab('Hour of Day') +ggtitle(tit.text) + format 
ggplot(data=site, aes(x=hour,y=hgv, colour=day)) + geom_smooth(method=loess,span=0.2, size=2)  + theme_bw()+ ylab('Count of HGV per hour') + xlab('Hour of Day') + format +ggtitle(tit.text)
#ggplot(data=site, aes(x=hour,y=hgv2, colour=day)) + geom_smooth(method=loess,span=0.2, size=2) + theme_bw()+ ylab('Percentage of HGV per hour') + xlab('Hour of Day') + format +ggtitle(tit.text)


site = subset(M50, site=='21' | site=='17a' & format(date,'%Y')==2012 & day=='Wed',select=c('name','site','hour','direction','light','hgv'))
site = subset(M50, site=='22' | site=='21' & format(date,'%Y')==2012 & day=='Wed',select=c('name','site','hour','direction','light','hgv'))
tmp = melt(site,id=c('site','name','direction','hour'))
tmp2 = dcast(tmp,hour+direction~site+name+variable,fun.aggregate=mean,na.rm=T)

tmp2$d.light = tmp2[,3]/tmp2[,5]
tmp2$d.hgv = tmp2[,4]/tmp2[,6]
ggplot(data=tmp2, aes(x=hour,y=d.light, colour=direction)) + geom_smooth(method=loess,size=2)  + theme_bw()  + format 
ggplot(data=tmp2, aes(x=hour,y=d.hgv, colour=direction)) + geom_smooth(method=loess,size=2)  + theme_bw()  + format 
