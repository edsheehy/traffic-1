
rm(list=ls())

require(ggplot2)

M50 = read.csv('CombinedTraffic_M50.csv') # Read in all data from M50
M50$date = as.Date(M50$date)

format = theme(text = element_text(size=20))

# Plot data coverage
b=ggplot(data=M50,aes(x=date,y=site, colour=site)) + geom_point(size=10, shape=15) + ggtitle('M50 Data Coverage') + format 

b + scale_colour_discrete(name="Location",labels=unique(paste(M50$site,'=',M50$name)))

site = subset(M50, site=='21' )
site = subset(M50, site=='21' & format(date,'%Y')>2011)
site$hour = as.numeric(format(site$date,'%H'))
site$month = (format(site$date,'%b'))
tit.text = paste('M50: count site=',site$name[1])

ggplot(data=site, aes(x=hour,y=light, colour=direction)) + geom_smooth(method=loess,span=0.2,size=2)  + theme_bw() +ggtitle(tit.text) + format 

#ggplot(data=site, aes(x=hour,y=light, colour=month)) + geom_smooth(method=loess,span=0.2, size=2)  + theme_bw() + ylab('Count of light vehicles per hour')+ xlab('Hour of Day') +ggtitle(tit.text) + format 

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
