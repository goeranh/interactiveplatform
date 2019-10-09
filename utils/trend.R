
trend_bar <- function(data, incbad = FALSE, minyear, rate, year, country){
   #print(as.data.frame(data))
   data <- as.data.table(data)
   setnames(data,old=c(rate,year,country),new=c("rate","year","country"))

   data<-data[,c("rate","year","country")]
   data<-data[year>=minyear]
   
   data[,numyear:=.N,by=c("country")]
   data[numyear>1,trend:=coef(lm(rate~year,na.action =na.exclude ))[2], by=c("country")]
   data[numyear>1,sig:=summary(lm(rate~year,na.action =na.exclude))$coefficients[8], by=c("country")]
   data[trend>0 & sig<0.1,cat:=1, by=c("country")]
   
   
   data[(sig>0.1|is.na(sig)==T) & is.na(trend)==F,cat:=0, by=c("country")] ###!!!!Se sig è NA assegno 0?
   data[trend<0 & sig<0.1,cat:=-1, by=c("country")]
   data<-data[,maxyear:=max(year),by=c("country")]
   # create rank variable 
   f <-data[year==maxyear,list(rate=mean(rate,na.rm = T)),by=c("country")]
   f[,rank:= factor(country,levels = rev(sort(unique(country))))]
   f <-f[, c("rank","country")]
   data <-merge(data,f, by=c("country"), all.x = T)
   data[,maxx:=max(rate)+(max(rate)/5),]
   if (incbad==T) {
      colors1<-c("Decreased"="#228B22" ,"No noticeable change"="#FF8C00", "Increased"="#ff0000")
   } else {
      colors1<-c("Decreased"="#ff0000" ,"No noticeable change"="#FF8C00", "Increased"="#228B22")
   }
   
   shapes<-c("-"=25 ,"="=21, "+"=24)
   data[,cat:=factor(cat,levels=c(-1, 0, 1),labels=c("-","=","+")),]
   trend <- unique(select(data, rank,cat))
   return(list(trend =trend , shapes = shapes))
}