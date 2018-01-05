library(IBrokers)


args<-commandArgs(trailingOnly=T)
t1<-as.character(args[1])
t2<-as.character(args[2])
path_main_t3<-as.character(args[3])

# t1<-"2015-06-15"
# t2<-"2015-06-17"

tws<-twsConnect(clientId = 1, host = 'localhost',port = 7496, verbose = TRUE, timeout = 5,filename = NULL, blocking=.Platform$OS.type=="windows")

print(as.Date(t1))
	   
mydays <- seq(as.Date(t1), as.Date(t2), by="1 day")
#mydays <- seq(as.Date("2015-06-15"), as.Date("2015-06-17"), by="1 day")

myweekdays <- mydays[ ! weekdays(mydays) %in% c('Saturday','Sunday') ] 
days<-format(myweekdays, format="%Y%m%d")
user<-Sys.info()["user"]

#---------Old Paths for Testing
#path_main<- paste('C:\\Users\\',user,'\\Google Drive\\GstratLive\\IB\\IB_DATA\\',sep='')
#path_main<-'C:\\Users\\XXX\\Desktop\\IB\\IB_DATA\\'	  
#---------End----------------------

path_main<-path_main_t3


all_sym<-read.csv(paste(file=path_main,"symbols_all",'.csv',sep=''),head=TRUE,sep=",",stringsAsFactors = FALSE)
#all_sym<- all_sym[which(all_sym$cstrDB>0),]
all_sym<- all_sym[which(all_sym$dlDaily>0),]

nb_daysT<-length(days)
nb_sym<-length(all_sym[,1])
for(s in 1:nb_sym){
	cur_sym		<- all_sym[s,]
	cur_sym		<- all_sym[s,]
	t_cur<-strftime(Sys.time())
	t_close<- (cur_sym$LocalClosingTime)		   
	t_close<-strptime(t_close, format="%H:%M")
	nb_days<-nb_daysT-1
	if(t_close<t_cur|  Sys.Date() >as.Date(t2)){
		nb_days<-nb_daysT
	}
	for(j in 1:nb_days){

		cur_date	<- days[j]
		
		name		<- paste(cur_sym$Symbol,'_',cur_date,sep='')
		name_daily		<- paste(cur_sym$Symbol,'_D_',cur_date,sep='')
		dir_		<- paste(substr(cur_date,1,4),substr(cur_date,5,6),substr(cur_date,7,8),sep='\\')
		
		dirIntra_		<- paste(path_main,dir_,sep="")
		dirFull_		<- paste(path_main,"daily\\",dir_,sep="")
		dir.create(dirIntra_, showWarnings = FALSE,recursive = TRUE)
		dir.create(dirFull_, showWarnings = FALSE,recursive = TRUE)
		full_path   <- paste(dirIntra_,'\\',name,'.csv',sep='')	
		full_pathDaily   <- paste(dirFull_,'\\',name_daily,'.csv',sep='')	

		if(!file.exists(full_path) | file.info(full_path)$size<20){
			print(paste(cur_date,cur_sym$Symbol,'intra',sep=" "))	
			contract	<-	reqContractDetails(tws, twsIndex(cur_sym$Symbol, cur_sym$Exchange, cur_sym$Currency))[[1]]$contract		   
			#contract<-	reqContractDetails(tws, twsIndex("SPX", "CBOE", "USD"))[[1]]$contract
			
			time_c		<- paste(cur_sym$ClosingTime,' ',cur_sym$TimeZone)
			last_time	<- paste(cur_date,' ',time_c)
			duree		<- "3000 S"
			t			<- reqHistoricalData(tws,contract ,endDateTime=last_time,duration = duree,barSize = "1 min")
			#t_daily			<- reqHistoricalData(tws,contract ,endDateTime=last_time,duration = '1 D',barSize = "1 day")
			
			t<-data.frame(t)

			data_c<-t
			if(length(data_c[1,1])>2){
				time_stamp<-data_c[1,1]
				time_stamp_d<-strftime(time_stamp, format="%Y%m%d")
				if(time_stamp_d != cur_date){
					data_c<-NULL
					write.csv(data_c, file =full_path,row.names=TRUE)
					print(paste(cur_date,cur_sym$Symbol,"   DOUBLON",sep=" "))	
				}
			}
			else{
				write.csv(t, file =full_path,row.names=TRUE)	
			}
			Sys.sleep(10)
		}
			if(!file.exists(full_pathDaily)  ){
			print(paste(cur_date,cur_sym$Symbol,'daily',sep=" "))	
			contract	<-	reqContractDetails(tws, twsIndex(cur_sym$Symbol, cur_sym$Exchange, cur_sym$Currency))[[1]]$contract		   
			#contract<-	reqContractDetails(tws, twsIndex("SPX", "CBOE", "USD"))[[1]]$contract
			
			time_c		<- paste(cur_sym$ClosingTime,' ',cur_sym$TimeZone)
			last_time	<- paste(cur_date,' ',time_c)
			duree		<- "1 D"
			t_daily			<- reqHistoricalData(tws,contract ,endDateTime=last_time,duration = duree,barSize = "1 day")
			
			t_daily<-data.frame(t_daily)
			write.csv(t_daily, file =full_pathDaily,row.names=TRUE)	
			Sys.sleep(10)
		}
			
	}		 
}  
		   
twsDisconnect(tws);   
		   
		   
		   
		   
	