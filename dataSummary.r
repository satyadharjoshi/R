# Version 1, Date: 31 May
# Author: Joshi - 058 567 8131
# Comments: Passing one of the arguments through VBA but I need to repair symbols_all_all file

args<-commandArgs(trailingOnly=T)
# These three arguments are passed from Excel
t1<-as.character(args[1])
t2<-as.character(args[2])
path_main_t3<-as.character(args[3])

#------Old variables to check flow of data from VBA to R
#t1<-"2015-05-03"
#t2<-"2015-06-03"
# --- End--------

mydays <- seq(as.Date(t1), as.Date(t2), by="1 day")

myweekdays <- mydays[ ! weekdays(mydays) %in% c('Saturday','Sunday') ] 
days<-format(myweekdays, format="%Y%m%d")
# user<-Sys.info()["user"]
user<-Sys.info()["user"]

#------Old Path Name------
#path_main<- paste('C:\\Users\\',user,'\\Google Drive\\versions michel\\IB\\IB_DATA\\',sep='')
#Changing folder from Micheal to current folder which is passed by VBA
#--------------- End-----------

path_main<-path_main_t3

all_sym<-read.csv(paste(file=path_main,"symbols_all_daily_close",'.csv',sep=''),head=TRUE,sep=",",stringsAsFactors = FALSE)

nb_sym<-length(all_sym[,1])
result<-NULL
for(s in 1:nb_sym){
	out<-NULL
	cur_data<-NULL
	for(j in 1:length(days)){
		
		
		cur_date	<- days[j]
		cur_sym		<- all_sym[s,]
		name		<- paste(cur_sym$Symbol,'_',cur_date,sep='')
		name_daily	<- paste(cur_sym$Symbol,'_D_',cur_date,sep='')
		dir_		<- paste(substr(cur_date,1,4),substr(cur_date,5,6),substr(cur_date,7,8),sep='\\')
		dir_		<- paste(path_main,dir_,sep="")
		full_path   <- paste(dir_,'\\',name,'.csv',sep='')	
		full_pathDaily   <- paste(dir_,'\\',name_daily,'.csv',sep='')	

			if(file.exists(full_path) & file.info(full_path)$size>10){
				print(paste(cur_date,cur_sym$Symbol,sep=" "))	

				data_c<-read.csv(full_path,head=TRUE,sep=",",stringsAsFactors = FALSE)	
				time_stamp<-data_c[1,1]
				time_stamp_d<-strftime(time_stamp, format="%Y%m%d")
				if(time_stamp_d != cur_date){
					data_c<-NULL
					write.csv(data_c, file =full_path,row.names=TRUE)
					print(paste(cur_date,cur_sym$Symbol,sep=" "))	
				}
				data_c<-length(data_c[,1])
				
			}
			else{
			data_c<-0
			}
			cur_data<-c(cur_data,data_c)
	}
	result<-cbind(result,cur_data)	
	
}  
result<-cbind(days,result)			   
result<-as.data.frame(result)
names(result)<-c('date',all_sym$Symbol)
write.csv(result, file =paste(path_main,'data_summary.csv',sep=''),row.names=FALSE)		
		   
		   
		   
	