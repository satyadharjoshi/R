


format_data_excel <- function(data_,name){
	names(data_)<-c(name,"OPEN","HIGH","LOW","CLOSE","VOLUME","WAP","GAP","COUNT")
	data_<-data_[rev(rownames(data_)),]
	return(data_)

}

args<-commandArgs(trailingOnly=T)
t1<-as.character(args[1])
t2<-as.character(args[2])
path_main_t3<-as.character(args[3])

#pat<- 'C:\\Users\\XXX\\Desktop\\IB\\VBA_out.txt'
user<-Sys.info()["user"]
pat<- paste('C:\\Users\\',user,'\\Google Drive\\GstratLive\\IB\\VBA_out.txt',sep='')


print(as.Date(t1))
#t1<-"2015-06-03"
#t2<-"2015-06-17"
		   
mydays <- seq(as.Date(t1), as.Date(t2), by="1 day")
#mydays <- seq(as.Date("2014-02-01"), as.Date("2014-02-20"), by="1 day")

myweekdays <- mydays[ ! weekdays(mydays) %in% c('Saturday','Sunday') ] 
days<-format(myweekdays, format="%Y%m%d")

path_main<-path_main_t3

#-----------Old Path------------------------------
#path_main<-'C:\\Users\\XXX\\Desktop\\IB\\IB_DATA\\'	  
#path_main<- paste('C:\\Users\\',user,'\\Google Drive\\GstratLive\\IB\\IB_DATA\\',sep='')
#--------------------------------------------------

	  
all_sym<-read.csv(paste(file=path_main,"symbols_all",'.csv',sep=''),head=TRUE,sep=",",stringsAsFactors = FALSE)

nb_sym<-length(all_sym[,1])
nb_daysT<-length(days)
for(s in 1:nb_sym){
	out<-NULL
	
	cur_sym		<- all_sym[s,]
	t_cur<-strftime(Sys.time())
	t_close<- (cur_sym$LocalClosingTime)		   
	t_close<-strptime(t_close, format="%H:%M")
	nb_days<-nb_daysT-1
	if(t_close<t_cur){
		nb_days<-nb_daysT
	}
	for(j in 1:nb_days){
		
		
		cur_date	<- days[j]
		name		<- paste(cur_sym$Symbol,'_',cur_date,sep='')
		name_daily	<- paste(cur_sym$Symbol,'_D_',cur_date,sep='')
		dir_		<- paste(substr(cur_date,1,4),substr(cur_date,5,6),substr(cur_date,7,8),sep='\\')
		dir_		<- paste(path_main,dir_,sep="")
		full_path   <- paste(dir_,'\\',name,'.csv',sep='')	
		full_pathDaily   <- paste(dir_,'\\',name_daily,'.csv',sep='')	

			if(file.exists(full_path) & file.info(full_path)$size>20){
				print(paste(cur_date,cur_sym$Symbol,sep=" "))	

				
				data_c<-read.csv(full_path,head=TRUE,sep=",",stringsAsFactors = FALSE)	
				data_c<-format_data_excel(data_c,cur_sym$Symbol)
			}
			else{
			data_c<-NULL
			}

	
		out<-rbind(data_c,out)
	}	
	write.csv(out, file =paste(path_main,'current\\',cur_sym$SymbolXL,'.csv',sep=''),row.names=FALSE)		
}  
		   


		   
	