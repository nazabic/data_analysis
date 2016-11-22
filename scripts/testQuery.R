buildEthoscopeQuery2 <- function(result_dir,query=NULL){
  
  rethomics:::checkDirExists(result_dir)
  
  key <- c("date","machine_name")
  use_date <- F
  if(!is.null(query)){
    q <- copy(as.data.table(query))
    rethomics:::checkColumns(key, colnames(q))
    q[, date:=as.character(date)]
    
    t <- q[,as.POSIXct(date, "%Y-%m-%d_%H-%M-%S", tz="GMT")]
    if(any(is.na(t)))
      t <- q[,as.POSIXct(date, "%Y-%m-%d", tz="GMT")]
    use_date <- T
    
    q[,date :=t]
    setkeyv(q,key)
  }
  all_db_files <- list.files(result_dir,recursive=T, pattern="*\\.db$")
  
  fields <- strsplit(all_db_files,"/")
  valid_files <- sapply(fields,length) == 4
  
  all_db_files <- all_db_files[valid_files]
  
  if(length(all_db_files) == 0){
    stop(sprintf("No .db files detected in the directory '%s'. Ensure it is not empty.",result_dir))
  }
  
  files_info <- do.call("rbind",fields[valid_files])
  files_info <- as.data.table(files_info)
  setnames(files_info, c("machine_id", "machine_name", "date","file"))
  
  if(use_date)
    files_info[,date:=as.POSIXct(date, "%Y-%m-%d", tz="GMT")]
  else
    files_info[,date:=as.POSIXct(date, "%Y-%m-%d_%H-%M-%S", tz="GMT")]
  
  files_info[,path := paste(result_dir,all_db_files,sep="/")]
  setkeyv(files_info,key)
  
  if(is.null(query))
    return(files_info)
  files_info[,n:=.N,by=key(files_info)]
  unique_fi = unique(files_info,fromLast = T)
  out <- unique_fi[q]
  duplicated_queries <- unique(out[n>1,.(date,machine_name)])
  if(nrow(duplicated_queries) > 0){
    for( i in 1:nrow(duplicated_queries)){
      str <- "Duplicated queries. Excluding {%s, %s}"        
      str <- sprintf(str,duplicated_queries[i,machine_name],duplicated_queries[i,date])
      warning(str)
    }
  }
  # we don't need the column 'n' any longer
  out[, n:=NULL]
  nas <- is.na(out[,path]) 
  if(any(nas)){
    out_nas <- out[nas,]
    for(i in nrow(out_nas)){
      warning(sprintf("No result for machine_name == %s and date == %s. Omiting query",out_nas[i,machine_name],out_nas[i,date])) 
    }
  }
  out <- na.omit(out)
  setkeyv(out, union(key(out),colnames(q)))
  out
}




setwd("/home/diana/github/sleep_analysis_experiments/dd_analysis")
q0 <- fread("./query.csv")
q1 <- q0[date=='2016-01-15' & machine_name =='ETHOSCOPE_019']
q <- buildEthoscopeQuery2("/mnt/ethoscope_results", q0)
q <- buildEthoscopeQuery2("/mnt/ethoscope_results")

p <- "019aeeee10184bb39b0754e75cef7900/ETHOSCOPE_019/2016-01-15_20-38-42/2016-01-15_20-38-42_019aeeee10184bb39b0754e75cef7900.db"

