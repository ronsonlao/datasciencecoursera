pollutantmean <- function(directory, pollutant = "sulfate", id = 1:332) {
  if(grep("specdata", directory) == 1) {
    directory <- ("./specdata/") # 需要在该目录下放进刚才下载的文件夹
  }
  # 初始化sum向量
  sum_vector <- c()
  # 找到并读取所有csv文件
  all_files <- as.character( list.files(directory) )
  file_paths <- paste(directory, all_files, sep="")
  
  for(i in id) {
    current_file <- read.csv(file_paths[i], header=T, sep=",")
    head(current_file)
    pollutant
    na_removed <- current_file[!is.na(current_file[, pollutant]), pollutant]
    sum_vector <- c(sum_vector, na_removed)
  }
  
  result <- mean(sum_vector)
  return(round(result, 3)) 
}

complete <- function(directory, id = 1:332) {
  # 设置目录
  if(grep("specdata", directory) == 1) {
    directory <- ("specdata/")
  }
  
  # 得到需要统计的文件个数
  id_len <- length(id)
  complete_data <- rep(0, id_len)
  
  # 找到需要统计的文件
  all_files <- as.character( list.files(directory) )
  file_paths <- paste(directory, all_files, sep="")
  j <- 1 
  
  # 计算每个文件中完整的数据个数
  for (i in id) {
    current_file <- read.csv(file_paths[i], header=T, sep=",")
    complete_data[j] <- sum(complete.cases(current_file))
    j <- j + 1
  }
  result <- data.frame(id = id, nobs = complete_data)
  return(result)
} 

corr <- function(directory, threshold = 0) {
  
  if(grep("specdata", directory) == 1) {
    directory <- ("./specdata/")
  }
  #计算整个表格内部的数据
  complete_table <- complete("specdata", 1:332)
  nobs <- complete_table$nobs
  # 过滤无用NA值
  ids <- complete_table$id[nobs > threshold]
  # get the length of ids vector
  id_len = length(ids)
  # 计算有效数据的个数
  corr_vector <- rep(0, id_len)
  # 遍历所有文件，找到符合的数据
  all_files <- as.character( list.files(directory) )
  file_paths <- paste(directory, all_files, sep="")
  j <- 1
  # 计算相关系数
  for(i in ids) {
    current_file <- read.csv(file_paths[i], header=T, sep=",")
    corr_vector[j] <- cor(current_file$sulfate, current_file$nitrate, use="complete.obs")
    j <- j + 1
  }
  result <- corr_vector
  return(result)   
}

