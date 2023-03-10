options(stringsAsFactors=FALSE)
library(rlog)
library(stringr)
#####
getRelativePath = function(to, from ){
  to_split = strsplit(to,"/")[[1]]  
  from_split = strsplit(from,"/")[[1]]
  idx = 1
  last_idx = idx
  while(to_split[idx] == from_split[idx]){
    last_idx = idx
    idx = idx + 1
  }
  return(paste(to_split[(last_idx+1):length(to_split)],collapse="/"))
}
### ```params_to_strip``` - loads ```params_to_strip.txt``` template and passes list to downstream function
params_to_strip <- function(template_file){
  template_data = read.delim(template_file,quote="",header=F)
  template_data = t(template_data)
  return(template_data)
}
## ```params_to_inject``` - loads ```params_to_inject.txt``` template and passes list to downstream function
params_to_inject <- function(template_file){
  template_data = read.delim(template_file,quote="",header=F)
  template_data = t(template_data)
  return(template_data)
}
## ```copy_modules_template``` - copy in ```template.modules.config``` file to pipeline dir as ```'conf/modules.config'```
copy_modules_template <- function(template_file=NULL,destination_dir=NULL,output_file=NULL){
  if(is.null(output_file)){
    output_file = "conf/modules.ica.config"
  }
  output_path = paste(destination_dir,output_file,sep="/",collapse="")
  copy_command = c("cp",template_file,output_path)
  rlog::log_info(paste("RUNNING_COMMAND:",paste(copy_command,collapse = " ")))
  system(paste(copy_command,collapse = " "))
  
}
#########################
## ```strip_params``` - consumes output of ```params_to_strip``` and ```read_params```, removes params defined in ```params_to_strip.txt```
strip_params <- function(params_list,params_to_strip){
  keys_to_keep = names(params_list)[! names(params_list) %in% params_to_strip]
  keys_removed = names(params_list)[ names(params_list) %in% params_to_strip]
  if(length(keys_removed) > 0){
    rlog::log_info(paste("REMOVED_PARAMS:",paste(keys_removed,collapse=" ,"),sep=" "))
  }
  params_list1 = list()
  for( i in 1:length(keys_to_keep)){
    params_list1[[keys_to_keep[i]]] = params_list[[keys_to_keep[i]]]
  }
  return(params_list1)
}
### ```inject_params``` - consumes output from ```strip_params``` and add params  defined in ```params_to_inject.txt```
inject_params <- function(params_list, params_to_inject){
  params_list1 = params_list
  params_to_inject_list = list()
  for(i in 1:length(params_to_inject)){
    line_split = strsplit(params_to_inject[i],"\\s+")[[1]]
    line_split = apply(t(line_split),2,trimws)
    rlog::log_info(paste("INJECT_PARAM:",line_split[1],"=",line_split[3],collapse = " "))
    params_to_inject_list[[line_split[1]]] = line_split[3]
  }
  
  keys_already_present = names(params_list)[names(params_list) %in% names(params_to_inject_list)]
  keys_not_present = names(params_to_inject_list)[! names(params_to_inject_list) %in% keys_already_present]
  if(length(keys_already_present) > 0){
    for(i in 1:length(keys_already_present)){
      params_list1[[keys_already_present[i]]] = params_list[[keys_already_present[[i]]]]
    }
    if(length(keys_not_present)>0){
      for(j in 1:length(keys_not_present)){
        params_list1[[keys_not_present[j]]] = params_to_inject_list[[keys_not_present[[j]]]]
      }
    }
  } else{
    if(length(keys_not_present)>0){
      for(j in 1:length(keys_not_present)){
        params_list1[[keys_not_present[j]]] = params_to_inject_list[[keys_not_present[[j]]]]
      }
    }
  }
  return(params_list1)
}
##########

###```add_module_reference``` - Add in reference to ```nextflow.config``` file ```includeConfig 'conf/modules.config' ```
add_module_reference <- function(nextflow_config,existing_module_file=NULL,additional_config='conf/modules.ica.config'){
  reference_statement = paste("includeConfig",paste("'",additional_config,"'",collapse="",sep=""),collapse=" ")
  nextflow_config_data = read.delim(nextflow_config,quote="",header=F)
  nextflow_config_data = t(nextflow_config_data)
  if(!is.null(existing_module_file)){
    nextflow_config_data1 = nextflow_config_data
    for(i in 1:length(nextflow_config_data1)){
      if(grepl(basename(existing_module_file),nextflow_config_data1[i])){
        nextflow_config_data1[i] = reference_statement
      }
    }
    nextflow_config_data = nextflow_config_data1
  } else{
    nextflow_config_data = c(nextflow_config_data,reference_statement)
  }
  rlog::log_info(paste("UPDATING",nextflow_config))
  updated_nextflow_config_file = gsub(".config$",".config.tmp",nextflow_config)
  write.table(x=nextflow_config_data,file=updated_nextflow_config_file,sep="\n",quote=F,row.names=F,col.names=F)
  system(paste("cp",updated_nextflow_config_file,nextflow_config))
}
