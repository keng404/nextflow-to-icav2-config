options(stringsAsFactors=FALSE)
library(rlog)
library(stringr)
source('reading_utils.R')
source('functional_utilities.R')
###- ```write_params``` - consumes output of inject_params and writes new ```nextflow.config``` file
write_params <- function(params_list,output_file=NULL){
  new_lines = c()
  if(is.null(output_file)){
    output_file = "nextflow.ica.config"
  }
  for(i in 1:length(names(params_list))){
    new_statement = paste(names(params_list)[i],"=",params_list[[names(params_list)[i]]],collpase=" ")
    new_lines = c(new_lines,new_statement)
  }
  rlog::log_info(paste("WRITING out params to:",output_file))
  updated_nextflow_config_file = gsub(".config$",".config.tmp",output_file)
  write.table(x=new_lines,file=updated_nextflow_config_file,sep="\n",quote=F,row.names=F,col.names=F)
  system(paste("cp",updated_nextflow_config_file,output_file))
}
### helper function for write modules file
create_conditional_statements = function(modules_list,module_name = NULL){
  conditional_statement_lines = c()
  statement_prefixes  = c('if','when','def','else if','else')
  config_keys = names(modules_list)
  keys_of_interest = c()
  for(i in 1:length(config_keys)){
    config_key_split = strsplit(config_keys[i],"\\s+")[[1]]
    if(sum(config_key_split[1] %in% statement_prefixes) > 0){
      keys_of_interest = c(keys_of_interest,config_keys[i])
    }
  }
  if( length(keys_of_interest) > 0){
    for(j in 1:length(keys_of_interest)){
      conditional_statement_lines = c(conditional_statement_lines,keys_of_interest[j])
      conditional_statement_lines = c(conditional_statement_lines,paste("\t","process {",collapse=""))
      if(!is.null(module_name)){
        conditional_statement_lines = c(conditional_statement_lines,paste("\t\t",module_name,collapse=""))
      }
      process_keys = names(modules_list[[keys_of_interest[j]]])
      for(k in 1:length(process_keys)){
        sub_process_keys = names(modules_list[[keys_of_interest[j]]][[process_keys[k]]])
        for(sk in 1:length(sub_process_keys)){
          if(!is.null(module_name)){
            conditional_statement_lines = c(conditional_statement_lines,paste("\t\t\t",sub_process_keys[sk],"=",modules_list[[keys_of_interest[j]]][[process_keys[k]]][[sub_process_keys[sk]]],collapse=" "))
          } else{
            conditional_statement_lines = c(conditional_statement_lines,paste("\t\t",sub_process_keys[sk],"=",modules_list[[keys_of_interest[j]]][[process_keys[k]]][[sub_process_keys[sk]]],collapse=" "))
          }
        }
      }
    }
  }
  return(conditional_statement_lines)
}
create_regular_statements = function(modules_list,module_name = NULL){
  regular_statement_lines = c()
  statement_prefixes  = c('if','when','def','else if','else')
  config_keys = names(modules_list)
  keys_of_interest = c()
  for(i in 1:length(config_keys)){
    config_key_split = strsplit(config_keys[i],"\\s+")[[1]]
    if(sum(config_key_split[1] %in% statement_prefixes) > 0){
      keys_of_interest = c(keys_of_interest,config_keys[i])
    }
  }
  if( length(keys_of_interest) > 0){
    for(j in 1:length(keys_of_interest)){
      regular_statement_lines = c(regular_statement_lines,keys_of_interest[j])
      regular_statement_lines = c(regular_statement_lines,paste("\t","process {",collapse=""))
      if(!is.null(module_name)){
        regular_statement_lines = c(regular_statement_lines,paste("\t\t",module_name,collapse=""))
      }
      sub_process_keys = names(modules_list[[keys_of_interest[j]]][["default"]])
      for(sk in 1:length(sub_process_keys)){
        if(!is.null(module_name)){
         regular_statement_lines = c(regular_statement_lines,paste("\t\t\t",sub_process_keys[sk],"=",modules_list[[keys_of_interest[j]]][["default"]][[sub_process_keys[sk]]],collapse=" "))
        } else{
          regular_statement_lines = c(regular_statement_lines,paste("\t\t",sub_process_keys[sk],"=",modules_list[[keys_of_interest[j]]][["default"]][[sub_process_keys[sk]]],collapse=" "))
        }
      }
    }
  }
  return(regular_statement_lines)
}
###- ```write_modules``` - consumes output of modules_to_list and write new ```modules.config``` file 
write_modules <- function(modules_list,output_file=NULL,template_file=NULL){
  modules_list1 = override_module_config(module_list)
  if(length(names(module_list1)) > 0 ){
    module_list = module_list1
  }
  new_lines = c()
  if(is.null(output_file)){
    output_file = "conf/modules.ica.config"
  }
  if(sum("general" %in% names(modules_list))  == 0){
    template_file_data = read.delim(template_file,quote="",header=F)
    template_file_data = t(template_file_data)
    new_lines = c(new_lines,template_file_data)
  } else{
    general_config_data = create_conditional_statements(modules_list[["general"]])
    if(length(general_config_data) > 0){
      new_lines = c(new_lines,general_config_data)
    }
    general_config_data = create_regular_statements(modules_list[["general"]])
    if(length(general_config_data) > 0){
      new_lines = c(new_lines,general_config_data)
    }
  }
  keys_of_interest = names(modules_list)
  keys_of_interest = keys_of_interest[keys_of_interest!="general"]
  #######
  for(koi in 1:length(keys_of_interest)){
    conditional_config_data = create_conditional_statements(modules_list[[keys_of_interest[koi]]],keys_of_interest[koi])
    if(length(conditional_config_data) > 0 ){
      new_lines = c(new_lines,conditional_config_data)
    }
    config_data = create_regular_statements(modules_list[[keys_of_interest[koi]]],keys_of_interest[koi])
    if(length(config_data) > 0){
      new_lines = c(new_lines,config_data)
    }
  }
#########  
  rlog::log_info(paste("WRITING out modules configuration to:",output_file))
  updated_nextflow_config_file = gsub(".config$",".config.tmp",output_file)
  write.table(x=new_lines,file=updated_nextflow_config_file,sep="\n",quote=F,row.names=F,col.names=F)
  system(paste("cp",updated_nextflow_config_file,output_file))
}