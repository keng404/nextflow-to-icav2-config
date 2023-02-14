options(stringsAsFactors=FALSE)
library(rlog)
library(stringr)
###- ```write_params``` - consumes output of inject_params and writes new ```nextflow.config``` file
write_params <- function(params_list,additional_lines = NULL,output_file=NULL){
  new_lines = c()
  if(is.null(output_file)){
    output_file = "nextflow.ica.config"
  }
  for(i in 1:length(names(params_list))){
    new_statement = paste(names(params_list)[i],"=",params_list[[names(params_list)[i]]],collpase=" ")
    new_lines = c(new_lines,new_statement)
  }
  if(!is.null(additional_lines)){
    for(i in 1:length(additional_lines)){
      new_lines = c(new_lines,additional_lines[i])
    }
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
######
######## grabbing instance table info from the ICA GitBook
get_instance_type_table <- function(url){
  library(rvest)
  html = read_html(url,encoding = "ISO-8859-1")
  html_div_nodes = html %>% html_elements("tr")
  nodes_that_have_table_data = html %>% html_elements("tr") %>% html_attr("data-rnwi-handle")
  
  nodes_to_check = (1:length(html_div_nodes))[nodes_that_have_table_data == "table-row"]
  nodes_to_check = nodes_to_check[!is.na(nodes_to_check)]
  
  computeTypes = FALSE
  lines_to_keep = c()
  for(i in 1:length(nodes_to_check)){
    text_of_interest = html_div_nodes[nodes_to_check[i]] %>% html_text2()
    cpuReference = FALSE
    if(grepl("Compute Type",text_of_interest)){
      computeTypes = TRUE
      
    } else if(grepl("cpu",text_of_interest) || grepl("small",text_of_interest) || grepl("medium",text_of_interest) || grepl("large",text_of_interest)){
      cpuReference = TRUE
    } else{
      computeTypes = FALSE
    }
    
    if(computeTypes || cpuReference){
      #print(html_attrs(html_div_nodes[nodes_to_check[i]]))
      #print(html_div_nodes[nodes_to_check[i]] %>% html_text2())
      new_line = strsplit(text_of_interest,"\t|\n")[[1]]
      new_line = new_line[new_line!=""]
      #lines_to_keep = c(lines_to_keep,new_line)
      lines_to_keep = rbind(lines_to_keep,new_line)
    }
  }
  
  header_line = c()
  content_lines = c()
  for(line in 1:nrow(lines_to_keep)){
    if(line == 1){
      ##header_line = strsplit(lines_to_keep[line],"\n")[[1]]
      header_line = lines_to_keep[line,]
    } else{
      ##content_lines = rbind(content_lines,strsplit(lines_to_keep[line],"\n")[[1]])
      content_lines = rbind(content_lines,lines_to_keep[line,])
    }
  }
  colnames(content_lines) = header_line
  rownames(content_lines) = NULL
  return(as.data.frame(content_lines))
}

# lookup_table format found in getInstancePodAnnotation function in the nf-core.ica_mod_nf_script.R file
getInstancePodAnnotation <- function(cpus,mem,container_name,ica_instance_table){
  pod_annotation_prefix = paste("pod annotation:", "'scheduler.illumina.com/presetSize'", ",","value:")
  pod_annotation = NULL
  pod_value = NA
  search_query = c()
  if(length(mem) >0){
    mem = apply(t(mem),2,function(x) strtoi(stringr::str_extract(x,"[[0-9]]+")))
    if(is.numeric(mem)){
      mem = 1.25 * mem 
    }
  } else{
    mem = stringr::str_extract(mem,"[[0-9]]+")
    if(is.numeric(mem)){
      mem = 1.25 * mem 
    }
  }
  if(length(cpus) >0){
    cpus = apply(t(cpus),2,function(x) strtoi(x))
    if(is.numeric(cpus)){
      cpus = 1.25 * cpus
    }
  } else{
    cpus = strtoi(cpus)
    if(is.numeric(cpus)){
      cpus = 1.25 * cpus
    }
  }
  rlog::log_info(paste("PARSED cpu and mem info:",mem,cpus))
  if(length(cpus) > 0 && length(mem) > 0){
    search_query = ica_instance_table$CPUs >= max(cpus) &  ica_instance_table$`Mem (GB)` >= max(mem)
  } else if(length(cpus) > 0 && length(mem) == 0){
    search_query = ica_instance_table$CPUs >= max(cpus) 
  } else if(length(cpus) == 0 && length(mem) > 0){
    search_query = ica_instance_table$`Mem (GB)` >= max(mem)
  } else{
    pod_annotation = paste(pod_annotation_prefix,"'himem-small'")
    return(pod_annotation)
  }
  if(!is.null(container_name) && grepl("dragen",container_name)){
    pod_annotation = paste(pod_annotation_prefix,"'fpga-medium'")
  } else if(sum(search_query) > 0){
    pod_value = ica_instance_table[search_query,]$`Compute Type`[1]
    pod_annotation = paste(pod_annotation_prefix,paste("'",pod_value,"'",sep=""))
    return(pod_annotation)
  } else{
    pod_annotation = paste(pod_annotation_prefix,"'himem-small'")
    return(pod_annotation)  
  }
}
#####
addMemOrCPUdeclarations <- function(pod_annotation,lookup_table){
  pod_annotation_split = strsplit(pod_annotation,"\\s+")[[1]]
  pod_annotation_split[length(pod_annotation_split)] = gsub("'","",pod_annotation_split[length(pod_annotation_split)])
  pod_annotation = pod_annotation_split[length(pod_annotation_split)] 
  declarations_to_add = c()
  scale_factor = 0.80  # make sure we don't take up all CPUs and memory to avoid throtting/job failure that way
  if(is.null(pod_annotation)){
    declarations_to_add = c(declarations_to_add,"\tcpus 7")
    declarations_to_add = c(declarations_to_add,"\tmemory 60GB")
  } else{
    lookup_query = lookup_table$`Compute Type` == pod_annotation
    if( sum(lookup_query) > 0 ){
      cpu_val = floor(scale_factor * strtoi(lookup_table[lookup_query,]$`CPU`[1]))
      declarations_to_add = c(declarations_to_add,paste("\tcpus",cpu_val))
      mem_val = floor(scale_factor * strtoi(lookup_table[lookup_query,]$`Mem (GB)`[1]))
      declarations_to_add = c(declarations_to_add,paste("\tmemory",paste("'",mem_val," GB","'",sep="")))
    } else{
      rlog::log_info(paste("ICA_INSTANCE_TABLE:",lookup_table))
      stop(paste("Could not find pod annotation label:",pod_annotation))
    }
  }
  return(declarations_to_add)
}
### - ```override_module_config``` - override module configs for (cpus, memory)

override_module_config <- function(module_list,ica_instance_table){
  module_list1 = module_list
  overrides <- c("cpus","memory")
  double_check <- c("publishDir")
  for(i in 1:length(names(module_list))){
    module_name = names(module_list)[i]
    sub_conf = module_list[[module_name]]
    for(j in 1:length(names(sub_conf))){
      conditional_logic = names(sub_conf)[j]
      config_names = names(module_list[[module_name]][[conditional_logic]])
      memory_declaration = NULL
      cpu_declaration = NULL
      pod_declaration = NULL
      for(k in 1:length(config_names)){
        config_parameter = config_names[k]
        if(config_parameter %in% overrides){
          rlog::log_info(paste("Overridding parameter:",config_parameter,"->",conditional_logic,"->",module_name))
          if(config_parameter == "cpus"){
            cpu_declaration =  parameter_value_split[l + 1]
          } else if(config_parameter == "memory"){
            memory_declaration =  parameter_value_split[l + 1]
          }
          
        } else if(config_parameter %in% double_check){
          rlog::log_info(paste("Double-Checking for parameter:",config_parameter,"->",conditional_logic,"->",module_name))
          path_count = 0
          pattern_count = 0
          revised_parameter_value = c()
          parameter_value = module_list[[module_name]][[conditional_logic]][[config_parameter]]
          parameter_value_split = strsplit(parameter_value,"\\s+")[[1]]
          for(l in 1:length(parameter_value_split)){
            if(parameter_value_split[l] == "path:"){
              revised_parameter_value[l] = "[ path:"
              path_count = path_count + 1
            } else if(parameter_value_split[l] == "pattern:"){
              if(l < (length(parameter_value_split)-1)){
                string_of_interest = parameter_value_split[l + 1]
                parameter_value_split[l + 1] = paste(string_of_interest,"],",collapse = " ")
                pattern_count = pattern_count + 1
              } else{
                string_of_interest = parameter_value_split[l + 1]
                parameter_value_split[l + 1] = paste(string_of_interest,"]",collapse = " ")
                pattern_count = pattern_count + 1
              }
              revised_parameter_value[l] = parameter_value_split[l] 
            } else{
              revised_parameter_value[l] = parameter_value_split[l]               
            }
            new_parameter_value = paste(revised_parameter_value,collapse = " ",sep = " ")
            module_list1[[module_name]][[conditional_logic]][[config_parameter]] = new_parameter_value
          }
        } else{
          rlog::log_info(paste("Skipping checks for parameter:",config_parameter,"->",conditional_logic,"->",module_name))
        }
      }
      if(!is.null(memory_declaration) | !is.null(cpu_declaration)){
        pod_declaration = getInstancePodAnnotation(cpu_declaration,memory_declaration,NULL,ica_instance_table)
        pod_declaration_split = strsplit(pod_declaration,"\\s+")[[1]]
        pod_declaration_statement = paste(pod_declaration_split[3:length(pod_declaration_split)],sep = " ",collapse = " ")
        if(!"pod annotation" %in% names(module_list[[module_name]][[conditional_logic]]) ){
          rlog::log_info(paste("Adding parameter:","pod annotation","->",conditional_logic,"->",module_name))
          rlog::log_info(pod_declaration_statement)
          module_list1[[module_name]][[conditional_logic]][["pod annotation"]] = pod_declaration_statement
        }
      }
    }
  }
  return(module_list1)
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