options(stringsAsFactors=FALSE)
library(rlog)
library(stringr)
source('reading_utils.R')
source('writing_utilities.R')
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
    line_split = strsplit(params_to_inject,"\\s+")[[1]]
    line_split = apply(t(line_split),2,trimws)
    params_to_inject_list[[line_split[1]]] = line_split[3]
  }
  
  keys_already_present = names(params_list)[names(params_list) %in% names(params_to_inject_list)]
  keys_not_present = names(params_to_inject_list)[! keys_already_present %in% names(params_to_inject_list)]
  if(length(keys_already_present) > 0){
    for(i in 1:length(keys_already_present)){
      params_list1[[keys_already_present[i]]] = params_list[[keys_already_present[[i]]]]
    }
    for(j in 1:length(keys_not_present)){
      params_list1[[keys_not_present[j]]] = params_to_inject_list[[keys_not_present[[j]]]]
    }
  } else{
    for(j in 1:length(keys_not_present)){
      params_list1[[keys_not_present[j]]] = params_to_inject_list[[keys_not_present[[j]]]]
    }
  }
  return(params_list1)
}
##########

###```add_module_reference``` - Add in reference to ```nextflow.config``` file ```includeConfig 'conf/modules.config' ```
add_module_reference <- function(nextflow_config,additional_config){
  reference_statement = paste("includeConfig",paste("'",additional_config,"'",collapse="",sep=""),collapse=" ")
  nextflow_config_data = read.delim(nextflow_config,quote="",header=F)
  nextflow_config_data = t(nextflow_config_data)
  nextflow_config_data = c(nextflow_config_data,reference_statement)
  rlog::log_info(paste("UPDATING",nextflow_config))
  updated_nextflow_config_file = gsub(".config$",".config.tmp",nextflow_config)
  write.table(x=nextflow_config_data,file=updated_nextflow_config_file,sep="\n",quote=F,row.names=F,col.names=F)
  system(paste("cp",updated_nextflow_config_file,nextflow_config))
}
######## grabbing instance table info from the ICA GitBook
get_instance_type_table <- function(url){
  library(rvest)
  html = read_html(url,encoding = "ISO-8859-1")
  html_div_nodes = html %>% html_elements("tr")
  nodes_that_have_table_data = html %>% html_elements("tr") %>% html_attr("data-rnw-int-class")
  
  nodes_to_check = (1:length(html_div_nodes))[nodes_that_have_table_data == "table-row____"]
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
ica_instance_table = get_instance_type_table(url=instance_type_table_url)
ica_instance_table$CPUs = as.numeric(ica_instance_table$CPUs)
ica_instance_table$`Mem (GB)` = as.numeric(ica_instance_table$`Mem (GB)`)
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

override_module_config <- function(module_list){
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
