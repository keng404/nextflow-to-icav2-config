getInstancePodAnnotation <- function(cpus,mem,container_name,ica_instance_table){
  pod_annotation_prefix = paste("pod annotation:", "'scheduler.illumina.com/presetSize'", "value:")
  pod_annotation = NULL
  pod_value = NA
  if(length(cpus) > 0 && length(mem) > 0){
    search_query = ica_instance_table$CPUs >= max(cpus) &&  ica_instance_table$`Mem..GB.` >= max(mem)
  } else if(length(cpus) > 0 && length(mem) == 0){
    search_query = ica_instance_table$CPUs >= max(cpus) 
  } else if(length(cpus) == 0 && length(mem) > 0){
    search_query = ica_instance_table$`Mem..GB.` >= max(mem)
  } else{
    pod_annotation = paste(pod_annotation_prefix,"'himem-small'")
    return(pod_annotation)
  }
  if(!is.null(container_name) && grepl("dragen",container_name)){
      pod_annotation = paste(pod_annotation_prefix,"'fpga-medium'")
  } else if(sum(search_query) > 0){
    pod_value = ica_instance_table[search_query,]$`Compute.Type`[1]
    pod_annotation = paste(pod_annotation_prefix,paste("'",pod_value,"'",sep=""))
    return(pod_annotation)
  } else{
    pod_annotation = paste(pod_annotation_prefix,"'himem-small'")
    return(pod_annotation)  
    }
}


foi = "/Users/keng/nf-core/sarek/main.nf"

parseProcessesInNextflowScript <- function(nf_script,nf_process_metadata,default_container,ica_instance_table){
  nf_script_process_lines = list()
  dsl2_enabled = FALSE
  in_process = FALSE
  out_process = TRUE
  process_is_labeled = FALSE 
  process_label = "default"
  process_name = NULL
  container_specified = FALSE
  process_container = default_container
  cpus = 1
  memory = 1
  line_indent = ""
  # include { UntarVegaReference } from './tools-vega.nf'
  skip_line = FALSE ### skip comment lines 
  ### first pass to see if this is a DSL2 script
  nf_script_dat = read.delim(nf_script,quote = "",header=F)
  for(i in 1:nrow(nf_script_dat)){
    skip_line = FALSE
    line_split = strsplit(nf_script_dat[i,],"\\s+")[[1]]
    clean_line = line_split
    for(j in 1:length(line_split)){
      clean_line[j] = trimws(line_split[j])
    }
    clean_line = clean_line[clean_line!=""]
    if(grepl("/",clean_line[1])){
      skip_line = TRUE
    } else{
      if(grepl("nextflow.enable.dsl",nf_script_dat[i,])){
        if(clean_line[3] == "2"){
          dsl2_enabled = TRUE
        }
      }
    }
  }
  rlog::log_info(paste("script",nf_script,"is DSL2 enabled:",dsl2_enabled))
  #### now we will parse the nf script for processes, will collect line #s so that we know how to perform edits
  process_labels = c()
  line_numbers = c()
  process_cpus = c(cpus)
  process_memory = c(memory)
  process_lines = c()
  for(i in 1:nrow(nf_script_dat)){
    skip_line = FALSE       ### skip comment lines 
    line_split = strsplit(nf_script_dat[i,],"\\s+")[[1]]
    clean_line = line_split
    for(j in 1:length(line_split)){
      clean_line[j] = trimws(line_split[j])
    }
    clean_line = clean_line[clean_line!=""]
    clean_line_length = length(clean_line)
    if(grepl("/",clean_line[1])){
      skip_line = TRUE
    } 
    if(!skip_line){
      ### skip blank lines 
      if(length(clean_line)>0){
        line_numbers = c(line_numbers,i)
        if(clean_line[1] == "process"){
          in_process = TRUE
          out_process = FALSE
          process_name = clean_line[2]
          rlog::log_info(paste("FOUND_PROCESS_NAME:",process_name))
          nf_script_process_lines[[process_name]] = list()
          process_label = NULL
          container_specified = FALSE
          process_labels = c()
          line_numbers = c()
          process_cpus = c()
          process_memory = c()
          process_lines = c()
          process_container = default_container
          in_expression = FALSE
        } else if(in_process && (length(clean_line)>1) && ( clean_line[length(clean_line)] == "{" || grepl("if",nf_script_dat[i,]) || grepl("else",nf_script_dat[i,]) || grepl("def",nf_script_dat[i,]))){
          in_expression = TRUE
          rlog::log_info(paste("EXPRESSION_LINE:",nf_script_dat[i,]))
          #process_lines = c(process_lines,nf_script_dat[i,])
        } else if(in_process &&  (clean_line[1] == '"""'|| clean_line[1] == "}") && in_expression){
          in_expression = FALSE
          rlog::log_info(paste("EXPRESSION_LINE:",nf_script_dat[i,]))
          #process_lines = c(process_lines,nf_script_dat[i,])
        } else if(in_process && clean_line[1] == "}" && !in_expression){
          rlog::log_info(paste("ADDING_LINE:",nf_script_dat[i,]))
          process_lines = c(process_lines,nf_script_dat[i,])
          rlog::log_info(paste("UPDATING_PROCESS",process_name))
          nf_script_process_lines[[process_name]][["process_lines"]] = process_lines
          nf_script_process_lines[[process_name]][["process_labels"]] = process_labels
          nf_script_process_lines[[process_name]][["line_numbers"]] = line_numbers
          nf_script_process_lines[[process_name]][["cpus_parsed"]] = process_cpus
          nf_script_process_lines[[process_name]][["memeory_in_gb_parsed"]] = process_memory
          in_process = FALSE
          out_process = TRUE
          process_name = NULL
          process_label = NULL
          container_specified = FALSE
          in_expression = FALSE
          process_labels = c()
          line_numbers = c()
          process_cpus = c()
          process_memory = c()
          process_lines = c()
          process_container = default_container
        }
        if(in_process){
          ## main goal is to replace any line containing label, cpu, memory, errorStrategy, queue
          ## with pod annotation, container --- if not found, publishDir --- if not found or if it's not an expression
          ## 
          #line_numbers = c(line_numbers,i)
          rlog::log_info(paste("PROCESS_LINE:",nf_script_dat[i,]))
          if(clean_line[1] == "label"){
            process_is_labeled = TRUE
            line_indent = gsub("label","",nf_script_dat[i,])
            process_label = gsub("\\'","",clean_line[2])
            rlog::log_info(paste("PROCESS_LABEL:",process_label))
            process_labels = c(process_labels,process_label)
          }
          if(clean_line[1] == "input:"){
            line_indent = gsub("input:","",nf_script_dat[i,])
            rlog::log_info(paste("LINE_INDENT_FIRST_TOKEN:",line_indent))
            ############
            process_labels_exist = length(process_labels) > 0 
            process_labels_metadata_exist = sum(process_labels %in% names(nf_process_metadata)) > 0
            process_name_metadata_exist = sum(process_name %in% names(nf_process_metadata)) > 0
            ######################
            if(process_labels_exist){
              if(!(process_label %in% names(nf_process_metadata))){
                rlog::log_warn(paste("NO_PROCESS_LABEL:","For process name:",process_name))
              }
            } else{
              rlog::log_warn(paste("NO_PROCESS_LABEL:","For process name:",process_name))
            }
            rlog::log_info(paste("FOUND_PROCESS_LABEL:",paste(process_labels,collapse=",")))
            #### adding container
            if(!container_specified){
              ### check if process label identifies container
              if(process_labels_exist && process_labels_metadata_exist){
                for(pl in 1:length(process_labels)){
                  if(process_labels[pl] %in% names(nf_process_metadata)){
                    if("container" %in% names(nf_process_metadata[[process_labels[pl]]])){
                      rlog::log_info(paste("ADDING container line:",nf_process_metadata[[process_labels[pl]]][["container"]]))
                      if(grepl("\\:",nf_process_metadata[[process_labels[pl]]][["container"]])){
                        container_selection_split = strsplit(nf_process_metadata[[process_labels[pl]]][["container"]],"\\s+")[[1]]
                        container_string = paste(line_indent,"container ",paste("'",gsub("\"","",container_selection_split[length(container_selection_split)]),"'",sep=""),sep="")
                        process_lines = c(process_lines,container_string)
                      } else{
                        process_lines = c(process_lines,nf_process_metadata[[process_labels[pl]]][["container"]])
                      }
                      container_specified = TRUE
                      break
                    }
                  }
                }
              }
              ### check if process name identifies container
              if(!container_specified){
                if(process_name_metadata_exist){
                  if("container" %in% names(nf_process_metadata[[process_name]])){
                    rlog::log_info(paste("ADDING container line:",nf_process_metadata[[process_name]][["container"]]))
                    if(grepl("\\:",nf_process_metadata[[process_name]][["container"]])){
                      container_selection_split = strsplit(nf_process_metadata[[process_name]][["container"]],"\\s+")[[1]]
                      container_string = paste(line_indent,"container ",paste("'",gsub("\"","",container_selection_split[length(container_selection_split)]),"'",sep=""),sep="")
                      process_lines = c(process_lines,container_string)
                    } else{
                      process_lines = c(process_lines,nf_process_metadata[[process_name]][["container"]])
                    }
                    container_specified = TRUE
                  }
                }
              }
              ### add default container
              if(!container_specified){
                rlog::log_info(paste("ADDING container line:",paste(line_indent,"container",process_container)))
                process_lines = c(process_lines,paste(paste(line_indent,"container",sep=""),process_container))
              }
              #########
              if(length(process_cpus) == 0){
                if(process_labels_exist && process_labels_metadata_exist){
                  for(pl in 1:length(process_labels)){
                    if(process_labels[pl] %in% names(nf_process_metadata)){
                      if("cpus" %in% names(nf_process_metadata[[process_labels[pl]]])){
                        process_cpus = c(process_cpus,nf_process_metadata[[process_labels[pl]]][["cpus"]])
                      }
                    }
                  }
                }
                if(process_name_metadata_exist){
                  if("cpus" %in% names(nf_process_metadata[[process_name]])){
                    process_cpus = c(process_cpus,nf_process_metadata[[process_name]][["cpus"]])
                  }
                }
              }
              if(length(process_memory) == 0){
                if(process_labels_exist && process_labels_metadata_exist){
                  for(pl in 1:length(process_labels)){
                    if(process_labels[pl] %in% names(nf_process_metadata)){
                      if("mem" %in% names(nf_process_metadata[[process_labels[pl]]])){
                        process_memory = c(process_memory,nf_process_metadata[[process_labels[pl]]][["mem"]])
                      }
                    }
                  }
                }
                if(process_name_metadata_exist){
                  if("mem" %in% names(nf_process_metadata[[process_name]])){
                    process_memory = c(process_memory,nf_process_metadata[[process_name]][["mem"]])
                  }
                }
              }
            }
            process_pod_annotation = getInstancePodAnnotation(cpus = process_cpus,mem = process_memory,container_name = process_container,ica_instance_table = ica_instance_table)
            rlog::log_info(paste("ADDING_POD_ANNOTATION:",process_pod_annotation))
            process_lines = c(process_lines,paste(line_indent,process_pod_annotation,sep=""))
            rlog::log_info(paste("ADDING_LEINENT_ERROR_STRATEGY:",paste("errorStrategy 'ignore'",sep="")))
            process_lines = c(process_lines,paste(line_indent,"errorStrategy 'ignore'",sep=""))
            rlog::log_info(paste("ADDING_TIME_COMPONENT:",paste("time","'1day'")))
            process_lines = c(process_lines,paste(line_indent,paste("time","'1day'"),sep=""))
            rlog::log_info(paste("ADDING_LINE:",nf_script_dat[i,]))
            process_lines = c(process_lines,nf_script_dat[i,])
          } else if(clean_line[1] == "cpus"){
            line_indent = gsub("cpus","",nf_script_dat[i,])
            cpus = stroi(clean_line[2])
            process_cpus = c(process_cpus,cpus)
          } else if(clean_line[1] == "memory"){
            line_indent = gsub("memory","",nf_script_dat[i,])
            memory = stroi(gsub("\\.GB","",clean_line[2]))
            process_memory = c(process_memory,memory)
          } else if(clean_line[1] == "container"){
            line_indent = gsub("container","",nf_script_dat[i,])
            container_specified = TRUE
            process_container = clean_line[2]
            rlog::log_info(paste("ADDING_LINE:",nf_script_dat[i,]))
            process_lines = c(process_lines,nf_script_dat[i,])
          } else if(clean_line[1] == "publishDir"){
            line_indent = gsub("publishDir","",nf_script_dat[i,])
            publish_dir_metadata = str_extract(nf_script_dat[i,],"(?<=\\()[^\\)]+")
            if(!is.na(publish_dir_metadata)){
              publish_dir_metadata_location = strsplit(publish_dir_metadata,"\\,")[[1]][1]
              publish_dir_metadata_location_split = strsplit(publish_dir_metadata_location,"/")[[1]]
              if(publish_dir_metadata_location == "out" || "out" %in% publish_dir_metadata_location_split){
                rlog::log_info(paste("ADDING_LINE:",nf_script_dat[i,]))
                process_lines = c(process_lines,nf_script_dat[i,])
              } else{
                rlog::log_info(paste("ADDING_custom_line:",nf_script_dat[i,]))
                process_lines = c(process_lines,paste(line_indent,"publishDir","out","mode: copy") )
              }
            }
            ### check that publishDir is being sent to an expression
            else if(grepl("\\$\\{",nf_script_dat[i,])){
              process_lines = c(process_lines,nf_script_dat[i,])
            } else if(clean_line[2] == "out" || clean_line[2] == "'out'"){
              rlog::log_info(paste("ADDING_LINE:",nf_script_dat[i,]))
              process_lines = c(process_lines,nf_script_dat[i,])
            }
          } else if(clean_line[1] != "queue" && clean_line[1] != "label" && clean_line[1] != "errorStrategy" && clean_line[1] != "time" && clean_line[1] != "publishDir"){
            rlog::log_info(paste("ADDING_LINE:",nf_script_dat[i,]))
            process_lines = c(process_lines,nf_script_dat[i,])
          } else{
            if(clean_line[1] != "queue" && clean_line[1] != "label"){
              rlog::log_info(paste("ADDING_LINE:",nf_script_dat[i,]))
              process_lines = c(process_lines,nf_script_dat[i,])
            }
          }
        }
      }
    } else{
      if(in_process){
        rlog::log_info(paste("NOT_PARSING_LINE:",nf_script_dat[i,]))
        rlog::log_info(paste("ADDING_LINE:",nf_script_dat[i,]))
        process_lines = c(process_lines,nf_script_dat[i,])
        line_numbers = c(line_numbers,i)
      }
    }
  }
  return(nf_script_process_lines)
}

#updated_nf_processes = parseProcessesInNextflowScript(nf_script=foi,nf_process_metadata=nf_process_metadata,default_container = default_container,ica_instance_table = ica_instance_table)
#print(updated_nf_processes[["VEP"]])
#test_foi = "/Users/keng/test3.nf"
#updated_nf_processes2 = parseProcessesInNextflowScript(nf_script=test_foi,nf_process_metadata=nf_process_metadata,default_container = default_container,ica_instance_table = ica_instance_table)


