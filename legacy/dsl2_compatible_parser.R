library(rlog)
library(stringr)
#########
path_merge <- function(base_path,add_path){
  path_split = strsplit(add_path,"/")[[1]]
  if(length(path_split) == 1){
    return(file.path(base_path,add_path))
  } else if(path_split[1] == "." || path_split[1] == ".."){
    if(path_split[1] == "."){
      iters_to_remove = apply(t(path_split),2,function(x) x == ".")
      path_split = path_split[!iters_to_remove]
      return(file.path(base_path,paste(path_split[1:length(path_split)],collapse="/")))
    } else{
      iters_to_break = apply(t(path_split),2,function(x) x == "..")
      base_path_split = strsplit(base_path,"/")[[1]]
      iters_to_subtract = (length(base_path_split) + 1) - (1:length(path_split))[iters_to_break]
      base_path_iters_to_keep = (1:length(base_path_split) %in% iters_to_subtract)
      base_path_to_merge = paste(base_path_split[!base_path_iters_to_keep],collapse="/")
      add_path_to_merge = paste(path_split[!iters_to_break],collapse="/")
      return(file.path(base_path_to_merge,add_path_to_merge))
    }
  } else{
    return(file.path(base_path,add_path))
  }
}
#########
simplify_path <- function(path_of_interest){
  if(!is.character(path_of_interest)){
    rlog::log_error(paste("ERROR this is not a character:",path_of_interest))
    stop(paste("ERROR this is not a character:",path_of_interest))
  }
  path_split = strsplit(path_of_interest,"/")[[1]]
  if(sum("." %in% path_split) > 0){
    iters_to_remove = apply(t(path_split),2,function(x) x == ".")
    path_split = path_split[!iters_to_remove]
    simplified_path = paste(path_split[1:length(path_split)],collapse="/")
  } else if(sum(".." %in% path_split) > 0){
    path_components = strsplit(path_of_interest,"\\.\\./")[[1]]
    number_of_dirs_to_skip = sum(path_components == "")
    path_components = path_components[path_components != ""]
    for(s in 1:number_of_dirs_to_skip){
      path_components[1] = dirname(gsub("/$","",path_components[1]))
    }
    if(length(path_components) > 1){
      simplified_path = path_merge(path_components[1],paste(path_components[2],collapse="/"))
    } else{
      simplified_path = path_components[1]
    }
  } else{
    simplified_path = path_of_interest
  }
  return(simplified_path)
}
does_path_exist <- function(path_of_interest){
  if(!file.exists(path_of_interest)){
    rlog::log_error("Could not find file:",path_of_interest)
    return(FALSE)
  } else{
    return(TRUE)
  }
}
################
find_all_nf_scripts <- function(main_script){
  scripts_metadata = list()
  scripts_rename = list()
  scripts_to_look_at = c()
  skip_line = FALSE ### skip comment lines 
  dsl2_enabled = FALSE
  ### first pass to see if this is a DSL2 script
  nf_script_dat = read.delim(main_script,quote = "",header=F)
  for(i in 1:nrow(nf_script_dat)){
    rlog::log_info(paste("ON line: ",i,"in the file",main_script))
    skip_line = FALSE
    line_split = strsplit(as.character(nf_script_dat[i,]),"\\s+")[[1]]
    clean_line = line_split
    for(j in 1:length(line_split)){
      clean_line[j] = trimws(line_split[j])
    }
    clean_line = clean_line[clean_line!=""]
    clean_line = clean_line[!is.na(clean_line)]
    if(length(clean_line) < 1){
      skip_line = TRUE
    }
    rlog::log_info(paste("MY_LINE:",clean_line))
    if(grepl("/",clean_line[1])){
      skip_line = TRUE
    } else{
      ### check if script is DSL2 enabled
      if(grepl("nextflow.enable.dsl",nf_script_dat[i,])){
        if(clean_line[3] == "2"){
          dsl2_enabled = TRUE
          rlog::log_info(paste("script",main_script,"is DSL2 enabled:",dsl2_enabled))
        }
      }
      if(!skip_line){
        if(clean_line[1] == "include"){
          result = str_extract(clean_line, "(?<=\\{)[^\\}]+")
          process = strsplit(result,"as")[[1]][1]
          idx_of_interest = length(clean_line)
          for(pp in 1:length(clean_line)){
            if(clean_line[pp] == "from"){
              idx_of_interest = pp + 1
            }
          }
          location_to_add = gsub("'","",clean_line[idx_of_interest])
          location_to_add = gsub("\"","",location_to_add)
          initial_wd = getwd()
          setwd(dirname(main_script))
          if(!grepl(".nf$",location_to_add)){
            file_listing = normalizePath(paste(location_to_add,".nf",sep=""))
          } else{
            file_listing = normalizePath(location_to_add)
          }
          setwd(initial_wd)
          script_to_add = file_listing
          rlog::log_info(paste("Found_Scripts:",paste(script_to_add,collapse=", ")))
          ######### assumes we are writing a new file based on the original main_script
          final_relative_path = gsub(paste(main_script,"/",sep=""),"",script_to_add)
          new_file_name = gsub(".nf$",".dev.nf",final_relative_path)
          new_file_path = file.path(dirname(location_to_add),basename(new_file_name))
          scripts_rename[[clean_line[length(clean_line)]]] = new_file_path
          #############################################################
          scripts_to_look_at = c(scripts_to_look_at,script_to_add)
          scripts_metadata[[clean_line[length(clean_line)]]] = script_to_add
        }
      }
    }
  }
  if(length(scripts_rename)>0){
    scripts_metadata[["scripts_rename"]] = scripts_rename
  }
  if(length(scripts_to_look_at) > 0) {
    scripts_metadata[["scripts_to_look_at"]] = scripts_to_look_at
  }
  if(length(scripts_metadata) == 0 | length(names(scripts_metadata)) == 0 ){
    scripts_metadata = NULL
  } else  if(length(scripts_metadata[["scripts_rename"]]) == 0  || length(scripts_metadata[["scripts_to_look_at"]]) == 0){
    scripts_metadata = NULL
  }
  return(scripts_metadata)
}
###########################
getWorkflowEvents <- function(script){
  workflowEventMetadata = list()
  in_workflow_event_process = FALSE
  out_workflow_event_process = TRUE
  line_indent = ""
  in_expression = FALSE
  out_expression = TRUE
  in_closure = FALSE
  skip_line = FALSE ### skip comment lines 
  workflow_events = c("workflow.onComplete","workflow.onError")
  expression_directives = c("def","if","else","try","catch")
  ### workflow.onComplete, workflow.onError
  ### get process lines
  ### get line numbers in the script
  nf_script_dat = read.delim(script,quote = "",header=F)
  for(i in 1:nrow(nf_script_dat)){
    skip_line = FALSE
    line_split = strsplit(nf_script_dat[i,],"\\s+")[[1]]
    clean_line = line_split
    for(j in 1:length(line_split)){
      clean_line[j] = trimws(line_split[j])
    }
    clean_line = clean_line[clean_line!=""]
    clean_line = clean_line[!is.na(clean_line)]
    if(grepl("/",clean_line[1])){
      skip_line = TRUE
      if(skip_line && in_workflow_event_process){
        rlog::log_info(paste("Adding comment line for workflow event:",workflow_event_name))
        rlog::log_info(paste("PROCESS_LINE:",nf_script_dat[i,]))
        process_lines = c(process_lines,nf_script_dat[i,])
        line_numbers = c(line_numbers,i)
      }
    } else{
      if(clean_line[1] %in% workflow_events || sum(workflow_events %in% clean_line[1]) > 0){
        in_workflow_event_process = TRUE
        out_workflow_event_process = FALSE
        in_expression = FALSE
        out_expression = TRUE
        workflow_event_name = gsub("\\{","",clean_line[1])
        rlog::log_info(paste("Found workflow event:",workflow_event_name))
        rlog::log_info(paste("PROCESS_LINE:",nf_script_dat[i,]))
        workflowEventMetadata[[workflow_event_name]] = list()
        process_lines = c(nf_script_dat[i,])
        line_numbers = c(i)
        in_closures = c()
      } else if(in_workflow_event_process && clean_line[1] != "}"){
        rlog::log_info(paste("PROCESS_LINE:",nf_script_dat[i,]))
        process_lines = c(process_lines,nf_script_dat[i,])
        line_numbers = c(line_numbers,i)
        if(sum(clean_line %in% expression_directives) > 0){
          if(clean_line[length(clean_line)] != "}" && "{"  %in% clean_line){
            in_expression = TRUE
            out_expression = FALSE
            in_closure = TRUE
            in_closures = c(in_closures,in_closure)
            rlog::log_info(paste("Found expression in workflow event:",nf_script_dat[i,]))
          }
        } 
      } else if(in_workflow_event_process && clean_line[1] == "}" && !out_expression){
        rlog::log_info(paste("PROCESS_LINE:",nf_script_dat[i,]))
        if(in_closure){
          if(! "{"  %in% clean_line){
            iter_to_change = length(in_closures) - sum(in_closures == FALSE)
            rlog::log_info(paste("updating in_closures:",iter_to_change,"number of in_closures",length(in_closures),nf_script_dat[i,]))
            in_closures[iter_to_change] = FALSE
          }
        } 
        if(sum(!in_closures) == length(in_closures)) {
          in_expression = FALSE
          out_expression = TRUE
          in_closures = c()
          rlog::log_info(paste("Exiting expression in workflow event:",nf_script_dat[i,]))
        }
        process_lines = c(process_lines,nf_script_dat[i,])
        line_numbers = c(line_numbers,i)
      } else if(in_workflow_event_process && clean_line[1] == "}" && out_expression){
        line_split1 = strsplit(nf_script_dat[i+1,],"\\s+")[[1]]
        clean_next_line = line_split1
        for(j in 1:length(line_split1)){
          clean_next_line[j] = trimws(line_split1[j])
        }
        clean_next_line = clean_next_line[clean_next_line!=""]
        clean_next_line_strsplit = strsplit(clean_next_line[length(clean_next_line)],"")[[1]]
        #################
        rlog::log_info(paste("PROCESS_LINE:",nf_script_dat[i,]))
        process_lines = c(process_lines,nf_script_dat[i,])
        line_numbers = c(line_numbers,i)
        rlog::log_info(paste("Updating workflow event:",workflow_event_name))
        workflowEventMetadata[[workflow_event_name]][["process_lines"]] = process_lines
        workflowEventMetadata[[workflow_event_name]][["line_numbers"]] = line_numbers
        workflowEventMetadata[[workflow_event_name]][["line_indent"]] = line_indent
        in_workflow_event_process = FALSE
        out_workflow_event_process = TRUE
        process_lines = c()
        line_numbers = c()
        in_expression = FALSE
        out_expression = TRUE
        in_closures = c()
      }
    }
  }
  return(workflowEventMetadata)
}
############
######## stub for getting actual path
#########################
## uncomment below to test
#main_script = "/Users/keng/nf-core/rnaseq/main.nf"
#scripts_to_check = find_all_nf_scripts(main_script=main_script)
#if(length(scripts_to_check) > 0){
#if(scripts_to_check == script_to_check){
  #workflow_script = list.files(path_merge(dirname(main_script),"workflows"),pattern="*.nf",full.names = T)
  #workflow_script = workflow_script[!is.na(workflow_script)]
#  workflow_script = scripts_to_check
#  scripts_to_add = workflow_script
#  scripts_to_parse = workflow_script
#  while(length(scripts_to_add) > 0){
#    if(length(scripts_to_add) > 0){
#      scripts_to_parse = c()
#      for(i in 1:length(scripts_to_add)){
#        rlog::log_info(paste("Looking through the following script:",paste(scripts_to_add[i],sep=", ")))
#        scripts_of_interest_metadata = find_all_nf_scripts(main_script=simplify_path(scripts_to_add[i]))
#        scripts_of_interest = scripts_of_interest_metadata[["scripts_to_look_at"]]
#        second_pass_scripts = scripts_of_interest[!scripts_of_interest %in% scripts_to_check]
#        if(length(second_pass_scripts) > 0){
#          path_exists_check  = apply(t(second_pass_scripts),2, function(x) does_path_exist(x))
#          if(sum(!path_exists_check) > 0){
#            fois = second_pass_scripts[!path_exists_check]
#            stop(paste("DOUBLE-CHECK if these scripts exist:",paste(fois,collapse=", ")))
#          }
#        }
#        scripts_to_parse = c(scripts_to_parse,scripts_of_interest[!scripts_of_interest %in% scripts_to_check])
#        scripts_to_check = c(scripts_to_check,scripts_of_interest[!scripts_of_interest %in% scripts_to_check])
#      }
#    }
#    scripts_to_add = scripts_to_parse
#  } 
#} else{
#  rlog::log_warn(paste("Did not find additional NF scripts to parse in addition to:",main_script))
#}
#scripts_to_check = apply(t(unique(scripts_to_check)),2,function(x) simplify_path(x))
#rlog::log_info(paste("Parsing through the following scripts:",paste(scripts_to_check,sep=", ")))


#####
## for each module/sub-module process
## update it and generate new file
## store those files and the original representations --- update the appropriate statements in the Main.nf and workflow.nf file
## create dummy submodule for copying intermediate files
## add this process to main script and call it
##########3

#### read in NF script and modify container amap
fixNullContainerMap <- function(nf_script){
  nf_script_dat = read.delim(nf_script,quote="",header=F)
  lines_to_keep = c()
  line_numbers = c()
  for(i in 1:nrow(nf_script_dat)){
    skip_line = FALSE
    line_split = strsplit(as.character(nf_script_dat[i,]),"\\s+")[[1]]
    clean_line = line_split
    for(j in 1:length(line_split)){
      clean_line[j] = trimws(line_split[j])
    }
    clean_line = clean_line[clean_line!=""]
    clean_line = clean_line[!is.na(clean_line)]
    if(length(clean_line) < 1){
      lines_to_keep = c(lines_to_keep,nf_script_dat[i,])
      line_numbers = c(line_numbers,i)
    } else{
      if(grepl("/",clean_line[1])){
        skip_line = TRUE
        if(skip_line){
          rlog::log_info(paste("Adding comment line:"))
          rlog::log_info(paste("PROCESS_LINE:",nf_script_dat[i,]))
          lines_to_keep = c(lines_to_keep,nf_script_dat[i,])
          line_numbers = c(line_numbers,i)
        }
      } else{
        if(clean_line[1] == 'container'){
          idx_of_interest = 2
          if(is.null(clean_line[idx_of_interest]) || clean_line[idx_of_interest] == 'null'){
            rlog::log_info(paste("found null container reference:",nf_script_dat[i,]))
            nf_script_dat[i,] = gsub('null',paste("'",'library/ubuntu:20.04',"'",sep=""),nf_script_dat[i,])
            rlog::log_info(paste("Changing line to:",nf_script_dat[i,]))
            lines_to_keep = c(lines_to_keep,nf_script_dat[i,])
            line_numbers = c(line_numbers,i)
          } else{
            lines_to_keep = c(lines_to_keep,nf_script_dat[i,])
            line_numbers = c(line_numbers,i)
          }
        } else{
          lines_to_keep = c(lines_to_keep,nf_script_dat[i,])
          line_numbers = c(line_numbers,i)
        }
      }
    }
  }
  modified_script = gsub(".nf$",".dev.nf",nf_script)
  write.table(lines_to_keep,file=modified_script,sep="\n",row.names=F,col.names = F,quote=F)
  rlog::log_info(paste("Checking for null container maps",nf_script))
  system(paste("cp",modified_script,nf_script))
}
#### check for syntax errors regarding processes in nextflow script
processEnclosureCheck <- function(processLines){
  modProcessLines = list()
  if(length(processLines) >0 ){
    process_names = names(processLines)
    rlog::log_info(paste("PROCESS_NAMES:",paste(names(processLines),collapse=",")))
    for(i in 1:length(process_names)){
      process_name = process_names[i]
      process_lines = processLines[[process_name]][["process_lines"]]
      my_left_braces = c()
      my_right_braces = c()
      for(j in 1:length(process_lines)){
        skip_line = FALSE
        line_split = strsplit(as.character(process_lines[j]),"\\s+")[[1]]
        clean_line = line_split
        for(k in 1:length(line_split)){
          clean_line[k] = trimws(line_split[k])
        }
        clean_line = clean_line[clean_line!=""]
        clean_line = clean_line[!is.na(clean_line)]
        if(length(clean_line) >0){
          if(grepl("/",clean_line[1])){
            skip_line = TRUE
            if(skip_line){
              rlog::log_info(paste("found comment line:"))
            }
          } else{
            for(item in 1:length(clean_line)){
              if(clean_line[item] == "{"  || grepl("\\{",clean_line[item])){
                rlog::log_info(paste("OPEN_BRACKET_LINE:",process_lines[j]))
                my_left_braces = c(my_left_braces,"{")
              }
              if(clean_line[item] == "}" || grepl("\\}",clean_line[item])){
                rlog::log_info(paste("CLOSED_BRACKET_LINE:",process_lines[j]))
                my_right_braces = c(my_right_braces,"}")
              }
            }
          }
        }
      }
      modProcessLines[[process_name]] = processLines[[process_name]]
      if(length(my_left_braces) > length(my_right_braces)){
        braces_to_add = length(my_left_braces) - length(my_right_braces)
        
        rlog::log_info(paste("Adding braces to:",process_name))
        for(bta in 1:braces_to_add){
          if(process_lines[length(process_lines)] != "}"){
            process_lines  = c(process_lines,"}")
          }
        }
        modProcessLines[[process_name]][["process_lines"]] = process_lines
      } else if(length(my_right_braces) > length(my_left_braces)){
        rlog::log_error(paste("Whoa! There's too many closed braces '}'"))
      }
    }
  }
  return(modProcessLines)
}
#################
statement_prefixes  = c('if','when','def','else','else if')
loadModuleMetadata <- function(config_files){
  modulesMetadata = list()
  statement_left_brackets = c()
  statement_right_brackets = c()
  for(i in 1:length(config_files)){
    config_file_dat = read.delim(config_files[i],header=F,quote="")
    in_module_closure = FALSE
    in_process_closure = FALSE
    in_expression = FALSE
    condition_for_config = "default"
    parameter_name = "unknown"
    value_collection = c()
    for(j in 1:nrow(config_file_dat)){
      skip_line = FALSE
      line_split = strsplit(config_file_dat[j,],"\\s+")[[1]]
      clean_line = line_split
      for(k in 1:length(line_split)){
        clean_line[k] = trimws(line_split[k])
      }
      clean_line = clean_line[clean_line!=""]
      clean_line = clean_line[!is.na(clean_line)]
      if(length(clean_line) >0){
        if(grepl("/",clean_line[1])){
          skip_line = TRUE
          if(skip_line){
            rlog::log_info(paste("Found comment line:",config_file_dat[j,]))
          }
        } else{
          rlog::log_info(paste("CONFIG_LINE_OF_INTEREST:",config_file_dat[j,]))
          rlog::log_info(paste("PROCESS_CLOSURE:",in_process_closure))
          rlog::log_info(paste("MODULE_CLOSURE:",in_module_closure))
          rlog::log_info(paste("CLEANED_LINE:",paste(clean_line,collapse=" ")))
          if(clean_line[1] == "process"){
            in_process_closure = TRUE
          } else if(clean_line[1] %in% statement_prefixes){
            rlog::log_info(paste("IN_EXPRESSION",paste(clean_line,collapse=" ")))
            in_expression = TRUE
            statement_left_brackets = c()
            statement_right_brackets = c()
            right_brackets_to_add = unlist(str_extract_all(clean_line, "\\}"))
            if(length(right_brackets_to_add) >0){
              statement_right_brackets = c(statement_right_brackets,right_brackets_to_add)
            }
            left_brackets_to_add = unlist(str_extract_all(clean_line, "\\{"))
            if(length(left_brackets_to_add) >0){
              statement_left_brackets = c(statement_left_brackets,left_brackets_to_add)
            }
            if(clean_line[1] == "if" || clean_line[1] == "else" || clean_line[1] == "else if"){
              condition_for_config = paste(clean_line,collapse =" ")
            }
            ### check if we've exited the expression
           # if(clean_line[length(clean_line)] == "}" || grepl("\\{$",clean_line[length(clean_line)])){
            if(length(statement_right_brackets) >0 || length(statement_left_brackets) > 0){
              rlog::log_info(paste("right brackets:",paste(statement_right_brackets,collapse=", ")))
              rlog::log_info(paste("left brackets:",paste(statement_left_brackets,collapse=", ")))
              if(length(statement_right_brackets) == length(statement_left_brackets)){
                in_expression = FALSE
                statement_left_brackets = c()
                statement_right_brackets = c()
                rlog::log_info(paste("EXITING_EXPRESSION"))
              }
            }
          #  }
          } else if(in_process_closure && clean_line[1] == "withName:" ){
            in_module_closure = TRUE
            module_name = gsub("\\'","",trimws(clean_line[2]))
            rlog::log_info(paste("Initializing info for module:",module_name,"condition:",condition_for_config))
            modulesMetadata[[module_name]] = list()
            modulesMetadata[[module_name]][[condition_for_config]] = list()
            moduleMetadata = list()
          } else if(in_process_closure && clean_line[1] == "if" && in_expression){
            condition_for_config = config_file_dat[j,]
            right_brackets_to_add = unlist(str_extract_all(clean_line, "\\}"))
            if(length(right_brackets_to_add) >0){
              statement_right_brackets = c(statement_right_brackets,right_brackets_to_add)
            }
            left_brackets_to_add = unlist(str_extract_all(clean_line, "\\{"))
            if(length(left_brackets_to_add) >0){
              statement_left_brackets = c(statement_left_brackets,left_brackets_to_add)
            }
            rlog::log_info(paste("Found conditional configurations for module:",condition_for_config))
          } else if(in_process_closure && in_module_closure && length(value_collection) > 0 && clean_line[1] != "]"){
            if("=" %in% clean_line){
              if(parameter_name != "unknown"){
                rlog::log_info(paste("Point3 : Found value for parameter:",parameter_name,"value:",value_collection))
                moduleMetadata[[parameter_name]] = value_collection
                value_collection = c()
                parameter_name = clean_line[1]
                parameter_name_split = strsplit(parameter_name,"\\.")[[1]]
                if(parameter_name_split[1] == "ext"){
                  parameter_name = paste("process.",parameter_name,sep="")
                }
              }
              if(!"]" %in% clean_line){
                value_collection = paste(clean_line[3:length(clean_line)])
                value_collection = gsub("\\{","",value_collection)
                value_collection = gsub("\\}","",value_collection)
                if(value_collection != "["){
                  if(parameter_name != "unknown"){
                    rlog::log_info(paste("Point1 : Found value for parameter:",parameter_name,"value:",value_collection))
                    moduleMetadata[[parameter_name]] = value_collection
                    value_collection = c()
                  }
                }
              }
            } else{
              value_collection = c(value_collection,paste(clean_line,collapse=" "))
            }
          } else if(in_process_closure && in_module_closure && length(value_collection) > 0 && clean_line[1] == "]"){
            if(sum(grepl("join",clean_line)) > 0){
              value_collection = c(value_collection,paste(clean_line,collapse=" "))
            }            
            value_collection = paste(value_collection,collapse=" ")
            value_collection = strsplit(value_collection,"\\]\\,\\[")[[1]]
            value_collection = apply(t(value_collection),2, function(x) gsub("\\[","",x))
            if(parameter_name != "unknown"){
              rlog::log_info(paste("Point2 : Found value for parameter:",parameter_name,"value:",value_collection))
              moduleMetadata[[parameter_name]] = value_collection
              value_collection = c()
            }
          } else if(in_process_closure && in_module_closure && (clean_line[1] == "}"  || grepl("\\}$",clean_line[length(clean_line)]))){
            in_module_closure = FALSE
            modulesMetadata[[module_name]][[condition_for_config]] = moduleMetadata
            rlog::log_info(paste("Updating info for module:",module_name,"condition:",condition_for_config))
          } else if(in_process_closure && in_module_closure && clean_line[1] == "]" ){
            rlog::log_info(paste("Skipping line"))
          } else if(in_process_closure && in_module_closure && "=" %in% clean_line){
            parameter_name = clean_line[1]
            parameter_name_split = strsplit(parameter_name,"\\.")[[1]]
            if(parameter_name_split[1] == "ext"){
              parameter_name = paste("process.",parameter_name,sep="")
            }
            if(parameter_name == "publishDir"){
              rlog::log_info(paste("IGNORING publishDir configuration for now"))
             # value_collection = c(value_collection,paste(clean_line[3:length(clean_line)],collapse=" "))
            } else{
              value_collection = paste(clean_line[3:length(clean_line)],collapse=" ")
              value_collection = gsub("\\{","",value_collection)
              value_collection = gsub("\\}","",value_collection)
              if(value_collection != "["){
                if(parameter_name != "unknown"){
                  rlog::log_info(paste("Point1 : Found value for parameter:",parameter_name,"value:",value_collection))
                  moduleMetadata[[parameter_name]] = value_collection
                  value_collection = c()
                }
              }
            }
          } else if(in_process_closure && !in_module_closure && (clean_line[1] == "}" || grepl("\\}$",clean_line) )){
            in_process_closure = FALSE
            rlog::log_info(paste("Exiting process closure"))
          } else if(!in_process_closure && !in_module_closure && in_expression){
            rlog::log_info(paste("Checking expression closure"))
            right_brackets_to_add = unlist(str_extract_all(clean_line, "\\}"))
            if(length(right_brackets_to_add) >0){
              statement_right_brackets = c(statement_right_brackets,right_brackets_to_add)
            }
            left_brackets_to_add = unlist(str_extract_all(clean_line, "\\{"))
            if(length(left_brackets_to_add) >0){
              statement_left_brackets = c(statement_left_brackets,left_brackets_to_add)
            }
            ### check if we've exited the expression
         #   if(clean_line[length(clean_line)] == "}" || grepl("\\{$",clean_line[length(clean_line)])){
            if(length(statement_right_brackets) > 0 || length(statement_left_brackets) > 0){
              rlog::log_info(paste("right brackets:",paste(statement_right_brackets,collapse=", ")))
              rlog::log_info(paste("left brackets:",paste(statement_left_brackets,collapse=", ")))
              if(length(statement_right_brackets) == length(statement_left_brackets)){
                in_expression = FALSE
                statement_left_brackets = c()
                statement_right_brackets = c()
                rlog::log_info(paste("EXITING_EXPRESSION"))
              }
            }
          #  }
          }
        }
      }
    }
  }
  return(modulesMetadata)
}
############
#x = loadModuleMetadata("/Users/keng/codes/mycosnp-nf/conf/modules.config")
#y = loadModuleMetadata("/Users/keng/nf-core/rnaseq/conf/modules.config")
findModules <- function(nf_file){
  modules_of_interest = list()
  nf_file_dat = read.delim(nf_file,header=F,quote="")
  file_mappings = find_all_nf_scripts(nf_file)
  relative_files = names(file_mappings[["scripts_rename"]])
  full_file_names = file_mappings[["scripts_to_look_at"]]
  for(j in 1:nrow(nf_file_dat)){
    skip_line = FALSE
    line_split = strsplit(nf_file_dat[j,],"\\s+")[[1]]
    clean_line = line_split
    for(k in 1:length(line_split)){
      clean_line[k] = trimws(line_split[k])
    }
    clean_line = clean_line[clean_line!=""]
    clean_line = clean_line[!is.na(clean_line)]
    if(length(clean_line) >0){
      if(grepl("/",clean_line[1])){
        skip_line = TRUE
        if(skip_line){
          rlog::log_info(paste("Found comment line:",nf_file_dat[j,]))
        }
      } else{
        if(clean_line[1] == "include"){
          rlog::log_info(paste("Found line of interest:",nf_file_dat[j,]))
          include_statement = str_extract(nf_file_dat[j,], "(?<=\\{)[^\\}]+")
          include_statement = apply(t(include_statement), 2, function(elem) strsplit(elem,"\\s+")[[1]])
          include_statement = apply(t(include_statement), 2, function(elem) trimws(elem))
          include_statement = include_statement[include_statement!=""]
          include_statement = include_statement[!is.na(include_statement)]
          rlog::log_info(paste("INCLUDE_STATEMENT:",paste(include_statement,collapse=" ")))
          relative_file_path = strsplit(nf_file_dat[j,],"\\s+")[[1]]
          relative_file_path = relative_file_path[length(relative_file_path)]
          relative_file_path_split = strsplit(relative_file_path,"/")[[1]]
          #if(!grepl(".nf$",relative_file_path)){
          #  relative_file_path = paste(relative_file_path,".nf",sep="")
          #}
            rlog::log_info(paste("Relative_FILE_PATH:",relative_file_path))
            if(length(include_statement) > 1){
              for(k in 1:(length(include_statement)-1)){
                if(include_statement[k + 1] == "as"){
                  rlog::log_info(paste("FOUND:", paste(file_mappings[[relative_file_path]],collapse=",",sep=" ")))
                  modules_of_interest[[include_statement[k + 2]]][["line_number"]] = j
                  modules_of_interest[[include_statement[k + 2]]][["module_path"]] = file_mappings[[relative_file_path]]
                  modules_of_interest[[include_statement[k + 2]]][["original_module_name"]] = include_statement[k]
                }
              }
            } else{
              modules_of_interest[[include_statement[1]]][["line_number"]] = j
              modules_of_interest[[include_statement[1]]][["module_path"]] = file_mappings[[relative_file_path]]
            }
          } else{
            if(clean_line[1] %in% names(modules_of_interest)){
              modules_of_interest[[clean_line[1]]][["line_number"]]  = c(modules_of_interest[[clean_line[1]]][["line_number"]] ,j)
            }
          }
      }
    } else{
      rlog::log_info(paste("Skipping",nf_file_dat[j,]))
    }
  }
  return(modules_of_interest)
}
######################################
#z  = findModules("/Users/keng/codes/mycosnp-nf/workflows/mycosnp.nf")
#z1 = findModules("/Users/keng/nf-core/rnaseq/workflows/rnaseq.nf")
##########################################################
publishStatementCheck <- function(process_lines){
  publishDir_statement_exists = FALSE
  for(i in 1:length(process_lines)){
    #rlog::log_info(paste("PUBLISH_DIR_LINE_PRINT:",process_lines[i]))
    if(length(strsplit(process_lines[i],"\\s+")[[1]]) > 0){
     # if("publishDir" %in% strsplit(process_lines[i],"\\s+")[[1]]){
      if(strsplit(process_lines[i],"\\s+")[[1]][1] == "publishDir"){
        publishDir_statement_exists = TRUE
      }
      if("publishDir" %in% strsplit(process_lines[i],"\\s+")[[1]][1]){
        publishDir_statement_exists = TRUE
      }
    }
  }
  rlog::log_info(paste("PUBLISH_DIR_STATEMENT_EXISTS:",publishDir_statement_exists))
  return(publishDir_statement_exists)
}

addPublishStatement <- function(process_lines,publish_dir_path=NULL){
  publish_dir_statement = NULL
  if(is.null(publish_dir_path)){
    rlog::log_error(paste("No argument provided for publish_dir_path"))
    publish_dir_path = "${params.outdir}"
  }
  new_process_lines = process_lines
  does_publish_statement_exist = publishStatementCheck(process_lines)
  if(!is.null(publish_dir_path)){
    publish_dir_statement = paste(paste(paste("","publishDir  ",collapse="\t",sep="\t"), 'path: { "',publish_dir_path,'"}',sep=""),paste(', mode: "copy", ','saveAs: { filename -> filename.equals(\'versions.yml\') ? null : filename }',sep=""),collapse="",sep="")
    #publish_dir_statement = gsub("outdir_custom1",publish_dir_path,publish_dir_statement)
    rlog::log_info(paste("PUBLISH_DIR_STATEMENT:",publish_dir_statement))
    idx = 1
    for(i in 1:length(process_lines)){
      if(grepl("\\{",process_lines[i])){
        idx = i
        break
      }
    }
    rlog::log_info(paste("IDX is",idx))
    rlog::log_info(paste("PROCESS_LENGTH IS",length(publish_dir_statement)))
    if(!does_publish_statement_exist && !is.null(publish_dir_statement)){
      new_process_lines = c(process_lines[idx],publish_dir_statement,process_lines[(idx+1):length(process_lines)])
    }
  }
  print(new_process_lines)
  return(new_process_lines)
}
############################################
originalModuleMapper <- function(module_list){
  og_mapper = list()
  for(i in 1:length(names(module_list))){
    if("original_module_name" %in% names(module_list[[names(module_list)[i]]]) ){
      og_module = module_list[[names(module_list)[i]]][["original_module_name"]]
    } else{
      og_module = names(module_list)[i]
    }
    if(length(names(og_mapper))>0){
      if(og_module %in% names(og_mapper)){
        og_mapper[[og_module]] = c(og_mapper[[og_module]],names(module_list)[i])
      } else{
        og_mapper[[og_module]] = names(module_list)[i]
      }
    } else{
      og_mapper[[og_module]] = names(module_list)[i]
    }
  }
  return(og_mapper)
}
simplifyExpression <- function(groovy_expression){
  just_groovy_expression = str_extract(groovy_expression, "(?<=\\{)[^\\}]+")
  return(just_groovy_expression)
}
getCustomOutdirName <- function(module_name){
  outdir_base = '${params.outdir}'
  additional_dir_path = paste(rev(strsplit(tolower(module_name),"[-_]")[[1]]),collapse="/")
  # remove special characters in basename of outdir path
  additional_dir_path = gsub("[[:punct:]]", "", additional_dir_path)  
  return(paste(outdir_base,additional_dir_path,sep="/"))
}

# if module has just one avatar or multiple avatars, then add publish statement and add params.outdir_custom to the appropriate line(s) in the
# intermediate data structure : dictionary of line edits where key is line# and the value is line edits
# inputs:
# output from findModules
# for each module grab process lines and add publishstatement
    # - dictionary of process lines
    # - pass this dictionary to addPublishStatement function
# output: new_lines
moduleNameMatcher <- function(module_metadata,query_name){
  names_of_interest = names(module_metadata)
  name_of_interest = NULL
  for(i in 1:length(names_of_interest)){
    module_name = names_of_interest[i]
    match_found  = FALSE
    if(grepl("|",module_name)){
      module_names = strsplit(module_name,"\\|")[[1]]
      for(j in 1:length(module_names)){
        if(match_found){
          break
        }
        if(!is.na(str_extract(module_names[j],".*"))){
          name_stub = gsub("\\.\\*","",module_name)
          if(name_stub != ""){
            if(grepl(name_stub,query_name)){
              name_of_interest = module_name
              match_found = TRUE
            }
          }
        } else{
          if(module_names[j] == query_name){
            name_of_interest = module_name
            match_found = TRUE
          }
        }
      }
    } else if(grepl(":",module_name)){
      module_names = strsplit(module_name,":")[[1]]
      module_names = c(module_names[length(module_names)])
      for(j in 1:length(module_names)){
        if(!is.na(str_extract(module_names[j],".*"))){
          name_stub = gsub("\\.\\*","",module_name)
          if(name_stub != ""){
            if(grepl(name_stub,query_name)){
              name_of_interest = module_name
              match_found = TRUE
            }
          }
        } else{
          if(module_names[j] == query_name){
            name_of_interest = module_name
            match_found = TRUE
          }
        }
      }
    } else{
      if(module_name == query_name){
        name_of_interest = module_name
        match_found = TRUE
      }
    }
  }
  return(name_of_interest)
}
############################
needsTaskCpuOrMemDeclaration <- function(process_lines){
  needs_declaration = list()
  needs_declaration['task.cpus'] = FALSE
  needs_declaration['task.memory'] = FALSE
  memory_declaration = FALSE
  cpu_declaration = FALSE
  for(i in 1:length(process_lines)){
    line_tokenization = strsplit(process_lines[i],"\\s+")[[1]]
    line_tokenization = apply(t(line_tokenization),2,trimws)
    line_tokenization = line_tokenization[line_tokenization!=""]
    if(length(line_tokenization) > 0){
      if(line_tokenization[1] == "cpus"){
        cpu_declaration = TRUE
      }
      if(line_tokenization[1] == "memory"){
        memory_declaration = TRUE
      }
      if(!cpu_declaration && grepl("task.cpus",process_lines[i])){
        needs_declaration['task.cpus'] = TRUE
      }
      if(!memory_declaration && grepl("task.memory",process_lines[i])){
        needs_declaration['task.memory'] = TRUE
      }
    }
  }
  return(needs_declaration)
}
grabPodAnnotation <- function(process_lines){
  is_pod_annotation_line = apply(t(process_lines),2,function(x) grepl("pod annotation:",x))
  if(sum(is_pod_annotation_line) > 0){
    pod_annotation_line = process_lines[is_pod_annotation_line][1]
    pod_annotation_line_split = strsplit(pod_annotation_line,"\\s+")[[1]]
    pod_annotation_line_split = pod_annotation_line_split[pod_annotation_line_split!=""]
    pod_annotation_label = pod_annotation_line_split[length(pod_annotation_line_split)]
    pod_annotation_label = gsub("'","",pod_annotation_label) 
    return(pod_annotation_label)
  } else{
    rlog::log_info(paste("grabPodAnnotation:PROCESS_OF_INTEREST:",process_lines))
    stop(paste("Could not find pod annotation line for process above"))
  }
}
###
## lookup_table format found in getInstancePodAnnotation function in the nf-core.ica_mod_nf_script.R file
generateMemOrCPUdeclarations <- function(declaration_list,pod_annotation,lookup_table){
  declarations_to_add = c()
  scale_factor = 0.75  # make sure we don't take up all CPUs and memory to avoid throtting/job failure that way
  if(is.null(pod_annotation)){
    if(declaration_list[['task.cpus']]){
      declarations_to_add = c(declarations_to_add,"\tcpus 5")
    }
    if(declaration_list[['task.memory']]){
      declarations_to_add = c(declarations_to_add,"\tmemory 20GB")
    }
  } else{
    lookup_query = lookup_table$`Compute.Type` == pod_annotation
    if( sum(lookup_query) > 0 ){
      if(declaration_list[['task.cpus']]){
        cpu_val = floor(scale_factor * strtoi(lookup_table[lookup_query,]$`CPU`[1]))
        declarations_to_add = c(declarations_to_add,paste("\tcpus",cpu_val))
      }
      if(declaration_list[['task.memory']]){
        mem_val = floor(scale_factor * strtoi(lookup_table[lookup_query,]$`Mem..GB.`[1]))
        declarations_to_add = c(declarations_to_add,paste("\tmemory",paste("'",mem_val," GB","'",sep="")))
      }
    } else{
      rlog::log_info(paste("ICA_INSTANCE_TABLE:",lookup_table))
      stop(paste("Could not find pod annotation label:",pod_annotation))
    }
  }
  return(declarations_to_add)
}
updateProcessDeclaratives <- function(process_lines,declarations){
  if(length(declarations) > 0){
    idx = 1
    for(i in 1:length(process_lines)){
      if(grepl("\\{",process_lines[i])){
        idx = i
        break
      }
    }
    rlog::log_info(paste("IDX is",idx))
    new_process_lines = c(process_lines[idx],declarations,process_lines[(idx+1):length(process_lines)])
    return(new_process_lines)
  } else{
    return(process_lines)
  }
}
findModuleConfigStatements <- function(lines_of_interest,module_name){
  moduleConfigs = list()
  module_lines = c()
  for(i in 1:length(lines_of_interest)){
    tokens = strsplit(lines_of_interest[i],"\\s+")[[1]]
    token_found = FALSE
    if(length(tokens[tokens != ""]) > 0){
      for(j in 1:length(tokens)){
        if(tokens[j] == "process"){
          module_name1 = gsub("{","",tokens[j+1])
          module_name1 = trimws(module_name1)
          if(module_name1 != ""){
            rlog::log_info(paste("REVERTING TO module name",module_name1,"from",module_name))
            module_name = module_name1
          } else{
            rlog::log_info(paste("Could not determine module name","from",module_name))
          }
        }
        token_of_interest = stringr::str_extract(tokens[j],"task.ext.+")
        ext_args_to_avoid = c('container','when')
        token_of_interest_split = strsplit(token_of_interest,"\\.")[[1]]
        token_of_interest_split_idx = token_of_interest_split[length(token_of_interest_split)]
        is_token_of_interest = 0
        if(!is.na(token_of_interest)){
          found_token_of_interest = apply(t(ext_args_to_avoid),2, function(x) grepl(x,token_of_interest_split_idx))
          is_token_of_interest = sum(found_token_of_interest)
        }
        if(!is.na(token_of_interest) & is_token_of_interest == 0){
          token_found = TRUE
          token_of_interest = gsub("[{|}]","",token_of_interest)
          token_of_interest = gsub("[[|]]","",token_of_interest)
          token_of_interest = trimws(token_of_interest)
          token_of_interest_split = strsplit(token_of_interest,"\\.")[[1]]
          new_name = paste("$params.def",module_name,token_of_interest_split[3:length(token_of_interest_split)],collapse="_",sep="_")
          moduleConfigs[[module_name]][[gsub("task","process",token_of_interest)]] = new_name
          tokens[j] = new_name
        } 
      }
    }
    if(token_found){
      rlog::log_info(paste("ADDING revised_LINE:",paste(tokens,collapse=" ")))
      new_line = paste(tokens,collapse=" ")
      new_line = gsub("\\$\\{meta.id\\}",paste("${meta.id}",tolower(module_name),collapse=".",sep="."),new_line)
      module_lines = c(module_lines,new_line)
    } else{
      new_line = gsub("\\$\\{meta.id\\}",paste("${meta.id}",tolower(module_name),collapse=".",sep="."),lines_of_interest[i])
      rlog::log_info(paste("ADDING unrevised_LINE:",new_line))
      module_lines = c(module_lines,new_line)
    }
  }
  if(length(names(moduleConfigs)) < 1){
    return(NULL)
  } else{
    moduleConfigs[[module_name]][["revised_lines"]] = c()
    moduleConfigs[[module_name]][["revised_lines"]] = module_lines
    return(moduleConfigs)
  }
}
#############################
makeFinalEdits <- function(nf_script,module_metadata,module_location){
  new_lines = c()
  new_lines = t(read.delim(nf_script,header=F,quote=""))
  path_split = strsplit(nf_script,"/")[[1]]
  is_workflow_or_subworkflow = FALSE
  # identify if nf_script is a workflow or subworkflow based on file_path
  if("workflows" %in% path_split || "subworkflows" %in% path_split){
    is_workflow_or_subworkflow = TRUE
  }
  line_edits = list()
  modules_of_interest = names(module_location)
  configurations_to_ignore = c("errorStrategy")
  for(i in 1:length(modules_of_interest)){
    module_of_interest = modules_of_interest[i]
    lines_to_add = c()
    # add publish statement and add params.outdir_custom to the appropriate line(s) in the new_lines
    line_numbers_of_interest = module_location[[module_of_interest]][["line_number"]]
    module_script = module_location[[module_of_interest]][["module_path"]]
    module_lines = t(read.delim(module_script,header=F,quote=""))
    module_lines1 = module_lines
    output_path = getCustomOutdirName(module_of_interest)
    rlog::log_info(paste("OUTPUT_PATH:",output_path))
    if(!publishStatementCheck(module_lines)){
      if(!is.null(output_path)){
        if(!is_workflow_or_subworkflow){
          module_lines1 = addPublishStatement(process_lines=module_lines,publish_dir_path=output_path)
        } else{
          rlog::log_info(paste("MAKE_FINAL_EDITS:SKIPPING PUBLISH_DIR STATEMENT FOR:",nf_script))
        }
      } else{
        rlog::log_error(paste("Cannot determine output path for the module",module_of_interest,output_path))
      }
    } else{
      module_lines1 = module_lines
    }
    ###########################################
    #if(paste(module_lines1,collapse="\n") != paste(module_lines,collapse="\n")){
    updated_module_script = gsub(".nf$",".dev.nf",module_script)
    print(module_lines1)
    ##################################
    module_extended_arguments = findModuleConfigStatements(module_lines1,module_of_interest)
    if(!is.null(module_extended_arguments)){
      module_lines1 = module_extended_arguments[[module_of_interest]][["revised_lines"]]
    } 
    ##################################
    write.table(x=module_lines1,file=updated_module_script,sep="\n",quote=F,row.names=F,col.names=F)
    rlog::log_info(paste("Generated updated module script to:",updated_module_script))
    system(paste("cp",updated_module_script,module_script))
    #}
    #publish_dir_statement = paste("publishDir", paste("\"",getCustomOutdirName(module_of_interest),"\"",sep=""),", mode:'copy'")
    #if(length(grepl("\\{$",lines_to_add[length(lines_to_add)])) == 0){
    #  lines_to_add = c(lines_to_add,publish_dir_statement)
    #} else if(!grepl("\\{$",lines_to_add[length(lines_to_add)])){
    #  lines_to_add = c(lines_to_add,publish_dir_statement)
    #} else{
    #  lines_to_add = c(lines_to_add,publish_dir_statement)
    #}
    rlog::log_info(paste("LOOKING_FOR_MODULE_CONFIGURATION_FOR:",module_of_interest))
    rlog::log_info(paste(names(module_metadata)))
    module_name_for_configuration = moduleNameMatcher(module_metadata,module_of_interest)
    if(is.null(module_name_for_configuration)){
      rlog::log_info(paste("Not adding additional configuration for:",module_of_interest,"in",nf_script))
    } else{
      rlog::log_info(paste("Checking",module_name_for_configuration,"for additional params"))
      configuration_scenarios = names(module_metadata[[module_name_for_configuration]])
      for(k in 1:length(configuration_scenarios)){
        configuration_parameters = names(module_metadata[[module_name_for_configuration]][[configuration_scenarios[k]]])
        configuration_parameters = configuration_parameters[!configuration_parameters %in% configurations_to_ignore]
        if(length(configuration_parameters) > 0 ){
          rlog::log_info(paste("Found the additional configuration parameters: ",paste(configuration_parameters,collapse = ", "),"for:",configuration_scenarios[k]))
          if(configuration_scenarios[k] != "default"){
            lines_to_add = c(lines_to_add,configuration_scenarios)
            for(k1 in 1:length(configuration_parameters)){
              parameter_value = module_metadata[[module_name_for_configuration]][[configuration_scenarios[k]]][[configuration_parameters[k1]]]
              if(length(parameter_value) > 1){
                parameter_value = paste(parameter_value , collapse = " ")
              }
              if(! configuration_parameters[k1] %in% names(module_extended_arguments[[module_of_interest]])){
                if(!grepl("\\$\\{meta",parameter_value) & !grepl("\\$\\meta",parameter_value)){
                  lines_to_add = c(lines_to_add,paste("   ",configuration_parameters[k1],"=",parameter_value))
                }
              } else{
                if(!grepl("\\$\\{meta",parameter_value) & !grepl("\\$\\meta",parameter_value)){
                  rlog::log_info(paste("Replacing",configuration_parameters[k1],"with",module_extended_arguments[[module_of_interest]][[configuration_parameters[k1]]]))
                  lines_to_add = c(lines_to_add,paste("   ","def",module_extended_arguments[[module_of_interest]][[configuration_parameters[k1]]],"=",parameter_value))
                }
              }
            }
            lines_to_add = c(lines_to_add,"}")
            
          } else{
            for(k1 in 1:length(configuration_parameters)){
              parameter_value = module_metadata[[module_name_for_configuration]][[configuration_scenarios[k]]][[configuration_parameters[k1]]]
              if(length(parameter_value) > 1){
                parameter_value = paste(parameter_value , collapse = " ")
              }
              if(!configuration_parameters[k1] %in% names(module_extended_arguments[[module_of_interest]])){
                lines_to_add = c(lines_to_add,paste(configuration_parameters[k1],"=",parameter_value))
              } else{
                rlog::log_info(paste("Replacing",configuration_parameters[k1],"with",module_extended_arguments[[module_of_interest]][[configuration_parameters[k1]]]))
                lines_to_add = c(lines_to_add,paste("   ","def",module_extended_arguments[[module_of_interest]][[configuration_parameters[k1]]],"=",parameter_value))
              }
            }
          }
        }
      }
    }
    rlog::log_info(paste("Adding lines:",paste(lines_to_add,collapse="\n")))
    rlog::log_info(paste("Line numbers to modify:",paste(line_numbers_of_interest[2:length(line_numbers_of_interest)],collapse = ", ")))
    line_number_key = line_numbers_of_interest[2:length(line_numbers_of_interest)]
    if(length(line_number_key) >1) {
      rlog::log_warn(paste("Not sure how to perform edits for:",module_of_interest,nf_script,"Skipping edits."))
    } else{
      line_edits[[toString(line_number_key)]] = lines_to_add
    }
  }
  #wprint(line_edits)
  #########################
  for(lidx in 1:length(new_lines)){
    if(toString(lidx + 1) %in% names(line_edits)){
      if(!is.null(line_edits[[toString(lidx + 1)]])){
        rlog::log_info(paste("ADDDING_NEW_LINES:",paste(new_lines[lidx],line_edits[[toString(lidx + 1)]],collapse="\n")))
        new_lines[lidx] = paste(new_lines[lidx],paste(line_edits[[toString(lidx + 1)]],collapse="\n"),collapse="\n",sep="\n")
        #new_lines[lidx] =  paste(line_edits[[toString(lidx + 1)]],collapse="\n")
      }
    }
  }
  updated_nf_script = gsub(".nf$",".final_edits.nf",nf_script)
  rlog::log_info(paste("Writing out final edits to :",updated_nf_script))
  write.table(x=new_lines,file=updated_nf_script,sep="\n",quote=F,row.names=F,col.names=F)
  system(paste("cp",updated_nf_script,nf_script))
}







