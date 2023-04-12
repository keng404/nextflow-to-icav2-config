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
###### workflow event checker -- see if events are malformed
malformed_workflow_event_processes <- function(parsed_process_list,script_name){
  malformed_workflow_event_processes_check = FALSE
  malformed_workflow_event_processes_double_check = c()
  if(length(parsed_process_list) > 0){
    for(i in 1:length(names(parsed_process_list))){
      process_name = names(parsed_process_list)[i]
      rlog::log_info(paste("Double-checking contents of ",process_name,"in","nf_workflow_events variable"))
      if(length(parsed_process_list[[names(parsed_process_list)[i]]]) > 0){
        process_lines = parsed_process_list[[names(parsed_process_list)[i]]][["process_lines"]]
        last_line = process_lines[length(process_lines)]
        if(last_line != "}"){
          rlog::log_warn(paste("POSSIBLE_MALFORMED_WORKFLOW_EVENT_PROCESS for:",process_name,"in",script_name))
          rlog::log_warn(paste("POSSIBLE_MALFORMED_WORKFLOW_EVENT_PROCESS for:",process_name,"\nFound",last_line,"expected","}"))
          #malformed_workflow_event_processes_check = TRUE
          if(!process_name %in% malformed_workflow_event_processes_double_check){
            malformed_workflow_event_processes_double_check = c(malformed_workflow_event_processes_double_check,process_name)
          }
        }
      } else{
        rlog::log_warn(paste("POSSIBLE_MALFORMED_WORKFLOW_EVENT_PROCESS for:",process_name,"in",script_name))
        stop(paste("EXITING: ",process_name,"is empty","in",script_name))
        malformed_workflow_event_processes_check = TRUE
      }
    }
    if(malformed_workflow_event_processes_check){
      stop("EXITING: Please check stderr for error messages.\nIt looks like the following processes weren't properly parsed:",paste(malformed_workflow_event_processes_double_check,collapse=", "),"\nCheck the parseProcessesInNextflowScript function\n")
    } else{
      rlog::log_info("All workflow events look ok!\n")
    }
  } else{
    rlog::log_info("No workflow events to validate!\n")
  }
}
