options(stringsAsFactors=FALSE)
library(argparse)
library(rlog)
library(stringr)
source('ica_configure/reading_utils.R')
source('ica_configure/writing_utilities.R')
source('ica_configure/functional_utilities.R')
# create parser object
parser <- ArgumentParser()
# specify our desired options 
# by default ArgumentParser will add an help option 
parser$add_argument("-c","--config-file","--config_file", default=NULL,required=TRUE,
                    help = "main config file")
parser$add_argument("-k","--is-simple-config","--is_simple_config",
                    action="store_true",default=FALSE, help = "Use config")
parser$add_argument("-f","--nf-core-mode","--nf_core_mode",action="store_true",
                    default=FALSE, help = "flag to indicate nf-core pipeline")
parser$add_argument("-i","--configs-to-ignore","--configs_to_ignore", default=c(""),nargs="?",
                    action="append",help="config files to ignore")
parser$add_argument("-x","--parameters-xml","--parameters_xml",
                    default=NULL, help = " parameters XML file output")
parser$add_argument("-g","--generate-parameters-xml","--generate_parameters_xml",
                    action="store_true",default=FALSE, help = "Generate parameters XML file")
parser$add_argument("-e","--error-stub","--error_stub", default="dummy_template.txt",
                    help = "error_stub to append to main.nf, workflow, and subworkflow files")
parser$add_argument("-s", "--nf-script","--nf_script", default=NULL,
                    help="Main NF script")
parser$add_argument("-w","--other-workflow-scripts","--other_workflow_scripts", default=c(""),nargs="?",
                    action="append",help="nextflow files to add error_stub")
parser$add_argument("-o","--input-files-override","--input_files_override", default = "input_override.non_dsl2_pipelines.txt",
                    help = "default stub to override input")
args <- parser$parse_args()
is_simple_config = args$is_simple_config
nf_script = args$nf_script
if(file.exists(args$input_files_override)){
  input_override = read.delim(args$input_files_override,header=F,quote="")
} else{
  input_override = c()
}
config_file = args$config_file
parameters_xml = args$parameters_xml
generate_parameters_xml = args$generate_parameters_xml
error_stub = args$error_stub
nf_script = args$nf_script
other_workflow_scripts = args$other_workflow_scripts
other_workflow_scripts = other_workflow_scripts[other_workflow_scripts != ""]
configs_to_ignore = args$configs_to_ignore
configs_to_ignore = configs_to_ignore[configs_to_ignore!=""]
###################################### add error stub
create_error_stub <- function(error_stub){
  new_lines = t(read.delim(error_stub,header=F,quote="",sep="\n"))
  return(c("workflow.onError{ ",new_lines,"}"))
}
add_error_stub <- function(nf_file,stub_statements){
  temp_file = paste(nf_file,".tmp",sep="")
  rlog::log_info(paste("READING NF file :",nf_file))
  nf_file_lines = t(read.delim(nf_file,header=F,quote="",sep="\n"))
  found_workflow_on_error_statement = FALSE
  workflow_on_error_bool = unlist(apply(t(nf_file_lines),2,function(x) grepl("workflow.onError",x)))
  if(sum(workflow_on_error_bool) == 0){
  updated_lines = c(nf_file_lines,stub_statements)
  rlog::log_info(paste("WRITING out updated NF file with error stub:",nf_file))
  write.table(x=updated_lines,file=temp_file,sep="\n",quote=F,row.names=F,col.names=F)
  system(paste("cp",temp_file,nf_file))
  } else{
    rlog::log_warn(paste("ADD stub statements manually",stub_statements,paste("IN_THE_FILE:",nf_file,sep=" "),sep="\n"))
  }
}
if(length(other_workflow_scripts) > 0 | !is.null(nf_script)){
  error_stub_to_add = create_error_stub(error_stub=error_stub)
  if(length(other_workflow_scripts)>0){
    for(i in 1:length(other_workflow_scripts)){
      add_error_stub(nf_file=other_workflow_scripts[i],stub_statements=error_stub_to_add)
    }
  }
  if(!is.null(nf_script)){
    add_error_stub(nf_file=nf_script,stub_statements=error_stub_to_add) 
  }
}
################################### Making edits to XML file
if(generate_parameters_xml){
config_dat  = read.delim(config_file,quote="",header=F)
  
z = get_params_from_config(conf_file=config_file)

paramsFiller <- function(list_to_fill,params_list){
  params_list_updated = list_to_fill
  for(i in 1:length(list_to_fill)){
    result = str_extract(list_to_fill[i], "(?<=\\{)[^\\}]+")
    # maybe try a while loop?
    if(!is.na(result)){
      # fill in appropriate params vaiue
      updated_value = gsub(result,gsub("'","",params_list[[result]]),list_to_fill[i])
      # remove ${ and } from original string
      updated_value = gsub("\\$\\{","",updated_value)
      updated_value = gsub("\\}","",updated_value)
      updated_value = gsub("\'","",updated_value)
      updated_value = gsub("\"","",updated_value)
      params_list_updated[i] = updated_value
    }
  }
  return(params_list_updated)
}

findOtherConfigs <- function(conf_path,conf_data,defaultConfigs = NULL){
  ## identify lines in config that refer to additional config files
  lines_to_keep = c()
  # check if line contains if/else expression ()
  for(i in 1:nrow(conf_data)){
    if(grepl("includeConfig",conf_data[i,]) && !(grepl("test",conf_data[i,],ignore.case = T))){
      ### check if config is in if loop, we'll need to look at this expresssion to evaluate
      #### which configs to use
      lines_to_keep = c(lines_to_keep,conf_data[i,])
    }
  }
  configs = c()
  if(length(lines_to_keep) > 0){
    for(j in 1:length(lines_to_keep)){
      if(lines_to_keep[j] != ""){
        lines_to_keep[j] = gsub("\\{"," ",lines_to_keep[j])
        lines_to_keep[j] = gsub("\\}"," ",lines_to_keep[j])
        line_parsed = strsplit(lines_to_keep[j],"\\s+")[[1]]
        line_parsed = line_parsed[line_parsed != ""]
        line_parsed = line_parsed[!grepl("includeConfig",line_parsed)]
        line_parsed = line_parsed[length(line_parsed)]
        if(!(grepl("\\{",line_parsed))){
          line_parsed = paste(dirname(conf_path),line_parsed,sep="/")
          if(!grepl("params",line_parsed)){
            configs = c(configs,gsub("\'","",line_parsed))
          }
        } else{
          if(!grepl("params",line_parsed)){
            configs = c(configs,gsub("\'","",line_parsed))
          }
        }
      }
    }
  }
  return(configs)
}

localConfigOrNot <- function(config_list){
  keep_array = c()
  for(i in 1:length(config_list)){
    keep_array[i] = !(grepl("http",config_list[i])) && file.exists(config_list[i])
  }
  return(config_list[keep_array])
}

rlog::log_warn(paste("Number of closures in config",config_file,sum(grepl("\\{",t(config_dat)))))
if( sum(grepl("\\{",t(config_dat))) > 0 && !is_simple_config){
  z1 = findOtherConfigs(conf_path=config_file,conf_data=config_dat)
} else{
  z1 = NULL 
}
final_config_list = c()
if(length(configs_to_ignore) > 0 && !is.null(z1)){
  z1 = z1[ !(z1 %in% configs_to_ignore)]
}
### second pass configs to ignore
configs_to_ignore = z1[ apply(t(z1),2,function(x) sum(strsplit(x,'/')[[1]] == "") > 1) ]
rlog::log_info(paste("CONFIGS_TO_IGNORE:",configs_to_ignore))
if(length(configs_to_ignore) > 0 && !is.null(z1)){
  z1 = z1[ !(z1 %in% configs_to_ignore)]
}
rlog::log_info(paste("CONFIGS_FOUND:",z1))
final_config_list = c()
if(length(z1) > 0){
  final_config_list = localConfigOrNot(paramsFiller(list_to_fill=z1,params_list=z))
} else{
  final_config_list = NULL
}
#############################
#### for DSL2 workflows ... find module config file and load in the configruation metadata.
#### For now we hard-code 'modules.config' by nf-core convention .... may expose this as command-line parameter
############################################
modules_config_file = NULL
modules_config = list()
is_module_config_file = NULL
if(!is.null(z1)){
  is_module_config_file = apply(t(z1),2,function(elem) basename(elem) == "modules.config")
}
# STEP1: Grab nextflow config and propagate to all configs to grab the appropriate params
paramCollection = list()
if(length(final_config_list) > 0 ){
  for(config_idx in 1:length(final_config_list)){
    key_name = strsplit(basename(final_config_list[config_idx]),"\\.")[[1]][1]
    current_file = final_config_list[config_idx]
    rlog::log_info(paste("Reading in",current_file))
    parsedParams  = get_params_from_config(conf_file=current_file)
    if(length(parsedParams) > 0) {
      paramCollection[[key_name]] = parsedParams
    }
  }
}
compareConfigs <- function(defaultConfig, otherConfigs){
  updatedConfig = defaultConfig
  configs_to_check  = names(otherConfigs)
  default_config_params = names(defaultConfig)
  params_added = 0
  for(i in 1:length(configs_to_check)){
    optional_config_params = names(otherConfigs[[configs_to_check[i]]])
    params_to_add = optional_config_params[!(optional_config_params %in% default_config_params)]
    if(length(params_to_add) > 0){
      rlog::log_info(paste("Adding params",paste(params_to_add,collapse = ", ",sep=", ")))
      params_added = params_added + length(params_to_add)
      for(j in 1:length(params_to_add)){
        updatedConfig[[params_to_add[j]]] = otherConfigs[[configs_to_check[i]]][[params_to_add[j]]]
      }
    }
  }
  rlog::log_info(paste("Added",params_added,"params"))
  return(updatedConfig)
}
# compare param_collection with z
if(length(paramCollection) > 0){
  y = compareConfigs(defaultConfig = z,otherConfigs = paramCollection)
} else{
  y = z
}


#####STEP2: Parse main.nf script to add params that are needed to run pipeline
### for DSL=2 NF scripts ... will need to look through other scripts
# check that param does not exist in main.nf
parse_nf_script <- function(nf_script){
  nf_lines = read.delim(nf_script,quote="",header=F)
  lines_of_interest = c()
  line_numbers = c()
  in_process_enclosure = FALSE
  out_process_enclosure = TRUE
  for(i in 1:nrow(nf_lines)){
    if(!in_process_enclosure){
      if(grepl("process",nf_lines[i,])){
        in_process_enclosure = TRUE
        out_process_enclosure = FALSE
      }
      if(grepl("params\\.",nf_lines[i,]) && !grepl("if",nf_lines[i,])  && !grepl("else",nf_lines[i,]) && !grepl("def",nf_lines[i,]) && !grepl("\\{params\\.",nf_lines[i,]) && !grepl("\\(params\\.",nf_lines[i,])){
        if(!grepl("task",nf_lines[i,])){
          rlog::log_info(paste("ADDING_NF_SCRIPT_LINE:",nf_lines[i,]))
          lines_of_interest = c(lines_of_interest,nf_lines[i,])
          line_numbers = c(line_numbers,i)
        }
      }
    } else{
      if(grepl("\\}",nf_lines[i,]) && !grepl("\\$",nf_lines[i,]) && !grepl("\\!",nf_lines[i,])){
        in_process_enclosure = FALSE
        out_process_enclosure = TRUE
      }
    }
  }
  param_list = c()
  param_metadata = list()
  if(length(lines_of_interest) >0){
    param_list = c()
    param_metadata = list()
    for(j in 1:length(lines_of_interest)){
      tokenize_line = strsplit(lines_of_interest[j],"\\s+")[[1]]
      tokenize_line = strsplit(tokenize_line[1],"\\=")[[1]]
      param_value = trimws(tokenize_line[length(tokenize_line)])
      for(k in 1:1){
        # for(k in 1:length(tokenize_line)){
        sanitized_token = trimws(tokenize_line[k])
        if(grepl("^params\\.",sanitized_token)){
          split_check = strsplit(sanitized_token,"\\.")[[1]]
          param_names = c()
          ### remove additional chains done on the params to get the original param name
          for(l in 1:length(split_check)){
            if(!grepl("\\{",split_check[l]) && !grepl("\\(",split_check[l]) && !grepl("\\)",split_check[l])){
              punctuation_to_remove = str_extract(split_check[l],"(?![[\\]\\[\\_]])[[:punct:]]")
              punctuation_to_remove = punctuation_to_remove[!is.na(punctuation_to_remove)]
              punctuation_to_remove = punctuation_to_remove[punctuation_to_remove != "\""]
              punctuation_to_remove = punctuation_to_remove[punctuation_to_remove != "'"]
              if(length(punctuation_to_remove) >0){
                for(pidx in 1:length(punctuation_to_remove)){
                  rlog::log_info(paste("REMOVING_PUNCTUATION:",punctuation_to_remove[pidx], "from",split_check[l]))
                  split_check[l] = gsub(punctuation_to_remove[pidx],"",split_check[l])
                }
              }
              param_names = c(param_names,split_check[l])
            }
          }
          double_sanitized_token = paste(param_names,collapse=".")
          param_list = c(param_list,double_sanitized_token)
          param_metadata[[double_sanitized_token]] = param_value
        }
      }
    }
  } else{
    rlog::log_warn(paste("COULD not find param lines for",nf_script))
  }
  results = list()
  results[["lines_kept"]] = lines_of_interest
  results[["line_numbers"]] = line_numbers
  results[["params_found"]] = unique(param_list)
  results[["params_metadata"]] = param_metadata
  return(results)
}
#nf_script = "/Users/keng/nf-core/sarek/main.nf"
rlog::log_info(paste("READING in NF script:",nf_script))
nf_script_dat = read.delim(nf_script,quote="",header=F)
foi_result = parse_nf_script(nf_script =nf_script)
print(foi_result)
all_nf_edits = list()
##### STEP0: params then proess until we reach end of NF script
#### STEP1: compare foi_result[["params_found"]] with y ??? 
#### determines what params to add to our main NF script
##### STEP2:
##### scan all params to add and if it's a nested parameter --- parameter list, be sure
##### to initialize the parameter list. For Example, params.genome['myGenome'].bwa would 
##### need  params.genome =  [:]  and params.genome['myGenome'] = [:] before specifying the param
##### params.genome['myGenome'].bwa

### for each element in y determine if the parameter depends on an upstream parameter
params_to_check = names(y)
params_to_add_to_nf_script = params_to_check[!(params_to_check %in% foi_result[["params_found"]])]

rlog::log_info(paste("Parameters to check:",paste(params_to_check,sep=", ")))
rlog::log_info(paste("Parameters to add:",paste(params_to_add_to_nf_script,sep=", ")))
#### try to figure out relative order that we should see these params in the main.nf script
#### known exception, this won't work well if your parameter references multiple parameters
#### 
param_order = list()
if(length(params_to_check)>0){
  for( i in 1:length(params_to_check)){
    param_key_order = c()
    if(grepl("\\$\\{",y[[params_to_check[i]]])){
      rlog::log_info(paste("FOUND param to check:",params_to_check[i]))
      param_key_order = c(param_key_order,params_to_check[i])
      result = str_extract(y[[params_to_check[i]]], "(?<=\\{)[^\\}]+")
      param_key_order = c(param_key_order,result)
      if(!is.na(result)){
        param_check = TRUE
        if(!result %in% names(y)){
          rlog::log_warn(paste("CANNOT FIND value for parameter",result))
          rlog::log_warn(paste("WILL NOT check expression",y[[params_to_check[i]]]))
          param_check = FALSE
        }
        while(param_check){
          if(!result %in% names(y)){
            rlog::log_warn(paste("CANNOT FIND value for parameter",result))
            rlog::log_warn(paste("WILL NOT check expression",y[[params_to_check[i]]]))
            param_check = FALSE
          } else{
            if(!grepl("\\$\\{",y[[result]])){
              param_check = FALSE
            } else{
              result = str_extract(y[[result]], "(?<=\\{)[^\\}]+")
              if(!result %in% param_key_order){
                param_key_order = c(param_key_order,result)
              }
            }
          }
        }
        rlog::log_info(paste(rev(param_key_order),collapse=" => "))
        param_order[[params_to_check[i]]] = rev(param_key_order)
      } else{
        rlog::log_info(paste("Are you sure about",params_to_check[i],"param to check:"))
      }
    } else{
      rlog::log_info(paste("NO need to check param:",params_to_check[i]))
      param_order[[params_to_check[i]]] = NULL
    }
  }
} else{
  rlog::log_info(paste("SETTING y to foi_result[['params_found']]"))
  ### if there are no  configuration parameters to add, set y to  foi_result[["params_found"]]).
  ### This defaults to all params found in main script
  y =  foi_result[["params_metadata"]]
  print(y)
}
#####
##### STEP0: params then process until we reach end of NF script
# create appropriate data structures in main.nf (i.e. closures in configs -> lists in main.nf)
if(!is.null(foi_result[["line_numbers"]])){
  parameter_line_blocks  = split(foi_result[["line_numbers"]], cumsum( c(0, diff(foi_result[["line_numbers"]])>1) ) )
} else{
  parameter_line_blocks = c()
}
parameter_edits = list()
if(length(params_to_add_to_nf_script) > 0){
  for( i in 1:length(params_to_add_to_nf_script)){
    if(params_to_add_to_nf_script[i] %in% names(param_order)){
      ### check to see if NF param is referenced in  script already,if so param needs to be defined after
      if(params_to_add_to_nf_script[i] %in% names(foi_result[["params_found"]])){
        line_number_of_interest = foi_result[["line_numbers"]][names(foi_result[["params_found"]]) == params_to_add_to_nf_script[i]]
        for(j in 1:length(names(parameter_line_blocks))){
          if(line_number_of_interest %in% parameter_line_blocks[[names(parameter_line_blocks[j])]]){
            parameter_edits[[params_to_add_to_nf_script[i]]] =  max(parameter_line_blocks[[names(parameter_line_blocks[j])]])
          }
        }
      } else{
        ### if not, find a parameter_line_block to start placing the params
        parameter_edits[[params_to_add_to_nf_script[i]]] = max(parameter_line_blocks[[1]])
      }
    } else{
      ### add anywhere in NF script
      parameter_edits[[params_to_add_to_nf_script[i]]] = max(parameter_line_blocks[[1]])
    }
  }
}
##############
### fix bug parameter_edits and y is not equivalent
##################
exceptions_list = c('null','true','false')
complex_params_added = c()
if(length(parameter_edits) > 0 ){
  for(i in 1:length(names(parameter_edits))){
    line_num = parameter_edits[[names(parameter_edits)[i]]]
    new_name = names(parameter_edits)[i]
    lines_to_add = c()
    if(grepl("\\[",new_name)){
      rlog::log_info(paste("CHECKING_TO_SEE if upstream param needs to be initialized for",new_name))
      new_name = new_name
      new_name_tokens = strsplit( new_name , "\\.")[[1]]
      new_name_tokens_v2 = c()
      tokens_to_connect = c()
      connect_elements = FALSE
      for( t in 1:length(new_name_tokens)){
        if(grepl("\\[",new_name_tokens[t])){
          tokens_to_connect = c(tokens_to_connect,new_name_tokens[t])
          connect_elements = TRUE
        } else if(grepl("\\]",new_name_tokens[t])){
          tokens_to_connect = c(tokens_to_connect,new_name_tokens[t])
          new_name_tokens_v2 = c(new_name_tokens_v2,paste(tokens_to_connect,collapse="."))
          tokens_to_connect = c()
          connect_elements = FALSE
        } else if(connect_elements == TRUE){
          tokens_to_connect = c(tokens_to_connect,new_name_tokens[t])
        } else{
          new_name_tokens_v2 = c(new_name_tokens_v2,new_name_tokens[t])
        }
      }
      rlog::log_info(paste("TOKENS:",paste(new_name_tokens_v2,collapse="---")))
      new_name_tokens = new_name_tokens_v2
      for(n in 1:length(new_name_tokens)){
        #test_split = strsplit(new_name_tokens[n],"[[:punct:]]")[[1]]
        test_split = strsplit(new_name_tokens[n],"\\[|]")[[1]]
        test_split = test_split[test_split!="" && !is.na(test_split)]
        new_key = str_extract_all(new_name,"(?<=\\[)[^\\]]+")[[1]][1]
        rlog::log_info(paste("MY_SPLIT:",paste(test_split,collapse="---")))
        if(length(test_split) > 1){
          rlog::log_info(paste("INVESTIGATING_PARAM:",new_name))
          if(length(str_extract_all(test_split[1],"'")[[1]]) %% 2 == 0 ){
            if(!(paste("params.",test_split[1],sep="") %in% complex_params_added)){
              line_to_add  = paste(paste("params.",test_split[1],sep=""),"=","[:]")
              rlog::log_info(paste("Initializing complex param:",paste("params.",test_split[1],sep="")))
              lines_to_add = c(lines_to_add,line_to_add)
              complex_params_added = c(complex_params_added,paste("params.",test_split[1],sep=""))
            } 
            if(!(paste("params.",test_split[1],"[",new_key,"]",sep="") %in% complex_params_added)){
              line_to_add  = paste(paste("params.",test_split[1],"[",new_key,"]",sep=""),"=","[:]")
              lines_to_add = c(lines_to_add,line_to_add)
              rlog::log_info(paste("Initializing complex param:",paste("params.",test_split[1],"[",new_key,"]",sep="")))
              complex_params_added = c(complex_params_added,paste("params.",test_split[1],"[",new_key,"]",sep=""))
            }
          }
        } else{
          redo_tokens = strsplit( new_name , "\\.")[[1]]
          tokens_to_check = redo_tokens[grepl("\\[",redo_tokens)]
          new_key = str_extract_all(new_name,"(?<=\\[)[^\\]]+")[[1]][1]
          for(n in 1:length(tokens_to_check)){
            test_split = strsplit(tokens_to_check[n],"\\[|]")[[1]]
            rlog::log_info(paste("INVESTIGATING_PARAM:",new_name))
            if(length(str_extract_all(test_split[1],"'")[[1]]) %% 2 == 0 ){
              if(!(paste("params.",test_split[1],sep="") %in% complex_params_added)){
                line_to_add  = paste(paste("params.",test_split[1],sep=""),"=","[:]")
                rlog::log_info(paste("Initializing complex param:",paste("params.",test_split[1],sep="")))
                lines_to_add = c(lines_to_add,line_to_add)
                complex_params_added = c(complex_params_added,paste("params.",test_split[1],sep=""))
              }
              if(!(paste("params.",test_split[1],"[",new_key,"]",sep="") %in% complex_params_added)){
                line_to_add  = paste(paste("params.",test_split[1],"[",new_key,"]",sep=""),"=","[:]")
                lines_to_add = c(lines_to_add,line_to_add)
                rlog::log_info(paste("Initializing complex param:",paste("params.",test_split[1],"[",new_key,"]",sep="")))
                complex_params_added = c(complex_params_added,paste("params.",test_split[1],"[",new_key,"]",sep=""))
              }
            }
          }
        }
        
      }
      ### break down param name and check that the param is properly initialized
    } else{
      print("hello")
      #if(!args$dsl2_enabled){
      #  new_name = gsub("params\\.","",new_name)
      #}
    }
    expressions_to_add = c()
    if(grepl("false",y[[names(parameter_edits)[i]]])){
      y[[names(parameter_edits)[i]]] = gsub("\"","",y[[names(parameter_edits)[i]]])
    }
    logical_values = y[[names(parameter_edits)[i]]] != "null" && y[[names(parameter_edits)[i]]] != "true" && y[[names(parameter_edits)[i]]] != "false"
    other_conditions = grepl("/",y[[names(parameter_edits)[i]]])  
    must_have_condition = !grepl("'",y[[names(parameter_edits)[i]]])  
    rlog::log_info(paste("param_name:",names(parameter_edits)[i],"logical_values:",logical_values,"other_conditions:",other_conditions,"must_have_condition:",must_have_condition))
    if(is.na(logical_values)){
      next
    }
    dangling_single_quote  =str_extract_all(y[[names(parameter_edits)[i]]],"\"")[[1]]
    dangling_double_quote  =str_extract_all(y[[names(parameter_edits)[i]]],"'")[[1]]
    if(must_have_condition && (logical_values || other_conditions)){
      if(!grepl("/",y[[names(parameter_edits)[i]]])){
        if((length(dangling_single_quote) == 2  && length(dangling_double_quote) == 0)|| (length(dangling_single_quote) == 0 && length(dangling_double_quote) ==2)){
          expression_to_add = paste(new_name,"=",y[[names(parameter_edits)[i]]])
        } else if(length(dangling_single_quote) > 0 && length(dangling_single_quote) < 2){
          rlog::log_warn(paste("Working with",y[[names(parameter_edits)[i]]]))
          y[[names(parameter_edits)[i]]] = gsub("\"","",y[[names(parameter_edits)[i]]])
          expression_to_add = paste(new_name,"=",paste(y[[names(parameter_edits)[i]]],"'",sep=""))
        } else if(length(dangling_double_quote) > 0 && length(dangling_double_quote) < 2){
          y[[names(parameter_edits)[i]]] = gsub("'","",y[[names(parameter_edits)[i]]])
          expression_to_add = paste(new_name,"=",paste(y[[names(parameter_edits)[i]]],"\"",sep=""))
        } else if(y[[names(parameter_edits)[i]]]  %in% exceptions_list){
          expression_to_add = paste(new_name,"=",y[[names(parameter_edits)[i]]])
        } else if(!grepl("/",y[[names(parameter_edits)[i]]])){
          if(is.numeric(y[[names(parameter_edits)[i]]])){
            expression_to_add = paste(new_name,"=",y[[names(parameter_edits)[i]]])
          } else{
            expression_to_add = paste(new_name,"=",paste("\"",y[[names(parameter_edits)[i]]],"\"",sep=""))
          }
        } else if(!grepl("\"",y[[names(parameter_edits)[i]]]) ){
          expression_to_add = paste(new_name,"=",paste("\"",y[[names(parameter_edits)[i]]],"\"",sep=""))
        } else{
          expression_to_add = paste(new_name,"=",y[[names(parameter_edits)[i]]])
        }
      } else if((length(dangling_single_quote) == 2  && length(dangling_double_quote) == 0)|| (length(dangling_single_quote) == 0 && length(dangling_double_quote) ==2)){
        expression_to_add = paste(new_name,"=",y[[names(parameter_edits)[i]]])
      } else if(length(dangling_single_quote) > 0 && length(dangling_single_quote) < 2){
        rlog::log_warn(paste("Working with",y[[names(parameter_edits)[i]]]))
        y[[names(parameter_edits)[i]]] = gsub("\"","",y[[names(parameter_edits)[i]]])
        expression_to_add = paste(new_name,"=",paste(y[[names(parameter_edits)[i]]],"'",sep=""))
      } else if(length(dangling_double_quote) > 0 && length(dangling_double_quote) < 2){
        y[[names(parameter_edits)[i]]] = gsub("'","",y[[names(parameter_edits)[i]]])
        expression_to_add = paste(new_name,"=",paste(y[[names(parameter_edits)[i]]],"\"",sep=""))
      } else if(y[[names(parameter_edits)[i]]]  %in% exceptions_list){
        expression_to_add = paste(new_name,"=",y[[names(parameter_edits)[i]]])
      } else if(!grepl("/",y[[names(parameter_edits)[i]]])){
        if(is.numeric(y[[names(parameter_edits)[i]]])){
          expression_to_add = paste(new_name,"=",y[[names(parameter_edits)[i]]])
        } else{
          expression_to_add = paste(new_name,"=",paste("\"",y[[names(parameter_edits)[i]]],"\"",sep=""))
        }      
      } else if(!grepl("\"",y[[names(parameter_edits)[i]]]) ){
        expression_to_add = paste(new_name,"=",paste("\"",y[[names(parameter_edits)[i]]],"\"",sep=""))
      } else{
        if(is.numeric(y[[names(parameter_edits)[i]]])){
          expression_to_add = paste(new_name,"=",y[[names(parameter_edits)[i]]])
        } else{
          expression_to_add = paste(new_name,"=",paste("\"",y[[names(parameter_edits)[i]]],"\"",sep=""))
        }
      }
    } else{
      if((length(dangling_single_quote) == 2  && length(dangling_double_quote) == 0)|| (length(dangling_single_quote) == 0 && length(dangling_double_quote) ==2)){
        expression_to_add = paste(new_name,"=",y[[names(parameter_edits)[i]]])
      } else if(length(dangling_single_quote) > 0 && length(dangling_single_quote) < 2){
        rlog::log_warn(paste("Working with",y[[names(parameter_edits)[i]]]))
        y[[names(parameter_edits)[i]]] = gsub("\"","",y[[names(parameter_edits)[i]]])
        expression_to_add = paste(new_name,"=",paste(y[[names(parameter_edits)[i]]],"'",sep=""))
      } else if(length(dangling_double_quote) > 0 && length(dangling_double_quote) < 2){
        y[[names(parameter_edits)[i]]] = gsub("'","",y[[names(parameter_edits)[i]]])
        expression_to_add = paste(new_name,"=",paste(y[[names(parameter_edits)[i]]],"\"",sep=""))
      } else if(y[[names(parameter_edits)[i]]]  %in% exceptions_list){
        expression_to_add = paste(new_name,"=",y[[names(parameter_edits)[i]]])
      } else if(!grepl("/",y[[names(parameter_edits)[i]]])){
        if(grepl("^[[:digit:]]+",y[[names(parameter_edits)[i]]])){
          expression_to_add = paste(new_name,"=",y[[names(parameter_edits)[i]]])
        } else{
          expression_to_add = paste(new_name,"=",paste("\"",y[[names(parameter_edits)[i]]],"\"",sep=""))
        }   
      } else if(!grepl("\"",y[[names(parameter_edits)[i]]])){
        expression_to_add = paste(new_name,"=",paste("\"",y[[names(parameter_edits)[i]]],"\"",sep=""))
      } else{
        if(is.numeric(y[[names(parameter_edits)[i]]])){
          expression_to_add = paste(new_name,"=",y[[names(parameter_edits)[i]]])
        } else{
          expression_to_add = paste(new_name,"=",paste("\"",y[[names(parameter_edits)[i]]],"\"",sep=""))
        }
      }
    }
    if(!(paste(line_num) %in% names(all_nf_edits))){
      if(length(lines_to_add) > 0) {
        expressions_to_add = c(lines_to_add,expression_to_add)
      } else{
        expressions_to_add = c(expression_to_add)
      }
      rlog::log_info(paste("Initializing key:",paste(line_num)))
      all_nf_edits[[paste(line_num)]] = c()
      rlog::log_info(paste("Appending to key:",paste(line_num),"parameter:",new_name))
      all_nf_edits[[paste(line_num)]] = c(all_nf_edits[[paste(line_num)]],expressions_to_add)
    } else{
      rlog::log_info(paste("Appending to key:",paste(line_num),"parameter:",new_name))
      if(length(lines_to_add) > 0) {
        expressions_to_add = c(lines_to_add,expression_to_add)
      } else{
        expressions_to_add = c(expression_to_add)
      }
      all_nf_edits[[paste(line_num)]] = c(all_nf_edits[[paste(line_num)]],expressions_to_add)
    }
  }
}
#########################
### params that are Groovy map
get_keys_from_complex_params <- function(complex_param_list){
  complex_params = list()
  if(length(complex_param_list) > 0){
    for( i in 1:length(complex_param_list)){
      original_param = strsplit(complex_param_list,"\\.")[[1]][1]
      key_value = str_extract_all(complex_param_list[i],"(?<=\\[)[^\\]]+")[[1]][1]
      if(original_param %in% complex_params){
        complex_params[[original_param]] = c(complex_params[[original_param]],key_value)
      } else{
        complex_params[[original_param]] = c()
        complex_params[[original_param]] = c(complex_params[[original_param]],key_value)
      }
    }
  }
  if(length(names(complex_params))>0){
    for(n in 1:length(names(complex_params))){
      complex_params[[names(complex_params)[n]]] = unique(complex_params[[names(complex_params)[n]]] )
    }
  }
  return(complex_params)
}
options_to_add = get_keys_from_complex_params(complex_param_list = complex_params_added)
########################
#### update nf script with parameter updates 
updated_nf_file = gsub(".nf$",".ica.nf",nf_script)
new_nf_lines = c(input_override)
if(length(all_nf_edits) > 0){
  for(i in 1:nrow(nf_script_dat)){
    if(toString(i) %in% names(all_nf_edits)){
      new_nf_lines = c(new_nf_lines,nf_script_dat[i,])
      edits_to_add = all_nf_edits[[toString(i)]]
      for(j in 1:length(edits_to_add)){
        new_nf_lines = c(new_nf_lines,edits_to_add[j])
      }
    } else{
      new_nf_lines = c(new_nf_lines,nf_script_dat[i,])
    }
  }
} else{
  for(i in 1:nrow(nf_script_dat)){
    new_nf_lines = c(new_nf_lines,nf_script_dat[i,])
  }
}
if(length(new_nf_lines) > 0) {
  rlog::log_info(paste("ADDING UPDATED PARAMS to",updated_nf_file))
  write.table(x=new_nf_lines,file=updated_nf_file,sep="\n",quote=F,row.names=F,col.names=F)
} else{
  updated_nf_file = nf_script
}
####################
createDummyValue <- function(param_value,param_type){
  param_value1 = param_value
  if(is.na(param_value) || is.null(param_value) || param_value == ""){
    if(param_type == "boolean"){
      param_value1 = "false"
    } else if(param_type == "integer"){
      param_value1 = 0
    } else{
      param_value1 = "null"
    }
  }
  return(param_value1)
}
##########################################
classifyParameters <- function(paramsToXML){
  xmlSections = list()
  xmlSections[["dataInputs"]] = list()
  xmlSections[["parameterSettings"]] = list()
  xmlSections[["parameterSettings"]][["general"]] = list()
  for(i in 1:length(names(paramsToXML))){
    param_name = names(paramsToXML)[i]
    rlog::log_info(paste("LOOKING into",param_name))
    param_value = paramsToXML[[param_name]]
    param_final_name = gsub("params\\.","",param_name)
    if(grepl("\\$\\{params\\.",param_value)){
      rlog::log_info(paste("LOOKING up",param_name,"PARAM_VALUE:",param_value))
      param_value = paramsFiller(list_to_fill=c(param_value),paramsToXML)[1]
      #rlog::log_info(paste("VALUE is",param_value))
    }
    parameter_type = 'string'
    description = paste(parameter_type,"that defines",param_name)
    parameter_metadata = list()
    filename = basename(param_value)
    aram_value = gsub("'", "", param_value)
    # initial algo will determine if parameter is string, int, boolean, or path
    if(grepl("/",param_value) || grepl("input",param_name)){
      split_param_value = strsplit(param_value,"/")[[1]]
      rlog::log_info(paste("CHECKING if",param_name,"is file or folder. value:",param_value))
      if((length(split_param_value) > 2 || grepl("input",param_name)) && param_value != "/" && !grepl("https://",param_value)  && !grepl("s3://",param_value) && !grepl("gs://",param_value) && !grepl("core.windows.net",param_value)){
        parameter_type = 'data'
        # if param value contains s3://, *.core.windows.net, or gs://, ignore, we won't put this in the XML
        # if path and contains file extension ('.') --- it's a file, if not it's a dir.
        #  or if the param contains 'dir' 
        data_type = "FILE"
        if(grepl("\\.",filename)){
          data_type = "FILE" 
        } 
        if(grepl("dir",param_name,ignore.case = TRUE)){
          data_type = "FOLDER"
        }
        if(!grepl("\\*",param_value) && !grepl("\\{",param_value) && !(grepl("\\[",param_name)) && !(grepl("\\$\\{",paramsToXML[[param_name]]))){
          rlog::log_info(paste("ADDING",param_name,"to dataInputs"))
          description = paste("Path",parameter_type,"that defines",data_type,param_name)
          parameter_metadata[["description"]] = description
          xmlSections[["dataInputs"]][[param_final_name]] = parameter_metadata
        } else{
          rlog::log_info(paste("ADDING",param_name,"to parameterSettings"))
          description = paste(parameter_type,"that defines",data_type,param_name)
          parameter_metadata[["description"]] = description
          parameter_metadata[["type"]] = parameter_type
          if(param_name != "params.help"){
            xmlSections[["parameterSettings"]][["general"]][[param_final_name]] = parameter_metadata
          }
        }
      }
      # all others will be put in options
      
    } else{
      if(!grepl("\\$\\{",param_value) && !(grepl("\\[",param_name))){
        if(!is.na(strtoi(param_value))){
          parameter_type = "integer"
          param_value = strtoi(param_value)
        }
        if(grepl("true",param_value,ignore.case=T) || grepl("false",param_value,ignore.case=T)){
          parameter_type = "boolean"
        }
        param_value = gsub("'","",param_value)
        parameter_metadata[["description"]] = description
        parameter_metadata[["type"]] = parameter_type
        parameter_metadata[["default"]] = param_value
        if(param_name != "params.help"){
          xmlSections[["parameterSettings"]][["general"]][[param_final_name]] = parameter_metadata
        }
      }
    }
    if(parameter_type == "string"){
      if(!grepl("\\$\\{",param_value) && !(grepl("\\[",param_name))){
        if(!is.na(strtoi(param_value))){
          parameter_type = "integer"
          param_value = strtoi(param_value)
        }
        if(grepl("true",param_value,ignore.case=T) || grepl("false",param_value,ignore.case=T)){
          parameter_type = "boolean"
        }
        param_value = createDummyValue(param_value=param_value,param_type=parameter_type)
        parameter_metadata[["description"]] = description
        parameter_metadata[["type"]] = parameter_type
        parameter_metadata[["default"]] = param_value
        if(param_name != "params.help"){
          xmlSections[["parameterSettings"]][["general"]][[param_final_name]] = parameter_metadata
        }
      }
    }
  }
  return(xmlSections)
}


  library(XML)
  rlog::log_info(paste("parameters found:",names(y)))
  getXMLSections = classifyParameters(paramsToXML = y)
  data_input_configurations = getXMLSections[["dataInputs"]]
  if(args$nf_core_mode){
    #if(!"input" %in% names(data_input_configurations) || length(names(data_input_configurations)) == 0){
    ########### workaround add input files --- will not be used by pipeline , but by ICA to stage the data
    data_input_configurations[["input_files"]] = list()
    data_input_configurations[["input_files"]][["description"]] = 'input files for pipeline.\nAll files will be staged in workflow.launchDir'
    #}
  }
  #####################
  step_configurations = getXMLSections[["parameterSettings"]]
  rlog::log_info(paste("STEP3:Generating ICA XML based off of",nf_script))
  doc = newXMLDoc()
  #root = xmlRoot(doc)                                      # FIND ROOT
  #pipeline_node = newXMLNode("pipeline",parent=root)
  root = newXMLNode("pipeline",doc=doc)
  xmlAttrs(root) = c(code=paste(basename(dirname(nf_script)),"pipeline"),version="1.0",xmlns="xsd://www.illumina.com/ica/cp/pipelinedefinition")
  # add data inputs
  # create new sections for steps
  ## then add options for each section
  ############
  #code="left" format="FASTQ" type="FILE" required="true" multiValue="false"
  #<pd:dataInput code="ref" format="FASTA" type="FILE" required="true" multiValue="false">
  #  <pd:label>ref</pd:label>
  #  <pd:description>the value for File transcriptome</pd:description>
  #  </pd:dataInput>
  ############
  #############################
  #### add data inputs
  rlog::log_info(paste("STEP3a: Adding dataInputs"))
  dataInputsNode = newXMLNode("dataInputs",parent=root)
  if(length(data_input_configurations) >0){
    for(i in 1:length(names(data_input_configurations))){
      dataInputNode = newXMLNode("dataInput",parent=dataInputsNode)
      if(grepl("folder",data_input_configurations[[names(data_input_configurations)[i]]][["description"]],ignore.case=T)){
        xmlAttrs(dataInputNode) = c(code = names(data_input_configurations)[i] ,format = "UNKNOWN",type = "DIRECTORY",required = "true",multiValue = "true")   
      } else{
        if(names(data_input_configurations)[i]  == "input_files" && data_input_configurations[[names(data_input_configurations)[i]]][["description"]] == 'input files for pipeline.\nAll files will be staged in workflow.launchDir'){
          xmlAttrs(dataInputNode) = c(code = names(data_input_configurations)[i] ,format = "UNKNOWN",type = "FILE",required = "false",multiValue = "true")   
        } else{
          xmlAttrs(dataInputNode) = c(code = names(data_input_configurations)[i] ,format = "UNKNOWN",type = "FILE",required = "true",multiValue = "true")   
        }
      }
      newXMLNode("label", names(data_input_configurations)[i], parent=dataInputNode)
      newXMLNode("description", data_input_configurations[[names(data_input_configurations)[i]]][["description"]], parent=dataInputNode)
    }
  } else{
    rlog::log_warn(paste("STEP3a: No dataInputs found"))
  }
  ############## add parameter options
  ## <pd:step execution="MANDATORY" code="General">
  #<pd:label>General</pd:label>
  #  <pd:description>General parameters</pd:description>
  #  <pd:tool code="generalparameters">
  #  <pd:label>generalparameters</pd:label>
  #  <pd:description>General Parameters</pd:description>
  
  #code="threads" minValues="1" maxValues="1" classification="USER"
  #
  #                <pd:parameter code="threads" minValues="1" maxValues="1" classification="USER">
  #                    <pd:label>threads</pd:label>
  #                    <pd:description>the value for threads</pd:description>
  #                    <pd:integerType/>
  #                    <pd:value>8</pd:value>
  #                </pd:parameter>
  ############  
  ########################
  rlog::log_info(paste("STEP3b: Adding parameter options"))
  stepsNode = newXMLNode("steps",parent=root)
  if(length(step_configurations)>0){
    for(i in 1:length(names(step_configurations))){
      initial_step_node = newXMLNode("step",parent=stepsNode)
      xmlAttrs(initial_step_node) = c(execution = "MANDATORY",code = names(step_configurations)[i])    
      step_label_node  = newXMLNode("label",names(step_configurations)[i],parent=initial_step_node)
      description_label_node  = newXMLNode("description",paste(names(step_configurations)[i],"parameters"),parent=initial_step_node)
      tool_description_node = newXMLNode("tool",parent=initial_step_node)
      xmlAttrs(tool_description_node) = c(code=paste(names(step_configurations)[i],"parameters"))
      nested_step_label_node  = newXMLNode("label",names(step_configurations)[i],parent=tool_description_node)
      nested_description_label_node  = newXMLNode("description",paste(names(step_configurations)[i],"parameters"),parent=tool_description_node)
      parameter_names = names(step_configurations[[names(step_configurations)[i]]])
      for(j in 1:length(parameter_names)){
        ##  input override for params.input
        if(parameter_names[j] == "input"){
          parameter_names[j] = "input_string"
        }
        parameter_metadata = step_configurations[[names(step_configurations[i])]][[parameter_names[j]]]
        nested_parameter_node = newXMLNode("parameter",parent=tool_description_node)
        xmlAttrs(nested_parameter_node) = c(code = names(step_configurations[[names(step_configurations)[i]]])[j],minValues = "1",maxValues="1",classification="USER")
        newXMLNode("label",parameter_names[j],parent=nested_parameter_node)
        newXMLNode("description",parameter_metadata[["description"]],parent=nested_parameter_node)
        if(parameter_names[j] %in% options_to_add){
          list_vals = options_to_add[[parameter_names[j]]]
          if(length(list_vals) > 0){
            options_node = newXMLNode(paste("optionsType"),parent=nested_parameter_node)
            for(lv in 1:length(list_vals)){
              newXMLNode("option",list_vals[lv],parent=options_node)
            }
          }
        } else{
          if(grepl("number",parameter_metadata[["type"]],ignore.case = T)){
            newXMLNode(paste("integer","Type",sep=""),parent=nested_parameter_node)
          } else{
            newXMLNode(paste(paste(parameter_metadata[["type"]],"Type",sep=""),sep=""),parent=nested_parameter_node)
          }
          if("default" %in% names(parameter_metadata)){
            newXMLNode("value",parameter_metadata[["default"]],parent=nested_parameter_node)
          } else{
            newXMLNode("value",createDummyValue(param_value ="" ,param_type = parameter_metadata[["type"]]),parent=nested_parameter_node)
          }
        }
      }
    }
  } else{
    rlog::log_warn(paste("STEP3a: No parameters found"))
  }
  # VIEW XML
  #print(doc)
  
  # SAVE XML TO FILE
  outputFile = paste(basename(dirname(nf_script)), "pipeline","xml",sep=".")
  if(!is.null(parameters_xml)){
    outputPath = parameters_xml
  } else{
    outputPath = paste(dirname(nf_script),"/",outputFile,sep="")
  }
  rlog::log_info(paste("STEP4: Generating parameters XML to",outputPath))
  saveXML(doc, file=outputPath,encoding="utf-8")
}

#### modify module files to get absolute path
scripts_to_absolute_path = list()
binary_dir = paste(dirname(nf_script),"bin",sep="/")
assets_dir = paste(dirname(nf_script),"assets",sep="/")
extensions_of_interest = c("py","pl","r","sh","bash")
if(dir.exists(binary_dir)){
  setwd(dirname(nf_script))
  files_of_interest = list.files(binary_dir,recursive=TRUE)
  basename_files_of_interest = apply(t(files_of_interest),2,basename)
  for(f in 1:length(files_of_interest)){
    basename_files_of_interest_split = strsplit(basename_files_of_interest[f],"\\.")[[1]]
    if(sum(tolower(basename_files_of_interest_split[length(basename_files_of_interest_split)]) %in% extensions_of_interest) >0 ){
      rlog::log_info(paste("ADDING",files_of_interest[f]))
      scripts_to_absolute_path[[files_of_interest[f]]] = paste("bin/",basename_files_of_interest[f],sep="")
    } else{
      rlog::log_info(paste("IGNORING:",files_of_interest[f]))
    }
  }
}
if(dir.exists(assets_dir)){
  setwd(dirname(nf_script))
  files_of_interest = list.files(assets_dir,recursive=TRUE)
  basename_files_of_interest = apply(t(files_of_interest),2,basename)
  for(f in 1:length(files_of_interest)){
    basename_files_of_interest_split = strsplit(basename_files_of_interest[f],"\\.")[[1]]
    if(sum(tolower(basename_files_of_interest_split[length(basename_files_of_interest_split)]) %in% extensions_of_interest) >0 ){
      rlog::log_info(paste("ADDING",files_of_interest[f]))
      scripts_to_absolute_path[[files_of_interest[f]]] = paste("assets/",basename_files_of_interest[f],sep="")
    } else{
      rlog::log_info(paste("IGNORING:",files_of_interest[f]))
    }
  }
}
pipeline_sub_dirs = list.dirs(dirname(nf_script))
pipeline_sub_dirs_of_interest = apply(t(pipeline_sub_dirs),2, function(x) basename(x) == "templates")
if(sum(pipeline_sub_dirs_of_interest) > 0){
  setwd(dirname(nf_script))
  for(j in 1:sum(pipeline_sub_dirs_of_interest)){
    other_dir_of_interest = pipeline_sub_dirs[pipeline_sub_dirs_of_interest][j]
    rlog::log_info(paste("Looking at additional files here:",other_dir_of_interest))
    files_of_interest = list.files(other_dir_of_interest,recursive=TRUE)
    basename_files_of_interest = apply(t(files_of_interest),2,basename)
    for(f in 1:length(files_of_interest)){
      rlog::log_info(paste("ADDING",files_of_interest[f]))
      relative_path = gsub(paste(dirname(nf_script),"/",sep=""),"",paste(other_dir_of_interest,"/",sep=""))
      scripts_to_absolute_path[[files_of_interest[f]]] = paste(relative_path,basename_files_of_interest[f],sep="")
    }
  }
}
print(names(scripts_to_absolute_path))
# make sure files are executable
override_nextflow_script_command <- function(script,associated_cmd){
  execution_choices = list()
  execution_choices[["sh"]] = "bash"
  execution_choices[["py"]] = "python"
  execution_choices[["r"]] = "Rscript"
  updated_cmd = ""
  script_filename_split = strsplit(basename(script),"\\.")[[1]]
  associated_cmd_split = strsplit(associated_cmd,"\\s+")[[1]]
  associated_cmd_split = associated_cmd_split[ associated_cmd_split!="" ]
  if(!"template" %in% associated_cmd_split){
    script_file_extension = tolower(script_filename_split[length(script_filename_split)])
    if(script_file_extension %in% names(execution_choices)){
      updated_cmd = paste(execution_choices[[script_file_extension]],associated_cmd)
    } else{
      updated_cmd = paste("bash",associated_cmd)
    }
  } else{
    updated_cmd = paste("\"","\n")
    #associated_cmd = paste(associated_cmd_split[associated_cmd_split != "template"],sep=" ",collapse = " ")
    script_file_extension = tolower(script_filename_split[length(script_filename_split)])
    #if(script_file_extension %in% names(execution_choices)){
    #  updated_cmd = paste(execution_choices[[script_file_extension]],associated_cmd,"\n","\"")
    #} else{
      updated_cmd = paste(associated_cmd,"\n","\"")
      updated_cmd = gsub("\\$\\{","$",updated_cmd)
      updated_cmd = gsub("\\}","",updated_cmd)
    #}
  }
  return(paste(updated_cmd,sep = " ",collapse=""))
}

## function to update module based on these conditions
absolute_path_update_module <- function(module_file){
  module_file_data = read.delim(module_file,quote="",header=F)
  #module_file_data = t(module_file_data)
  updated_lines = module_file_data
  paths_of_interest = names(scripts_to_absolute_path)
  found_updates = FALSE
  for(idx in 1:nrow(module_file_data)){
    found_update = FALSE
    module_line = module_file_data[idx,]
    module_line = gsub("\'","",module_line)
    module_line_split = strsplit(module_line,"\\s+")[[1]]
    module_line_split = module_line_split[module_line_split!= ""]
    if(length(module_line_split) > 0 ){
      for(poi in 1:length(paths_of_interest)){
        relative_path_lookup = paths_of_interest[poi]
        basename_path_lookup = scripts_to_absolute_path[[paths_of_interest[poi]]]
        if(sum(basename(relative_path_lookup) %in% module_line_split) > 0){
          replacement_value = paste("${workflow.launchDir}/",basename_path_lookup,sep="")
          found_update = TRUE
          found_updates = TRUE
          rlog::log_info(paste("relative_path_lookup:",relative_path_lookup))
          if(!grepl("launchDir",module_line_split[module_line_split %in% basename(relative_path_lookup)] )){
            replacement_value = override_nextflow_script_command(basename_path_lookup,replacement_value)
            module_line_split[module_line_split %in% relative_path_lookup] = replacement_value
          }
        } else if(sum(basename(basename_path_lookup) %in% module_line_split)>0){
          replacement_value = paste("${workflow.launchDir}/",basename_path_lookup,sep="")
          found_update = TRUE
          found_updates = TRUE
          rlog::log_info(paste("basename_path_lookup:",basename_path_lookup))
          if(!grepl("launchDir",module_line_split[module_line_split %in% basename_path_lookup] )){
            replacement_value = override_nextflow_script_command(basename_path_lookup,replacement_value)
            module_line_split[module_line_split %in% basename_path_lookup] = replacement_value
          }
        }
      }
    }
    if(found_update){
      if("template" %in% module_line_split){
        #module_line_split = module_line_split[module_line_split != "template"]
        #module_line_split = paste('\t"""\n',module_line_split,"\n",'\t"""',collapse = " ",sep = " ")
        #module_line_split = gsub("'","",module_line_split)
        module_line_split = paste("\n",module_line_split)
      }
      new_line = paste(module_line_split,collapse = " ",sep = " ")
      rlog::log_info(paste("UPDATING PATH in this line:",new_line))
      updated_lines[idx,] = new_line
    } 
  }
  if(found_updates){
    output_file = paste(module_file,".tmp",sep="")
    write.table(updated_lines,output_file,row.names=F,col.names = F,quote=F,sep="\n")
    rlog::log_info(paste("UPDATING:",module_file))
    system(paste("mv",output_file,module_file))
  } else{
    rlog::log_info(paste("No updates required for:",module_file))
  }
}

#### apply function above to all nextflow module files
module_dir = paste(dirname(nf_script),"modules",sep="/") ### make this configurable at runtime for one-off executions
module_files = list.files(module_dir,full.names=TRUE,recursive=TRUE)
if(length(module_files) > 0 ){
  if(length(names(scripts_to_absolute_path)) > 0) {
    for( m in 1:length(module_files)){
      rlog::log_info(paste("SCANNING for path updates in:",module_files[m]))
      absolute_path_update_module(module_file = module_files[m])
    }
  } else{
    rlog::log_warn(paste("No path updates needed"))
  }
} else{
  module_files = c(nf_script)
  if(length(names(scripts_to_absolute_path)) > 0) {
    for( m in 1:length(module_files)){
      rlog::log_info(paste("No module files, looking at main script:",module_files[m]))
      absolute_path_update_module(module_file = module_files[m])
    }
  } else{
    rlog::log_warn(paste("No path updates needed"))
  }
}
