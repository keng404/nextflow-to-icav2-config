options(stringsAsFactors=FALSE)
suppressPackageStartupMessages(library("argparse"))
library(rlog)
library(stringr)
# create parser object
parser <- ArgumentParser()

# specify our desired options 
# by default ArgumentParser will add an help option 

parser$add_argument("-s", "--nf-script","--nf_script", default=NULL, required=TRUE,
                    help="Main NF script")
parser$add_argument("-c","--config-file","--config_file", default=NULL,required=TRUE,
                    help = "main config file")
parser$add_argument("-e", "--dsl2-enabled","--dsl2_enabled", action="store_true",default=FALSE,
                    help="Create pipeline in ICA")
parser$add_argument("-f","--nf-core-mode","--nf_core_mode",action="store_true",
                    default=FALSE, help = "flag to indicate nf-core pipeline")
parser$add_argument("-x","--parameters-xml","--parameters_xml",
                    default=NULL, help = " parameters XML file output")
parser$add_argument("-k","--is-simple-config","--is_simple_config",
                    action="store_true",default=FALSE, help = "Use config")
parser$add_argument("-g","--generate-parameters-xml","--generate_parameters_xml",
                    action="store_true",default=FALSE, help = "Generate parameters XML file")
parser$add_argument("-i","--configs-to-ignore","--configs_to_ignore", default=c(""),nargs="?",
                    action="append",help="config files to ignore")
parser$add_argument("-u","--instance-type-url","--instance_type_url", default="https://help.ica.illumina.com/project/p-flow/f-pipelines#compute-types",
                    help = "URL that contains ICA instance type table")
parser$add_argument("-d","--default-instance","--default_instance", default = "himem-small",
                    help = "default instance value")
parser$add_argument("-a","--modules-config-file","--modules_config_file", default = NULL,
                    help = "configuration file for modules of a pipeline")
parser$add_argument("-m","--dummy-docker-image","--dummy_docker_image", default = "library/ubuntu:21.04",
                    help = "default Docker image to copy intermediate and report files from ICA")
parser$add_argument("-t","--intermediate-copy-template","--intermediate_copy_template", default = "dummy_template.txt",
                    help = "default NF script to copy intermediate and report files from ICA")
parser$add_argument("-w","--input-files-override","--input_files_override", default = "input_override.non_dsl2_pipelines.txt",
                    help = "default stub to override input")
parser$add_argument("-n","--ica-instance-namespace","--ica_instance_namespace", default="scheduler.illumina.com/presetSize",
                    help = "ICA instance type namespace : will allow ICA scheduler to know  what instances to use")
# get command line options, if help option encountered print help and exit,
# otherwise if options not found on command line then set defaults, 
args <- parser$parse_args()
is_simple_config = args$is_simple_config
nf_script = args$nf_script
config_file = args$config_file
config_dat  = read.delim(config_file,quote="",header=F)
input_override = read.delim(args$input_files_override,header=F,quote="")
parameters_xml  = args$parameters_xml
configs_to_ignore = args$configs_to_ignore
configs_to_ignore = configs_to_ignore[configs_to_ignore!=""]
generate_parameters_xml = args$generate_parameters_xml
if(!is.null(parameters_xml)){
  generate_parameters_xml = TRUE
}
###########
ica_instance_namespace = args$ica_instance_namespace
default_instance = args$default_instance
instance_type_table_url = args$instance_type_url
######### NEED to add collection of process labels --- will help with update of 
########## corresponding functions
########## these labels will contain metadata for cpu/memory, error retry strategy,
########## and docker images used to run process
getParamsFromConfig <- function(conf_data){
  ## identify lines referring to params defined in config file
  lines_to_keep = c()
  line_skip = FALSE
  in_params_closure = FALSE
  in_closure = FALSE
  out_closure = TRUE
  out_params_closure = TRUE
  initial_nested_param = NA
  nested_param_key = NA
  
  for(i in 1:nrow(conf_data)){
    line_split = strsplit(conf_data[i,],"\\s+")[[1]]
    clean_line = line_split
    for(t in 1:length(line_split)){
      sanitized_token = trimws(line_split[t])
      clean_line[t] = sanitized_token
    }
    clean_line = clean_line[clean_line!=""]
    line_skip = FALSE
    if(grepl("/",clean_line[1])){
      line_skip = TRUE
    } 
    if(!line_skip && grepl("\\{",conf_data[i,]) && grepl("params",conf_data[i,]) && !grepl("def",conf_data[i,]) && !grepl("if",conf_data[i,]) && !grepl("else",conf_data[i,]) && !grepl("\\(params",conf_data[i,])  && !grepl("\\{params",conf_data[i,]) && !grepl("\\$\\{",conf_data[i,])){
      rlog::log_info(paste("ENTERING_PARAMS_ENCLOSURE:",conf_data[i,]))
      in_params_closure = TRUE
    } else if(!line_skip && grepl("\\{",conf_data[i,]) && !grepl("\\$\\{",conf_data[i,]) && !grepl("params",conf_data[i,])){
      in_closure = TRUE
      out_closure = FALSE
    } else if(!line_skip && grepl("\\}",conf_data[i,]) && !grepl("\\$\\{",conf_data[i,]) &&  !grepl("params",conf_data[i,]) && in_closure == TRUE){
      in_closure = FALSE
      out_closure = TRUE
    } else if(!line_skip && in_params_closure && grepl("\\}",conf_data[i,]) && !grepl("\\$\\{",conf_data[i,]) && in_closure ==FALSE ){
    ### initial check each line to see if we've exited the params closure
        out_params_closure = TRUE
        in_params_closure = FALSE
        rlog::log_info(paste("EXITED_PARAMS_ENCLOSURE:",conf_data[i,]))
    } else{
      rlog::log_info(paste("NOT_CHANGING_STATUS:",conf_data[i,]))
    }
    
    ### grab parameters in params enclosure
    if(!line_skip && in_params_closure == TRUE){
      if(!grepl("\\}",conf_data[i,]) && !grepl("\\{",conf_data[i,]) && grepl("\\=",conf_data[i,])){
        if(is.na(initial_nested_param) && is.na(nested_param_key)){
          rlog::log_info(paste("ADDING_LINE:",conf_data[i,]))
          lines_to_keep = c(lines_to_keep,conf_data[i,])
        }
      } else{
        rlog::log_info(paste("OTHER_LINE:",conf_data[i,]))
        if(!line_skip && !grepl("params",conf_data[i,])){
          if(!grepl("\'",conf_data[i,]) && !grepl("\\$\\{",conf_data[i,]) && grepl("\\{",conf_data[i,])){
            initial_nested_param = strsplit(conf_data[i,],"\\s+")[[1]]
            initial_nested_param = initial_nested_param[initial_nested_param!=""]
            initial_nested_param = initial_nested_param[!grepl("\\{",initial_nested_param)]
          } else{
            if( grepl("\\{",conf_data[i,]) && !grepl("\\$\\{",conf_data[i,])){
              nested_param_key = strsplit(conf_data[i,],"\\s+")[[1]]
              nested_param_key = nested_param_key[nested_param_key!=""]
              nested_param_key = nested_param_key[!grepl("\\{",nested_param_key)]
              rlog::log_info(paste("initial_nested_param:",initial_nested_param,"\t","nested_param_key:",nested_param_key))
            } else{
              rlog::log_info(paste("IGNORE_PARAM_LINE:",conf_data[i,]))
            }
          }
        }
      }
      rlog::log_info(paste("LINE_OF_INTEREST:",conf_data[i,],"PARAMS_ENCLOSURE:",in_params_closure,"IN_OTHER_CLOSURE:",in_closure))
      rlog::log_info(paste("initial_nested_param:",initial_nested_param,"\t","nested_param_key:",nested_param_key))
      if(!line_skip && !is.na(initial_nested_param) && !is.na(nested_param_key)){
        if(grepl("\\}",conf_data[i,]) && !grepl("\\$\\{",conf_data[i,])){
          rlog::log_info(paste("resetting nested key:",conf_data[i,]))
          nested_param_key = NA
        } else{
          if(!is.na(initial_nested_param) && !is.na(nested_param_key) && grepl("\\=",conf_data[i,])){
            modified_line = paste(paste(initial_nested_param,"[",nested_param_key,"]",".",sep=""),trimws(conf_data[i,]),sep="")
            rlog::log_info(paste("ADDING_MODIFIED_LINE:",modified_line))
            lines_to_keep = c(lines_to_keep,modified_line)
          }
        }
      } else if(!line_skip && !is.na(initial_nested_param) && is.na(nested_param_key)){
        if(grepl("\'",conf_data[i,]) && grepl("\\{",conf_data[i,])){
          rlog::log_info(paste("resetting nested key rule2:",conf_data[i,]))
          nested_param_key = strsplit(conf_data[i,],"\\s+")[[1]]
          nested_param_key = nested_param_key[nested_param_key!=""]
          nested_param_key = nested_param_key[!grepl("\\{",nested_param_key)]
        } else{
          rlog::log_info(paste("IGNORE_PARAM_LINE2:",conf_data[i,]))
          #if(grepl("\\}",conf_data[i,])){
          #  initial_nested_param = NA
          #}
        }
      } else if(!line_skip && (grepl("params\\.",conf_data[i,]) || grepl("\\$\\{",conf_data[i,])) && grepl("\\=",conf_data[i,])){
        if(!grepl("nextflow",conf_data[i,]) && !grepl("def",conf_data[i,])){
          rlog::log_info(paste("ADDING_LINE_THAT_MIGHT_BE_MISSED:",conf_data[i,]))
          lines_to_keep = c(lines_to_keep,conf_data[i,])
        }
      }
    } else if (!line_skip && !grepl("def",conf_data[i,]) && !grepl("if",conf_data[i,])  && !grepl("else",conf_data[i,])){
      if(grepl("params\\.",conf_data[i,]) && grepl("\\=",conf_data[i,]) && !grepl("\\(params",conf_data[i,])  && !grepl("\\{params",conf_data[i,]) ){
        modified_line = conf_data[i,]
        #modified_line = gsub("params\\.","",conf_data[i,])
        rlog::log_info(paste("ADDING_LINE_STRIPPING_PARAMS:",modified_line))
        lines_to_keep = c(lines_to_keep,modified_line)
      }
    }
    
  } 
  rlog::log_info(paste("DONE_PARSING\n\n"))
  params = list()
  if(length(lines_to_keep) > 0){
    for(j in 1:length(lines_to_keep)){
      ### adding exception if parameter is introduced as a complex chained expression (i.e. params.goolar.ToString().toList().toValue().....)
      if(grepl("\\(",lines_to_keep[j])){
        lines_to_keep[j] = strsplit(lines_to_keep[j],"\\(")[[1]][1]
      }
      line_parsed = strsplit(lines_to_keep[j],"\\s+")[[1]]
      line_parsed = line_parsed[line_parsed != ""]
      params[[paste("params.",line_parsed[1],sep="")]] = line_parsed[3]
      # params[[paste("params.",line_parsed[1],sep="")]] = gsub("\'","",line_parsed[3])
    }
  }
  return(params)
}

z = getParamsFromConfig(conf_data=config_dat)

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

merge_module_configuration <- function(meta_module_configuration,new_module_configuration){
  updated_meta_module_configuration = meta_module_configuration
  modules_of_interest = names(new_module_configuration)
  for(j in 1:length(modules_of_interest)){
    module_of_interest = modules_of_interest[j]
    if(module_of_interest %in% names(updated_meta_module_configuration) ){
      rlog::log_info(paste("FOUND MODULE CONFIGURATION FOR:",module_of_interest,"AVOIDDING OVERRIDE"))
    } else{
      rlog::log_info(paste("ADDING MODULE CONFIGURATION FOR:",module_of_interest))
      updated_meta_module_configuration[[module_of_interest]] = new_module_configuration[[module_of_interest]]
    }
  }
  return(updated_meta_module_configuration)
}

if(args$dsl2_enabled){
  if(sum(is_module_config_file) > 0 ){
    if(is.null(args$modules_config_file)){
      modules_config_file = z1[is_module_config_file]
    } else{
      modules_config_file = args$modules_config_file
    }
    source('dsl2_compatible_parser.R')
    modules_config = loadModuleMetadata(modules_config_file)
    rlog::log_info(paste("Found MODULES_CONFIG_FILE:",modules_config_file))
    other_module_files = findOtherConfigs(conf_path=modules_config_file,conf_data=read.delim(modules_config_file,quote="",header=F))
    other_module_files = other_module_files[other_module_files!=""]
    if(length(other_module_files) > 0){
      for(omf_idx in 1:length(other_module_files)){
        rlog::log_info(paste("Found MODULES_CONFIG_FILE:",other_module_files[omf_idx]))
        module_config_subset = loadModuleMetadata(other_module_files[omf_idx])
        rlog::log_info(paste("UPDATING MODULE CONFIGURATION"))
        modules_config = merge_module_configuration(modules_config,module_config_subset)
      }
    }
    print(modules_config)
  } else{
    rlog::log_info(paste("Could not find MODULES_CONFIG_FILE.\nSkipping the module configuration edits\n"))
  }
}
#paramsFiller(list_to_fill=z1,params_list=z)


## STEP1: Grab nextflow config and propagate to all configs to grab the appropriate params
paramCollection = list()
if(length(final_config_list) > 0 ){
  for(config_idx in 1:length(final_config_list)){
    key_name = strsplit(basename(final_config_list[config_idx]),"\\.")[[1]][1]
    current_file = final_config_list[config_idx]
    rlog::log_info(paste("Reading in",current_file))
    parsedParams  = getParamsFromConfig(conf_data=read.delim(current_file,header=F))
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
 #3 compare paramCollection with z and avoid deplication -- if param exists in z,
# skip the corresponding param in paramCollection

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
#############
#getXMLSections = classifyParameters(paramsToXML = y)
#names(getXMLSections[["parameterSettings"]])
#names(getXMLSections[["dataInputs"]])
#################
### double-check or regenerate XML file ... which behavior to choose?

#parameters_xml = NULL
#generate_parameters_xml = TRUE
if(generate_parameters_xml){
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


grabProcessMetadataFromConfigs <- function(conf_files,AllParams,default_config){
  processMetadata = list()
  processMetadata[["default"]] = list()
  processMetadata[["default"]][["errorStrategy"]] = "ignore"
  processMetadata[["default"]][["maxForks"]] = "10"
  processMetadata[["default"]][["cpus"]] = 5
  processMetadata[["default"]][["memory"]] = "16 GB"
  for(f in 1:length(conf_files)){
    conf_file = conf_files[f]
    conf_data = read.delim(conf_file,quote="",header=F)
    rlog::log_info(paste("READING IN:",conf_file))
    in_process_closure = FALSE
    out_process_closure = TRUE
    in_process_label = FALSE
    out_process_label = TRUE
    process_label_name = NULL
    ####################################
    for(l in 1:nrow(conf_data)){
      skip_line = FALSE # --- line is a comment
      line_split = strsplit(conf_data[l,],"\\s+")[[1]]
      clean_line = line_split
      for(t in 1:length(line_split)){
        sanitized_token = trimws(line_split[t])
        clean_line[t] = sanitized_token
      }
      clean_line = clean_line[clean_line!=""]
      clean_line = clean_line[!is.na(clean_line)]
      if(grepl("/",clean_line[1])){
        skip_line = TRUE
      } else if(clean_line[1]=="process" && grepl("\\{",conf_data[l,])){
        in_process_closure = TRUE
        out_process_closure = FALSE
      } else if(grepl("withName",conf_data[l,]) || grepl("withLabel",conf_data[l,])){
        rlog::log_info(paste("PROCESS_LABEL_FOUND:",conf_data[l,]))
        rlog::log_info(paste("LINE_NUMBER:",l))
        in_process_label = TRUE
        out_process_label = FALSE
        process_label_name = gsub("\\{","",strsplit(clean_line[1],"\\:")[[1]][2])
        processMetadata[[process_label_name]] = list()
        processMetadata[[process_label_name]][["errorStrategy"]] =  "ignore"
        rlog::log_info(paste("GETTING_METADATA_FOR_PROCESS_LABEL:",process_label_name))
      } else if(in_process_label && grepl("\\}",conf_data[l,]) && !grepl("=",conf_data[l,])){
        in_process_label = FALSE
        out_process_label = TRUE
        process_label_name = NULL
        rlog::log_info(paste("EXITING_PROCESS_LABEL_CLOSURE:",conf_data[l,]))
      } else if(in_process_closure && out_process_label  && grepl("\\}",conf_data[l,])  && !grepl("=",conf_data[l,]) ){
        in_process_closure = FALSE
        out_process_closure = TRUE
        rlog::log_info(paste("EXITING_PROCESS_CLOSURE:",conf_data[l,]))
      }
    ###########################
      if(in_process_closure && !skip_line){
        if(!in_process_label){
          rlog::log_info(paste("PROCESS_CLOSURE_LINE:",conf_data[l,]))
          if(grepl("\\=",conf_data[l,]) && (grepl("cpus",conf_data[l,]) || grepl("memory",conf_data[l,]) || grepl("container",conf_data[l,]))){
            ## fill in params if they are referenced in the expression
            line_to_evaluate = conf_data[l,]
            if(grepl("\\$\\{",line_to_evaluate)){
              line_to_evaluate = gsub("\\$","",line_to_evaluate)
              line_to_evaluate = gsub("\\{","",line_to_evaluate)
              line_to_evaluate = gsub("\\}","",line_to_evaluate)
            }
            for(k in 1:length(names(AllParams))){
              if(!grepl("\\'",names(AllParams)[k]) && !grepl("\\(",names(AllParams)[k]) && !grepl("\\[",names(AllParams)[k])){
                if(grepl(names(AllParams)[k],line_to_evaluate)){
                  rlog::log_info(paste("INITIAL_LINE:",line_to_evaluate))
                  #line_to_evaluate = paramsFiller(list_to_fill =  c(line_to_evaluate),params_list = AllParams)
                  line_to_evaluate = gsub(names(AllParams)[k],gsub("'","",AllParams[[names(AllParams)[k]]]),line_to_evaluate)
                  rlog::log_info(paste("TRANSLATED_LINE:",line_to_evaluate))
                }
              }
            }
            if(grepl("cpus",conf_data[l,])){
              cpus = str_extract(line_to_evaluate,"[[:digit:]]+")
              rlog::log_info(paste("CPUS_PARSED:",cpus))
              processMetadata[["default"]][["cpus"]] = cpus
            } else if(grepl("memory",conf_data[l,])){
              mem = str_extract(line_to_evaluate,"[[:digit:]]+.[[:alpha:]]+")
              rlog::log_info(paste("MEM_PARSED:",mem))
              processMetadata[["default"]][["mem"]] = mem
            } else if(grepl("container",conf_data[l,])){
              container = line_to_evaluate
              rlog::log_info(paste("CONTAINER_PARSED:",container))
              processMetadata[["default"]][["container"]] =  container
              } else{
              rlog::log_info(paste("NOT_PARSING_PROCESS_CLOSURE_LINE:",conf_data[l,]))
            }
          }
          
        } else{
          #print(processMetadata)
          rlog::log_info(paste("PROCESS_LABEL_NAME",process_label_name))
          print(process_label_name %in% names(processMetadata))
          rlog::log_info(paste("NAMES:",paste(names(processMetadata),collapse=", ")))
          #rlog::log_info(paste("PROCESS_LABEL_CLOSURE_LINE:",conf_data[l,]))
          if(grepl("\\=",conf_data[l,]) && (grepl("cpus",conf_data[l,]) || grepl("memory",conf_data[l,]) || grepl("container",conf_data[l,]))){
            ## fill in params if they are referenced in the expression
            line_to_evaluate = conf_data[l,]
            if(grepl("\\$\\{",line_to_evaluate)){
              line_to_evaluate = gsub("\\$","",line_to_evaluate)
              line_to_evaluate = gsub("\\{","",line_to_evaluate)
              line_to_evaluate = gsub("\\}","",line_to_evaluate)
            }
            for(k in 1:length(names(AllParams))){
              if(!grepl("\\'",names(AllParams)[k]) && !grepl("\\(",names(AllParams)[k]) && !grepl("\\[",names(AllParams)[k])){
                if(grepl(names(AllParams)[k],line_to_evaluate)){
                  rlog::log_info(paste("INITIAL_LINE:",line_to_evaluate))
                  #line_to_evaluate = paramsFiller(list_to_fill =  c(line_to_evaluate),params_list = AllParams)
                  line_to_evaluate = gsub(names(AllParams)[k],gsub("'","",AllParams[[names(AllParams)[k]]]),line_to_evaluate)
                  rlog::log_info(paste("TRANSLATED_LINE:",line_to_evaluate))
                }
              }
            }
            if(grepl("cpus",conf_data[l,])){
              cpus = str_extract(line_to_evaluate,"[[:digit:]]+")
              rlog::log_info(paste("PROCESS_LABEL_CPUS_PARSED:",cpus))
              processMetadata[[process_label_name]][["cpus"]] = cpus
            } else if(grepl("memory",conf_data[l,])){
              mem = str_extract(line_to_evaluate,"[[:digit:]]+.[[:alpha:]]+")
              rlog::log_info(paste("PROCESS_LABEL_MEM_PARSED:",mem))
              processMetadata[[process_label_name]][["mem"]] = mem
            } else if(grepl("container",conf_data[l,])){
              container = line_to_evaluate
              rlog::log_info(paste("PROCESS_LABEL_CONTAINER_PARSED:",container))
              processMetadata[[process_label_name]][["container"]] =  container
            } else{
              rlog::log_info(paste("NOT_PARSING_PROCESS_LABEL_CLOSURE_LINE:",conf_data[l,]))
            }
          }
        }
      }
    }
  }
  return(processMetadata)
}
if(length(final_config_list) > 0){
  nf_process_metadata = grabProcessMetadataFromConfigs(conf_file = final_config_list,AllParams= y)
  print(nf_process_metadata)
} else{
  nf_process_metadata = list()
}


### add DSL2 support for this process, adding different process metadata for ICA to 
### properly handle these workflows

getDefaultContainer <- function(config_file){
  defaultContainer = "null"
  config_dat = read.delim(config_file,quote="",header=F)
  for(l in 1:nrow(config_dat)){
    if(grepl("process.container",config_dat[l,]) && grepl("\\=",config_dat[l,])){
      line_split = strsplit(config_dat[l,],"\\s+")[[1]]
      clean_line = line_split
      for(j in 1:length(line_split)){
        clean_line[j] = trimws(line_split[j])
      }
      clean_line = clean_line[clean_line!=""]
      clean_line = clean_line[!is.na(clean_line)]
      defaultContainer = clean_line[3]
    }
  }
  return(defaultContainer)
}

default_container = getDefaultContainer(config_file = config_file)

################
###  need to figure out how to keep proper indentation when modifying a process
##SAdd lines for each process that:
##  1) defines an instance-type --- use the Illumina GitBook for this  and  the configs to determine the appropriate cpu/ram settings 
## for each proess look for label, cpu, and or memory
## if for development, add a 'errorStrategy ignore' in our process
##  2) defines a docker image to run the instance on
#################

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
######3 workflow event checker -- see if events are malformed
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

### process checker --- see if processes are malformed
malformed_processes <- function(parsed_process_list,script_name){
  malformed_processes_check = FALSE
  malformed_processes_double_check = c()
  if(length(parsed_process_list) > 0){
    for(i in 1:length(names(parsed_process_list))){
      process_name = names(parsed_process_list)[i]
      rlog::log_info(paste("Double-checking contents of ",process_name,"in","updated_nf_processes variable"))
      if(length(parsed_process_list[[names(parsed_process_list)[i]]]) > 0){
        process_lines = parsed_process_list[[names(parsed_process_list)[i]]][["process_lines"]]
        process_lines = process_lines[process_lines!= ""]
        second_to_last_line = trimws(process_lines[length(process_lines)-1])
        last_line = trimws(process_lines[length(process_lines)])
        ### we will turn off the flag change for malformed_processes_double_check in the second to last line check
        ### R converts ''' to \"\"\"
        if(second_to_last_line != "'''" && second_to_last_line != "\"\"\""){
          rlog::log_warn(paste("POSSIBLE_MALFORMED_PROCESS for:",process_name,"in",script_name))
          rlog::log_warn(paste("POSSIBLE_MALFORMED_PROCESS for:",process_name,"\nFound",second_to_last_line,"expected","'''"))
          ##malformed_processes_check = TRUE
          if(!process_name %in% malformed_processes_double_check){
            malformed_processes_double_check = c(malformed_processes_double_check,process_name)
          }
        }
        if(last_line != "}"){
          rlog::log_warn(paste("POSSIBLE_MALFORMED_PROCESS for:",process_name,"in", script_name))
          rlog::log_warn(paste("POSSIBLE_MALFORMED_PROCESS for:",process_name,"\nFound",last_line,"expected","}"))
          malformed_processes_check = TRUE
          if(!process_name %in% malformed_processes_double_check){
            malformed_processes_double_check = c(malformed_processes_double_check,process_name)
          }
        }
      } else{
        rlog::log_warn(paste("POSSIBLE_MALFORMED_PROCESS for:",process_name,"in",script_name))
        stop(paste("EXITING: ",process_name,"is empty","in",script_name))
        malformed_processes_check = TRUE
      }
    }
    #return(malformed_processes_check)
    #if(malformed_processes_check){
    #  stop("EXITING: Please check stderr for error messages.\nIt looks like the following processes weren't properly parsed:",paste(malformed_processes_double_check,collapse=", "),"\nCheck the parseProcessesInNextflowScript function\n")
    #} else{
    #  rlog::log_info("All processes look ok!\n")
    #}
  } else{
    rlog::log_info("No processes to validate!\n")
  }
}
#############
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
addMemOrCPUdeclarations <- function(pod_annotation,lookup_table){
  pod_annotation_split = strsplit(pod_annotation,"\\s+")[[1]]
  pod_annotation_split[length(pod_annotation_split)] = gsub("'","",pod_annotation_split[length(pod_annotation_split)])
  pod_annotation = pod_annotation_split[length(pod_annotation_split)] 
  declarations_to_add = c()
  scale_factor = 0.75  # make sure we don't take up all CPUs and memory to avoid throtting/job failure that way
  if(is.null(pod_annotation)){
    declarations_to_add = c(declarations_to_add,"\tcpus 2")
    declarations_to_add = c(declarations_to_add,"\tmemory 4GB")
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
######################################
parseProcessesInNextflowScript <- function(nf_script,nf_process_metadata,default_container,ica_instance_table,AllParams){
  nf_script_process_lines = list()
  dsl2_enabled = FALSE
  in_process = FALSE
  out_process = TRUE
  process_has_input = FALSE
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
    line_split = strsplit(as.character(nf_script_dat[i,]),"\\s+")[[1]]
    clean_line = line_split
    for(j in 1:length(line_split)){
      clean_line[j] = trimws(line_split[j])
    }
    clean_line = clean_line[clean_line!=""]
    clean_line = clean_line[!is.na(clean_line)]
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
    line_split = strsplit(as.character(nf_script_dat[i,]),"\\s+")[[1]]
    clean_line = line_split
    for(j in 1:length(line_split)){
      clean_line[j] = trimws(line_split[j])
    }
    clean_line = clean_line[clean_line!=""]
    clean_line = clean_line[!is.na(clean_line)]
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
          process_name = gsub("\\{","",clean_line[2])
          rlog::log_info(paste("FOUND_PROCESS_NAME:",process_name))
          nf_script_process_lines[[process_name]] = list()
          process_label = NULL
          container_specified = FALSE
          process_has_input = FALSE
          process_labels = c()
          line_numbers = c(i)
          process_cpus = c()
          process_memory = c()
          process_lines = c()
          process_container = default_container
          in_expression = FALSE
        } else if(in_process && (length(clean_line)>1) && grepl("\\{",nf_script_dat[i,]) && !grepl("\\$\\{",nf_script_dat[i,]) && ( clean_line[length(clean_line)] == "{" || grepl("if",clean_line) || grepl("else",clean_line) || grepl("def",clean_line))){
          in_expression = TRUE
          rlog::log_info(paste("EXPRESSION_LINE_ENTER:",nf_script_dat[i,]))
          ### ADD EXCEPTION if expression is bash and not Groovy
          if(( grepl("if",clean_line) || grepl("else",clean_line)) && grepl("\\[",nf_script_dat[i,])){
            in_expression = FALSE
          }
          #process_lines = c(process_lines,nf_script_dat[i,])
        } else if(in_process &&  (clean_line[1] == '"""'|| clean_line[1] == "}") && in_expression){
          in_expression = FALSE
          rlog::log_info(paste("EXPRESSION_LINE_EXIT:",nf_script_dat[i,]))
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
          process_has_input = FALSE
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
            process_has_input = TRUE
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
            process_cpus = process_cpus[!is.na(process_cpus)]
            process_memory = process_memory[!is.na(process_memory)]
            rlog::log_info(paste("CPUS",paste(process_cpus,sep=","),"MEM",paste(process_memory,sep=""),"CONTAINER",process_container))
            process_pod_annotation = getInstancePodAnnotation(cpus = process_cpus,mem = process_memory,container_name = process_container,ica_instance_table = ica_instance_table)
            rlog::log_info(paste("ADDING_POD_ANNOTATION:",process_pod_annotation))
            process_lines = c(process_lines,paste(line_indent,process_pod_annotation,sep=""))
            additional_lines = addMemOrCPUdeclarations(process_pod_annotation,ica_instance_table)
            for(ali in 1:length(additional_lines)){
              process_lines = c(process_lines,paste(line_indent,additional_lines[ali],sep=""))
            }
            rlog::log_info(paste("ADDING_LEINENT_ERROR_STRATEGY:",paste("errorStrategy 'ignore'",sep="")))
            process_lines = c(process_lines,paste(line_indent,"errorStrategy 'ignore'",sep=""))
            rlog::log_info(paste("ADDING_TIME_COMPONENT:",paste("time","'1day'")))
            process_lines = c(process_lines,paste(line_indent,paste("time","'1day'"),sep=""))
            rlog::log_info(paste("ADDING_MAX_FORKS_COMPONENT:",paste("maxForks","10")))
            process_lines = c(process_lines,paste(line_indent,paste("maxForks","10"),sep=""))
            rlog::log_info(paste("ADDING_LINE:",nf_script_dat[i,]))
            process_lines = c(process_lines,nf_script_dat[i,])
          } else if((clean_line[1] == "output:"  || clean_line[1] == "when:") && !process_has_input){
            line_indent = gsub("output:","",nf_script_dat[i,])
            line_indent = gsub("when:","",line_indent)
            rlog::log_info(paste("OUTPUT_LINE_INDENT_FIRST_TOKEN:",line_indent))
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
            process_cpus = process_cpus[!is.na(process_cpus)]
            process_memory = process_memory[!is.na(process_memory)]
            rlog::log_info(paste("CPUS",paste(process_cpus,sep=","),"MEM",paste(process_memory,sep=""),"CONTAINER",process_container))
            process_pod_annotation = getInstancePodAnnotation(cpus = process_cpus,mem = process_memory,container_name = process_container,ica_instance_table = ica_instance_table)
            rlog::log_info(paste("ADDING_POD_ANNOTATION:",process_pod_annotation))
            
            process_lines = c(process_lines,paste(line_indent,process_pod_annotation,sep=""))
            rlog::log_info(paste("ADDING_LEINENT_ERROR_STRATEGY:",paste("errorStrategy 'ignore'",sep="")))
            process_lines = c(process_lines,paste(line_indent,"errorStrategy 'ignore'",sep=""))
            rlog::log_info(paste("ADDING_TIME_COMPONENT:",paste("time","'1day'")))
            process_lines = c(process_lines,paste(line_indent,paste("time","'1day'"),sep=""))
            rlog::log_info(paste("ADDING_MAX_FORKS_COMPONENT:",paste("maxForks","10")))
            process_lines = c(process_lines,paste(line_indent,paste("maxForks","10"),sep=""))
            rlog::log_info(paste("ADDING_LINE:",nf_script_dat[i,]))
            process_lines = c(process_lines,nf_script_dat[i,])
            } else if(clean_line[1] == "cpus"){
              for(k in 1:length(names(AllParams))){
                if(!grepl("\\'",names(AllParams)[k]) && !grepl("\\[",names(AllParams)[k])){
                  if(grepl(names(AllParams)[k],nf_script_dat[i,])){
                    rlog::log_info(paste("INITIAL_LINE:",nf_script_dat[i,]))
                    #line_to_evaluate = paramsFiller(list_to_fill =  c(line_to_evaluate),params_list = AllParams)
                    nf_script_dat[i,] = gsub(names(AllParams)[k],gsub("'","",AllParams[[names(AllParams)[k]]]),nf_script_dat[i,])
                    rlog::log_info(paste("TRANSLATED_LINE:",nf_script_dat[i,]))
                  }
                }
              }
              line_split = strsplit(nf_script_dat[i,],"\\s+")[[1]]
              clean_line = line_split
              for(j in 1:length(line_split)){
                clean_line[j] = trimws(line_split[j])
              }
              clean_line = clean_line[clean_line!=""]
              clean_line = clean_line[!is.na(clean_line)]
              clean_line_length = length(clean_line)
            line_indent = gsub("cpus","",nf_script_dat[i,])
            cpus = strtoi(clean_line[2])
            process_cpus = c(process_cpus,cpus)
            rlog::log_info(paste("ADDING_LINE:",nf_script_dat[i,]))
            process_lines = c(process_lines,nf_script_dat[i,])
          } else if(clean_line[1] == "memory"){
            for(k in 1:length(names(AllParams))){
              if(!grepl("\\'",names(AllParams)[k]) && !grepl("\\(",names(AllParams)[k]) && !grepl("\\[",names(AllParams)[k])){
                if(grepl(names(AllParams)[k],nf_script_dat[i,])){
                  rlog::log_info(paste("INITIAL_LINE:",nf_script_dat[i,]))
                  #line_to_evaluate = paramsFiller(list_to_fill =  c(line_to_evaluate),params_list = AllParams)
                  nf_script_dat[i,] = gsub(names(AllParams)[k],gsub("'","",AllParams[[names(AllParams)[k]]]),nf_script_dat[i,])
                  rlog::log_info(paste("TRANSLATED_LINE:",nf_script_dat[i,]))
                }
              }
            }
            line_split = strsplit(nf_script_dat[i,],"\\s+")[[1]]
            clean_line = line_split
            for(j in 1:length(line_split)){
              clean_line[j] = trimws(line_split[j])
            }
            clean_line = clean_line[clean_line!=""]
            clean_line = clean_line[!is.na(clean_line)]
            clean_line_length = length(clean_line)
            line_indent = gsub("memory","",nf_script_dat[i,])
            memory = strtoi(gsub("\\.GB","",clean_line[2]))
            process_memory = c(process_memory,memory)
            rlog::log_info(paste("ADDING_LINE:",nf_script_dat[i,]))
            process_lines = c(process_lines,nf_script_dat[i,])
          } else if(clean_line[1] == "container"){
            for(k in 1:length(names(AllParams))){
              if(!grepl("\\'",names(AllParams)[k]) && !grepl("\\(",names(AllParams)[k]) && !grepl("\\[",names(AllParams)[k])){
                if(grepl(names(AllParams)[k],nf_script_dat[i,])){
                  rlog::log_info(paste("INITIAL_LINE:",nf_script_dat[i,]))
                  #line_to_evaluate = paramsFiller(list_to_fill =  c(line_to_evaluate),params_list = AllParams)
                  nf_script_dat[i,] = gsub(names(AllParams)[k],gsub("'","",AllParams[[names(AllParams)[k]]]),nf_script_dat[i,])
                  rlog::log_info(paste("TRANSLATED_LINE:",nf_script_dat[i,]))
                }
              }
            }
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
valid_nf_file <- function(file_path){
  file_path1 = gsub(".nf$","",file_path)
  file_path_split = strsplit(basename(file_path1),"\\.")[[1]]
  file_path_split[1] = gsub("[[:alpha:]]+","",file_path_split[1])
  if(file_path_split[1] != ""){
    return(FALSE)
  } else{
    return(TRUE)
  }
}

if(args$dsl2_enabled){
  source('dsl2_compatible_parser.R')
  #main_script = "nf-core/rnaseq/main.nf"
  first_pass = find_all_nf_scripts(main_script=nf_script)
  scripts_to_check = c()
  scripts_to_ignore = c()
  scripts_seen = c()
  if(!is.null(first_pass)){
    scripts_to_check = find_all_nf_scripts(main_script=nf_script)[["scripts_to_look_at"]]
    additional_files = list.files(paste(dirname(nf_script),"modules",sep="/"),pattern=".nf",full.names = TRUE,recursive = TRUE)
    additional_files = additional_files[!scripts_to_check %in% additional_files]
    scripts_to_check = c(scripts_to_check,additional_files)
  }
  if(length(scripts_to_check) > 0){
    workflow_script = scripts_to_check
    scripts_to_add = workflow_script
    scripts_to_parse = workflow_script
    while(length(scripts_to_add) > 0){
      scripts_to_add = scripts_to_add[apply(t(scripts_to_add),2,valid_nf_file)]
      scripts_to_add = scripts_to_add[!scripts_to_add %in% scripts_to_ignore]
      if(length(scripts_to_add) > 0){
        rlog::log_info(paste("LOOKING to parse these files:",paste(scripts_to_add,collapse=", ")))
        scripts_to_parse = c()
        scripts_of_interest_metadata = NULL
        second_pass_scripts = c()
        path_exists_check = c()
        for(i in 1:length(scripts_to_add)){
          scripts_seen = c(scripts_seen,scripts_to_add[i])
          rlog::log_info(paste("Looking through the following script:",paste(scripts_to_add[i],sep=", ")))
          scripts_of_interest_metadata = find_all_nf_scripts(main_script=simplify_path(scripts_to_add[i]))
          #print(scripts_of_interest_metadata)
          if(!is.null(scripts_of_interest_metadata)){
            print(scripts_of_interest_metadata)
            scripts_of_interest = scripts_of_interest_metadata[["scripts_to_look_at"]]
            #print(scripts_of_interest)
            if(length(second_pass_scripts) > 0){
              path_exists_check  = apply(t(second_pass_scripts),2, function(x) does_path_exist(x))
              if(sum(!path_exists_check) > 0){
                fois = second_pass_scripts[!path_exists_check]
                stop(paste("EXITING: DOUBLE-CHECK if these scripts exist:",paste(fois,collapse=", ")))
              }
            }
            scripts_to_parse = c(scripts_to_parse,scripts_of_interest[!scripts_of_interest %in% scripts_to_check])
            scripts_to_parse = scripts_to_parse[!scripts_to_parse %in% scripts_seen]
            scripts_to_check = c(scripts_to_check,scripts_of_interest[!scripts_of_interest %in% scripts_to_check])
            scripts_to_parse = scripts_to_check[!scripts_to_check %in% scripts_seen]
          } else{
            scripts_to_ignore = c(scripts_to_ignore,scripts_to_add[i])
          }
        }
      #print(scripts_to_parse)
      scripts_to_parse = scripts_to_parse[!scripts_to_parse %in% scripts_to_ignore]
      scripts_to_check = scripts_to_check[!scripts_to_check %in% scripts_to_ignore]
      scripts_to_parse = scripts_to_parse[!scripts_to_parse %in% scripts_seen]
      scripts_to_check = scripts_to_check[!scripts_to_check %in% scripts_seen]
      scripts_to_add = scripts_to_parse
      rlog::log_info(paste("SCRIPTS_TO_ADD:",paste(scripts_to_add,collapse=", ")))
      rlog::log_info(paste("SCRIPTS_TO_IGNORE:",paste(scripts_to_ignore,collapse=", ")))
      rlog::log_info(paste("SCRIPTS_SEEN:",paste(scripts_seen,collapse=", ")))
      } else{
        scripts_to_add = NULL
      }
    } 
  } else{
    rlog::log_warn(paste("Did not find additional NF scripts to parse in addition to:",main_script))
  }
  scripts_to_check = c(updated_nf_file,unique(apply(t(unique(scripts_to_check)),2,function(x) simplify_path(x))))
  rlog::log_info(paste("Parsing through the following scripts:",paste(scripts_to_check,sep=", ")))
} else{
  scripts_to_check = c(updated_nf_file)
}
### gnereate dummy process to copy intermediate files
find_dummy_output_channel = function(process_list){
  dummy_output_channel = "null"
  found_channel = FALSE
  # for dummy_output scan through the updated_nf_processes in reverse order until we find the first output channel
  for(n in length(names(process_list)):1){
    process_lines = process_list[[names(process_list)[n]]][["process_lines"]]
    skip_line = FALSE ### skip comment lines 
    in_output_directive = FALSE
    if(!found_channel){
      for(pl in 1:length(process_lines)){
        skip_line = FALSE
        rlog::log_info(paste("LOOKING_AT_LINE_FOR_CHANNEL",process_lines[pl]))
        if(!is.null(process_lines[pl])){
          line_split = strsplit(process_lines[pl],"\\s+")[[1]]
          clean_line = line_split
          for(j in 1:length(line_split)){
            clean_line[j] = trimws(line_split[j])
          }
          clean_line = clean_line[clean_line!=""]
          clean_line = clean_line[!is.na(clean_line)]
          if(grepl("/",clean_line[1])){
            skip_line = TRUE
          } else{
            if(clean_line[1] == "output:"){
              in_output_directive = TRUE
            }
            if(in_output_directive){
              if(grepl("file",process_lines[pl]) && grepl("into",process_lines[pl])){
                rlog::log_info(paste("FOUND_DUMMY_CHANNEL_EXPRESSION:",process_lines[pl]))
                dummy_output_channel = process_lines[pl]
                found_channel = TRUE
                break
              }
            }
          }
        }
      }
    }
  }
  return(dummy_output_channel)
}
##########
workflow_event_added_commands = read.delim(args$intermediate_copy_template,quote="",header=F)
workflow_event_added_commandsv1 = c()
for(wead_idx in 1:nrow(workflow_event_added_commands)){
  workflow_event_added_commandsv1 = c(workflow_event_added_commandsv1,workflow_event_added_commands[wead_idx,])
}
workflow_event_added_commands = workflow_event_added_commandsv1
workflow_events = c("workflow.onComplete","workflow.onError")
##############
source('dsl2_compatible_parser.R')
module_files = c()
workflow_files = c()
subworkflow_files = c()
for(uidx in 1:length(scripts_to_check)){
  rlog::log_info(paste("Starting to parse:",scripts_to_check[uidx]))
  if(sum("modules" %in% strsplit(scripts_to_check[uidx],"/")[[1]]) > 0) {
    module_files = c(module_files,scripts_to_check[uidx])
  }
  if(sum("workflows" %in% strsplit(scripts_to_check[uidx],"/")[[1]]) > 0 ) {
    workflow_files = c(workflow_files,scripts_to_check[uidx])
  }
  if(sum("subworkflows" %in% strsplit(scripts_to_check[uidx],"/")[[1]]) > 0 ) {
    subworkflow_files = c(subworkflow_files,scripts_to_check[uidx])
  }
  updated_nf_processes = parseProcessesInNextflowScript(nf_script=scripts_to_check[uidx],nf_process_metadata=nf_process_metadata,default_container = default_container,ica_instance_table = ica_instance_table,AllParams = y)
  #print(updated_nf_processes[["VEP"]])
  #print(updated_nf_processes)

  updated_nf_processes = processEnclosureCheck(updated_nf_processes)
  print(updated_nf_processes)
  malformed_processes(updated_nf_processes,scripts_to_check[uidx])
  ### add-in dummy process to copy back intermediate files and pipeline reports
  nf_workflow_events = list()
  if(uidx == 1){
    rlog::log_info(paste("Grabbing workflow events from:",scripts_to_check[uidx]))
    nf_workflow_events = getWorkflowEvents(script = scripts_to_check[uidx])
    malformed_workflow_event_processes(nf_workflow_events,scripts_to_check[uidx])
    print(nf_workflow_events)
  }
  script_metadata = list()
  if(args$dsl2_enabled){
    script_metadata = find_all_nf_scripts(main_script=scripts_to_check[uidx])
  }
  ############
  ## check out the updated processes and the line #s. Keep what isn't a process-line # (i.e. line # defining a process)
  ## when you find a process, update it
  
  # get first line # of each process to determine the order in which we'll update our updated_nf_file
  # this will inform us of lines outside the process that we'll want to keep
  process_updated_nf_file = gsub(".nf$",".dev.nf",scripts_to_check[uidx])
  key_order = c()
  second_pass_updated_lines = c()
  if(length(names(updated_nf_processes)) > 0){
    rlog::log_info(paste("Update process for:",scripts_to_check[uidx]))
    for(i in 1:length(names(updated_nf_processes))){
      line_num = min(updated_nf_processes[[names(updated_nf_processes)[i]]][["line_numbers"]])
      key_order[i] = line_num
    }
    process_order = names(updated_nf_processes)[order(key_order)]
    updated_nf_dat = read.delim(scripts_to_check[uidx],quote="",header=F)
    previous_line_count = 0
    for(pidx in 1:length(process_order)){
      current_process = updated_nf_processes[[process_order[pidx]]]
      current_process_new_lines = current_process[["process_lines"]]
      current_process_line_numbers = current_process[["line_numbers"]]
      if(length(current_process_new_lines) == 0){
        rlog::log_error(paste("NO_LINES_FOUND_FOR_PROCESS",process_order[pidx]))
      }
      lower_line_count_bound = 0
      upper_line_count_bound = min(current_process_line_numbers)
      if(pidx > 1){
        lower_line_count_bound = max(updated_nf_processes[[process_order[pidx-1]]][["line_numbers"]])
      }
      rlog::log_info(paste("LOWER_LINE_BOUND",lower_line_count_bound,"UPPER_LINE_BOUND",upper_line_count_bound))
      lines_to_add = TRUE
      for(lidx in 1:nrow(updated_nf_dat)){
        if(!lines_to_add){
          break
        }
        if(lidx < upper_line_count_bound && lidx > lower_line_count_bound){
          second_pass_updated_lines = c(second_pass_updated_lines,updated_nf_dat[lidx,])
        } else if(lidx == min(current_process_line_numbers)){
            for(k in 1:length(current_process_new_lines)){
              if(args$dsl2_enabled){
                script_of_interest_metadata = find_all_nf_scripts(main_script=scripts_to_check[uidx])
                renaming_metadata = NULL
                if(!is.null(script_of_interest_metadata)){
                  renaming_metadata = script_of_interest_metadata[["scripts_rename"]]
                }
                if(length(renaming_metadata) > 0){
                  for(ns in 1:length(names(renaming_metadata))){
                    rlog::log_info(paste("UPDATING line:",current_process_new_lines[k],"to"))
                    current_process_new_lines[k] = gsub(names(renaming_metadata)[ns],paste("'",renaming_metadata[[names(renaming_metadata)[ns]]],"'"),current_process_new_lines[k])
                    rlog::log_info(paste("UPDATED_LINE:",current_process_new_lines[k]))
                  }
                }
              }
              second_pass_updated_lines = c(second_pass_updated_lines,current_process_new_lines[k])
            }
          lines_to_add = FALSE
        }
      }
      current_line_count = length(second_pass_updated_lines)
      rlog::log_info(paste("ADDED",current_line_count-previous_line_count,"lines from process:",process_order[pidx]))
      previous_line_count = current_line_count
    }
    ##### copy intermediate files
    #### get the rest of the script
    line_count_to_finish = max(current_process_line_numbers) + 1
    if(line_count_to_finish < nrow(updated_nf_dat)){
      for(lidx in line_count_to_finish:nrow(updated_nf_dat)){
        second_pass_updated_lines = c(second_pass_updated_lines,updated_nf_dat[lidx,])
      }
    }
    ########################3
    reharvested_lines = c()
    new_lines = c()
    if(uidx == 1){
      rlog::log_info(paste("adding additional workflow event to:",scripts_to_check[uidx]))
      line_number_shift = length(second_pass_updated_lines) - nrow(updated_nf_dat)
      rlog::log_info(paste("Initial line_number_shift:",line_number_shift))
      for(weidx in 1:length(workflow_events)){
        workflow_event = workflow_events[weidx]
    #if(length(workflow_events) > 0 ){
        #for(weidx in 1:length(names(nf_workflow_events))){
          #workflow_event = workflow_events[weidx]
            if(workflow_event %in% names(nf_workflow_events)){
              rlog::log_info(paste("workflow event found:",workflow_event))
              process_lines = nf_workflow_events[[workflow_event]][["process_lines"]]
              line_numbers = nf_workflow_events[[workflow_event]][["line_numbers"]]
              line_index_to_start = (line_numbers[1]-1) + line_number_shift
              rlog::log_info(paste("line_index_to_start:",line_index_to_start))
              reharvested_lines = second_pass_updated_lines[1:line_index_to_start]
              for(lidx in 1:(length(process_lines)-1)){
                reharvested_lines = c(reharvested_lines,process_lines[lidx])
              }
              for(welidx in 1:length(workflow_event_added_commands)){
                reharvested_lines = c(reharvested_lines,paste("\t",workflow_event_added_commands[welidx],sep="")) 
              }
             number_of_lines_to_add =  (length(process_lines)-1) + length(workflow_event_added_commands)
             rlog::log_info(paste("number_of_lines_to_add:",number_of_lines_to_add + 1))
              line_number_shift = line_number_shift + length(workflow_event_added_commands)
              rlog::log_info(paste("adding existing workflow event lines:",workflow_event,"new_line_number_shift:",line_number_shift))
              reharvested_lines = c(reharvested_lines,process_lines[length(process_lines)])
              rlog::log_info(paste("initial_length_of_script:",length(second_pass_updated_lines),"length_of_new_script:",length(reharvested_lines)))
              current_script_length = length(second_pass_updated_lines) 
              ## add downstream lines if needed --- there mey be functions defined after the workflow.onComplete/onError directives
              if(current_script_length > length(reharvested_lines)){
                new_start_idx = length(reharvested_lines) -length(workflow_event_added_commands) + 1
                for(ii in new_start_idx:current_script_length){
                  reharvested_lines = c(reharvested_lines,second_pass_updated_lines[ii])
                }
              }
            } else{
              rlog::log_info(paste("workflow event not found:",workflow_event))
              new_lines = unlist(c(paste(workflow_event,"{"),workflow_event_added_commands,"}"))
              second_pass_updated_lines = c(second_pass_updated_lines,new_lines)
            }
      }
      #else{
      #  rlog::log_info(paste("workflow event not found:",workflow_event))
      #  new_lines = unlist(c(paste(workflow_event,"{"),workflow_event_added_commands,"}"))
      #  rlog::log_info(paste("adding new workflow event lines:",paste(new_lines,collapse="\n")))
      #  reharvested_lines = c(reharvested_lines,new_lines)
      #  }
      if(length(reharvested_lines) > 0){
        second_pass_updated_lines = reharvested_lines
      }
      #if(length(new_lines) > 0) {
      #  second_pass_updated_lines  = c(second_pass_updated_lines,new_lines)
      #  second_pass_updated_lines = unique(second_pass_updated_lines)
      #}
      }
    
  } else{
    rlog::log_info(paste("No process update required for:",scripts_to_check[uidx]))
    updated_nf_dat =  read.delim(scripts_to_check[uidx],quote="",header=F)
    ############## initialize lines as the lines from the original file --- no modifications
    second_pass_updated_linesv1 = c()
    for(line_index in 1:nrow(updated_nf_dat)){
      second_pass_updated_linesv1 = c(second_pass_updated_linesv1,updated_nf_dat[line_index,])
    }
    second_pass_updated_lines = second_pass_updated_linesv1
    reharvested_lines = c()
    #######################
    reharvested_lines = c()
    new_lines = c()
    if(uidx == 1){
      rlog::log_info(paste("adding additional workflow event to:",scripts_to_check[uidx]))
      line_number_shift = 0
      found_workflow_event = FALSE
      for(weidx in 1:length(workflow_events)){
        ##if(length(workflow_events) > 0 ){
        found_workflow_event = FALSE
          workflow_event = workflow_events[weidx]
          #for(weidx in 1:length(names(nf_workflow_events))){
            rlog::log_info(workflow_event)
            if(workflow_event %in% names(nf_workflow_events)){
              found_workflow_event = TRUE
              rlog::log_info(paste("workflow event found:",workflow_event))
              process_lines = nf_workflow_events[[workflow_event]][["process_lines"]]
              line_numbers = nf_workflow_events[[workflow_event]][["line_numbers"]]
              line_index_to_start = (line_numbers[1]-1) + line_number_shift
              rlog::log_info(paste("line_index_to_start:",line_index_to_start))
              reharvested_lines = second_pass_updated_lines[1:line_index_to_start]
              rlog::log_info(paste("adding existing workflow event lines:",workflow_event,"number_of_lines:"))
              for(lidx in 1:(length(process_lines)-1)){
                reharvested_lines = c(reharvested_lines,process_lines[lidx])
              }
              for(welidx in 1:length(workflow_event_added_commands)){
                reharvested_lines = c(reharvested_lines,paste("\t",workflow_event_added_commands[welidx],sep="")) 
              }
              number_of_lines_to_add =  (length(process_lines)-1) + length(workflow_event_added_commands)
              rlog::log_info(paste("number_of_lines_to_add:",number_of_lines_to_add + 1))
              line_number_shift = line_number_shift + length(workflow_event_added_commands)
              reharvested_lines = c(reharvested_lines,process_lines[length(process_lines)])
              rlog::log_info(paste("initial_length_of_script:",length(second_pass_updated_lines),"length_of_new_script:",length(reharvested_lines)))
              current_script_length = length(second_pass_updated_lines) 
              ## add downstream lines if needed --- there mey be functions defined after the workflow.onComplete/onError directives
              if(current_script_length > length(reharvested_lines)){
                new_start_idx = length(reharvested_lines) -length(workflow_event_added_commands) + 1
                for(ii in new_start_idx:current_script_length){
                  reharvested_lines = c(reharvested_lines,second_pass_updated_lines[ii])
                }
              }
            } else{
              rlog::log_info(paste("workflow event not found:",workflow_event))
              new_lines = unlist(c(paste(workflow_event,"{"),workflow_event_added_commands,"}"))
              rlog::log_info(paste("adding new workflow event lines:",paste(new_lines,collapse="\n")))
              second_pass_updated_lines = c(second_pass_updated_lines,new_lines)
            }
      }
      if(length(reharvested_lines) > 0){
        second_pass_updated_lines = reharvested_lines
      }
    #  if(length(new_lines) > 0) {
    #    second_pass_updated_lines  = c(second_pass_updated_lines,new_lines)
    #    second_pass_updated_lines = unique(second_pass_updated_lines)
    #  }
    }
  }
  
  #if(length(script_metadata) > 0){
  #  script_rename = script_metadata[["scripts_rename"]]
  #  script_name_original = names(script_rename)
  #  for(spul in 1:length(second_pass_updated_lines)){
  #    for(sno in 1:length(script_name_original)){
  #      if(grepl(script_name_original[sno],second_pass_updated_lines[spul])){
  #        rlog::log_info(paste("Renaming",script_name_original[sno],"to",script_rename[[script_name_original[sno]]]))
  #        second_pass_updated_lines[spul] = gsub(script_name_original[sno],script_rename[[script_name_original[sno]]],second_pass_updated_lines[spul])
  #      }
  #    }
  #  }
  #}
  
  rlog::log_info(paste("Writing updated processes to",process_updated_nf_file))
  rlog::log_info(paste("FINAL_NUMBER_OF_LINES:", length(second_pass_updated_lines)))
  write.table(x=second_pass_updated_lines,file=process_updated_nf_file,sep="\n",quote=F,row.names=F,col.names=F)
  rlog::log_info(paste("Renaming_CMD:",paste("cp",process_updated_nf_file,scripts_to_check[uidx])))
  system(paste("cp",process_updated_nf_file,scripts_to_check[uidx]))
  ### check for null container map references
  fixNullContainerMap(nf_script=scripts_to_check[uidx])
  if(args$dsl2_enabled){
    if(length(names(modules_config)) > 0){
      rlog::log_info(paste("CHECKING_FOR_MODULES"))
      module_location = findModules(nf_file=scripts_to_check[uidx])
      print(module_location)
      if(length(names(module_location)) > 0 ){
        source('dsl2_compatible_parser.R')
        rlog::log_info(paste("ADDITIONAL_STEP:",scripts_to_check[uidx], "adding the proper module configurations based on:",modules_config_file))
        makeFinalEdits(nf_script = scripts_to_check[uidx],module_metadata = modules_config,module_location = module_location)
      } else{
        rlog::log_info(paste("SKIPPING edits based on module configuration"))
      }
    } else{
      rlog::log_info(paste("SKIPPING edits based on module configuration"))
    }
  }
}
###############
if(length(module_files) > 0){
  source('dsl2_compatible_parser.R')
  process_directives = c("publishDir","pod","errorStrategy","time")
  for(i in 1:length(module_files)){
    mfoi = module_files[i]
    mfoi_lines = t(read.delim(mfoi,header=F,quote=""))
    mfoi_lines1 = c()
    module_name = "UNKNOWN"
    process_directive_count = list()
    for(ipd in 1:length(process_directives)){
      process_directive_count[[process_directives[ipd]]] = 0
    }
    for(j in 1:length(mfoi_lines)){
      add_line = TRUE
      mfoi_lines_split = strsplit(mfoi_lines[j],"\\s+")[[1]]
      mfoi_lines_split = mfoi_lines_split[mfoi_lines_split!=""]
      if(length(mfoi_lines_split) >= 2){
        if("process" %in% mfoi_lines_split){
          module_name = mfoi_lines_split[2]
        }
        # avoid duplicate process directives ... causess errors 
        if(mfoi_lines_split[1] %in% process_directives){
          #print(process_directive_count)
          rlog::log_info(paste("FOUND_PROCESS_DIRECTIVE_OF_INTEREST",mfoi_lines_split[1]))
          if(mfoi_lines_split[1] == "pod"){
            rlog::log_info(paste("POD_ANNOTATION_LINE:",paste(mfoi_lines_split,collapse="_",sep="_")))
            ### add specific note to only coount pod annotation
            rlog::log_info(paste("IS_IT_POD_ANNOTATION:","annotation:"  %in% mfoi_lines_split))
            if("annotation:"  %in% mfoi_lines_split){
              process_directive_count[[mfoi_lines_split[1]]]  = process_directive_count[[mfoi_lines_split[1]]] + 1
            }
          } else{
              process_directive_count[[mfoi_lines_split[1]]]  = process_directive_count[[mfoi_lines_split[1]]]  + 1
          }
          if(process_directive_count[[mfoi_lines_split[1]]]  > 1){
            add_line = FALSE
          }
        } 
      } 
      if(add_line){
        mfoi_lines1 = c(mfoi_lines1,mfoi_lines[j]) 
      } else{
        rlog::log_info(paste("NOT_ADDING_LINE:",mfoi_lines[j]))
      }

    }
    module_lines1 = mfoi_lines1
    path_split = strsplit(mfoi,"/")[[1]]
    is_workflow_or_subworkflow = FALSE
    if("workflows" %in% path_split || "subworkflows" %in% path_split){
      is_workflow_or_subworkflow = TRUE
    }
    if(!is_workflow_or_subworkflow){
      module_lines1 = addPublishStatement(mfoi_lines1,getCustomOutdirName(module_name))
    } else{
      rlog::log_info(paste("addPublishStatement:SKIPPING PUBLISH_DIR STATEMENT FOR:",nf_script))
    }
    if(grepl("\\}",module_lines1[length(module_lines1)]) && grepl("\\}",module_lines1[length(module_lines1)-1]) ){
      module_lines1 = module_lines1[1:(length(module_lines1)-1)]
    }
    ###########################################
    ### update mem and cpu where needed. --- do I see task.cpus or task.mem
    declaration_list = needsTaskCpuOrMemDeclaration(module_lines1)
    ### ica_instance_table, pod annotation line and grab CPU and Mem
    pod_annotation = grabPodAnnotation(module_lines1)
    ### convert CPU and Mem values to nextflow compatible strings
    declaration_lines = generateMemOrCPUdeclarations(declaration_list,pod_annotation,ica_instance_table)
    rlog::log_info(paste("ADDIING_DECLARATIVES:",declaration_lines))
    ### inject into module if needed
    module_lines1 = updateProcessDeclaratives(module_lines1,declaration_lines)
    #########
    #if(paste(module_lines1,collapse="\n") != paste(module_lines,collapse="\n")){
    updated_module_script = gsub(".nf$",".dev.nf",mfoi)
    print(module_lines1)
    write.table(x=module_lines1,file=updated_module_script,sep="\n",quote=F,row.names=F,col.names=F)
    rlog::log_info(paste("Generated final updated module script to:",updated_module_script))
    system(paste("cp",updated_module_script,mfoi))
  }
}
#################

## remove vestigal publishDir lines
if(length(workflow_files) > 0){
  for(idx in 1:length(workflow_files)){
    wfoi = workflow_files[idx]
    wfoi_lines = t(read.delim(wfoi,header=F,quote=""))
    wfoi_lines1 = wfoi_lines
    publishDir_statement_or_not = apply(t(wfoi_lines),2,function(x) grepl("publishDir",x))
    wfoi_lines1 = wfoi_lines1[!publishDir_statement_or_not]
    updated_workflow_file = gsub(".nf$",".dev.nf",workflow_files[idx])
    write.table(x=wfoi_lines1,file=updated_workflow_file,sep="\n",quote=F,row.names=F,col.names=F)
    rlog::log_info(paste("Generated final updated workflow script to:",updated_workflow_file))
    system(paste("cp",updated_workflow_file,wfoi))
  }
}
##########################################
getParamsTokens <- function(my_line){
  params_tokens = c()
  line_tokens = strsplit(my_line,"\\s+")[[1]]
  #line_tokens = apply(t(line_tokens),2,trimws)
  line_tokens = line_tokens[line_tokens!=""]
  if(length(line_tokens) > 0){
    for(i in 1:length(line_tokens)){
      line_tokens[i] = gsub(",","",line_tokens[i])
      if(grepl("^params\\.",line_tokens[i])){
        rlog::log_info(paste("getParamsTokens:ADDING_TOKEN:",line_tokens[i]))
        params_tokens = c(params_tokens,line_tokens[i])
      }
    }
  }
  params_tokens = params_tokens[!is.null(params_tokens)]
  return(unique(params_tokens))
}
obtainDefaultValues <- function(main_script,tokens_of_interest){
  found_configuration = list()
  for(k in 1:length(tokens_of_interest)){
    found_configuration[[tokens_of_interest[k]]] = FALSE
  }
  #default_configuration_lines = c("params = [:]")
  default_configuration_lines = c()
  main_script_dat = t(read.delim(main_script,header=F,quote=""))
  for(i in 1:length(main_script_dat)){
    main_script_line = main_script_dat[i]
    main_script_line_split = strsplit(main_script_line,"\\s+")[[1]]
    main_script_line_split = apply(t(main_script_line_split),2,trimws)
    main_script_line_split = main_script_line_split[main_script_line_split!=""]
    for(j in 1:length(tokens_of_interest)){
      if(tokens_of_interest[j] == main_script_line_split[1]){
        if(!found_configuration[[tokens_of_interest[j]]]){
          default_configuration_lines = c(default_configuration_lines,main_script_dat[i])
          found_configuration[[tokens_of_interest[j]]] = TRUE
        }
      }
    }
  }
  return(default_configuration_lines)
}
## add params = [:] amd params.{variable_name} to subworkflow file
if(length(subworkflow_files) > 0){
  for(idx in 1:length(subworkflow_files)){
    swfoi = subworkflow_files[idx]
    swfoi_lines = t(read.delim(swfoi,header=F,quote=""))
    swfoi_lines_of_interest = swfoi_lines[apply(t(swfoi_lines),2,function(x) grepl("params\\.",x) )]
    ## find params, add params of interest to top of file
    tokens_of_interest = apply(t(swfoi_lines_of_interest),2,getParamsTokens)
    tokens_of_interest = unique(unlist(tokens_of_interest))
    tokens_of_interest = tokens_of_interest[!is.null(tokens_of_interest)]
    lines_to_add = c()
    rlog::log_info(paste("LENGTH_OF_TOKENS:",length(tokens_of_interest)))
    if(length(tokens_of_interest) > 0){
      rlog::log_info(paste("TOKENS_OF_INTEREST:",paste(tokens_of_interest,sep=", ",collapse=", ")))
      rlog::log_info(paste("READING_IN_MAIN_NF_SCRIPT:",updated_nf_file))
      lines_to_add = obtainDefaultValues(updated_nf_file,tokens_of_interest)
      lines_to_add = c(lines_to_add ,obtainDefaultValues(config_file,tokens_of_interest))
      if(length(lines_to_add) >0){
        lines_to_add = c("params = [:]","process = [:]", "process.ext = [:]",lines_to_add)
      }
    }
    if(length(lines_to_add) > 0){
      swfoi_lines1 = c(lines_to_add,swfoi_lines)
    } else{
      swfoi_lines1 = swfoi_lines
    }
    #############################################
    updated_workflow_file = gsub(".nf$",".dev.nf",swfoi)
    write.table(x=swfoi_lines1,file=updated_workflow_file,sep="\n",quote=F,row.names=F,col.names=F)
    rlog::log_info(paste("Generated final updated workflow script to:",updated_workflow_file))
    system(paste("cp",updated_workflow_file,swfoi))
  }
}

#STEP4: Check for file_paths based-off the parameter XML file to ensure that they are
# wrapped by file(myChannel1)
