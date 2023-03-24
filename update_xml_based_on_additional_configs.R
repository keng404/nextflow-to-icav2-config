options(stringsAsFactors=FALSE)
suppressPackageStartupMessages(library("argparse"))
library(rlog)
library(stringr)
library(rvest)
# create parser object
parser <- ArgumentParser()
parser$add_argument("-c","--config-file","--config_file", default=NULL,required=TRUE,
                    help = "main config file")
parser$add_argument("-k","--is-simple-config","--is_simple_config",
                    action="store_true",default=FALSE, help = "Use config")
parser$add_argument("-i","--configs-to-ignore","--configs_to_ignore", default=c(""),nargs="?",
                    action="append",help="config files to ignore")
parser$add_argument("-x","--parameters-xml","--parameters_xml",
                    default=NULL, help = " parameters XML file output")
# get command line options, if help option encountered print help and exit,
# otherwise if options not found on command line then set defaults, 
args <- parser$parse_args()
is_simple_config = args$is_simple_config
config_file = args$config_file
config_dat  = read.delim(config_file,quote="",header=F)
configs_to_ignore = args$configs_to_ignore
configs_to_ignore = configs_to_ignore[configs_to_ignore!=""]
parameters_xml = args$parameters_xml
######################################
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
##########################################
paramsFiller <- function(list_to_fill,params_list){
  params_list_updated = list_to_fill
  for(i in 1:length(list_to_fill)){
    result = str_extract(list_to_fill[i], "(?<=\\{)[^\\}]+")
    # maybe try a while loop?
    if(!is.na(result)){
      # fill in appropriate params vaiue
      updated_value = gsub(result,gsub("'","",params_list[[result]]),list_to_fill[i])
      # remove ${ and } from original string
      updated_value = sub("\\$\\{","",updated_value)
      updated_value = sub("\\}","",updated_value)
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
      rlog::log_info(paste("CONFIG_LINE_OF_INTEREST:",conf_data[i,]))
      lines_to_keep = c(lines_to_keep,conf_data[i,])
    }
  }
  configs = c()
  if(length(lines_to_keep) > 0){
    for(j in 1:length(lines_to_keep)){
      if(lines_to_keep[j] != ""){
        #lines_to_keep[j] = gsub("\\{"," ",lines_to_keep[j])
        #lines_to_keep[j] = gsub("\\}"," ",lines_to_keep[j])
        line_parsed = strsplit(lines_to_keep[j],"\\s+")[[1]]
        line_parsed = line_parsed[line_parsed != ""]
        line_parsed = line_parsed[!grepl("includeConfig",line_parsed)]
        line_parsed = line_parsed[length(line_parsed)]
        if(!(grepl("\\{",line_parsed))){
          line_parsed = paste(dirname(conf_path),line_parsed,sep="/")
          if(!grepl("params",line_parsed)){
            rlog::log_info(paste("HELLO:",line_parsed))
            configs = c(configs,gsub("\'","",line_parsed))
          }
        } else{
          #if(!grepl("params",line_parsed)){
            rlog::log_info(paste("HELLO:",line_parsed))
            configs = c(configs,gsub("\'","",line_parsed))
          #}
        }
      }
    }
  }
  return(configs)
}

localConfigOrNot <- function(config_list){
  keep_array = c()
  for(i in 1:length(config_list)){
    keep_array[i] = (grepl("http",config_list[i])) | ( file.exists(config_list[i]) & basename(config_list[i]) == "igenomes.config" )
  }
  return(config_list[keep_array])
}

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
configs_to_ignore = c()
rlog::log_info(paste("CONFIGS_TO_IGNORE:",configs_to_ignore))
if(length(configs_to_ignore) > 0 && !is.null(z1)){
  z1 = z1[ !(z1 %in% configs_to_ignore)]
}
rlog::log_info(paste("CONFIGS_FOUND:",z1,collapse=","))
final_config_list = c()
if(length(z1) > 0){
  params_to_fill = sum(apply(t(z1),2,function(x) grepl("\\$\\{",x)))
  ########################
  jailbreak_counter = 0
  jailbreak_threshold = 10
  ###########################
  while(params_to_fill > 0 & jailbreak_counter < jailbreak_threshold ){
    z1 = paramsFiller(list_to_fill=z1,params_list=z)
    params_to_fill = sum(apply(t(z1),2,function(x) grepl("\\$\\{",x)))
    jailbreak_counter = jailbreak_counter + 1
  }
  final_config_list = localConfigOrNot(paramsFiller(list_to_fill=z1,params_list=z))
} else{
  final_config_list = NULL
  stop(paste("No additional config files found"))
}
#######################
find_other_configs <- function(config_url){
  test_doc = rvest::read_html(config_url,encoding = "ISO-8859-1")
  test_content = strsplit(rvest::html_text(test_doc),"\n")[[1]]
  test_content = as.matrix(test_content)
  lines_to_check = c()
  additional_configs = c()
  for(i in 1:nrow(test_content)){
    clean_line = strsplit(test_content[i,],"\\s+")[[1]]
    clean_line = clean_line[clean_line!=""]
    if(length(clean_line) > 0){
      if(clean_line[1] == "includeConfig"){
        line_to_check = paste(clean_line[2:length(clean_line)],collapse = " ",sep = " ")
        lines_to_check = c(lines_to_check,line_to_check)
      }
    }
  }
  if(length(lines_to_check) > 0){
    params_to_fill = sum(apply(t(lines_to_check),2,function(x) grepl("\\$\\{",x)))
    ########################
    jailbreak_counter = 0
    jailbreak_threshold = 10
    ###########################
    while(params_to_fill > 0 & jailbreak_counter < jailbreak_threshold ){
      lines_to_check = paramsFiller(list_to_fill=lines_to_check,params_list=z)
      params_to_fill = sum(apply(t(lines_to_check),2,function(x) grepl("\\$\\{",x)))
      jailbreak_counter = jailbreak_counter + 1
    }
    additional_configs = localConfigOrNot(paramsFiller(list_to_fill=lines_to_check,params_list=z))
  }
  return(additional_configs)
}

#########
valid_url <- function(url_in,t=2){
  con <- url(url_in)
  check <- suppressWarnings(try(open.connection(con,open="rt",timeout=t),silent=T)[1])
  suppressWarnings(try(close.connection(con),silent=T))
  ifelse(is.null(check),TRUE,FALSE)
}
#####################
if(length(final_config_list) > 0 ){
  final_config_list1 = c()
  for(config_idx in 1:length(final_config_list)){
    rlog::log_info(paste("LOOKING at:",final_config_list[config_idx]))
    is_valid_url = FALSE
    if(grepl("http",final_config_list[config_idx]) | grepl("ftp",final_config_list[config_idx])){
      is_valid_url = valid_url(url_in=c(final_config_list[config_idx]))
    } else{
      final_config_list1 = c(final_config_list1,final_config_list[config_idx])
    }
    if(is_valid_url){
      additional_configs = find_other_configs(final_config_list[config_idx])
      if(length(additional_configs) > 0){
        final_config_list1 = c(final_config_list1,additional_configs)
      } else{
        final_config_list1 = c(final_config_list1,final_config_list[config_idx])
      }
    } else{
      rlog::log_warn(paste(final_config_list[config_idx],"may not be valid URL"))
    }
  }
  final_config_list = final_config_list1
  if(length(final_config_list) > 0){
    for(config_idx in 1:length(final_config_list)){
      config_of_interest = final_config_list[config_idx]
      xml_update_command = paste("Rscript","test_config_parser.R")
      if(grepl("^http",config_of_interest)){
        xml_update_command = paste(xml_update_command,"--config-url",config_of_interest,"--parameters-xml",parameters_xml)
      } else if(file.exists(config_of_interest)){
        xml_update_command = paste(xml_update_command,"--config-file",config_of_interest,"--parameters-xml",parameters_xml)
      } else{
        rlog::log_error(paste("Don't know what to do with:",config_of_interest,"\nAre you sure it exists as a URL or local file?"))
      }
      xml_update_command_run = paste(xml_update_command)
      rlog::log_info(paste("RUNNING:",xml_update_command))
      system(xml_update_command_run)
    }
  } else{
    rlog::log_info(paste("No additional XML configurations added based on :",config_file))
  }
} else{
  rlog::log_info(paste("No additional XML configurations added based on :",config_file))
}

