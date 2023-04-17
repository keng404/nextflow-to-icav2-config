options(stringsAsFactors=FALSE)
suppressPackageStartupMessages(library("argparse"))
library(rlog)
library(stringr)
get_params_from_config <- function(conf_file,conf_data = NULL){
  if(is.null(conf_data)){
    conf_data = read.delim(conf_file,quote="",header=F)
  }
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
      sanitized_token = gsub("'","\"",sanitized_token)
      clean_line[t] = sanitized_token
    }
    clean_line = clean_line[clean_line!=""]
    line_skip = FALSE
    if(length(clean_line) ==0){
      next
    }
    if(grepl("/",clean_line[1])){
      line_skip = TRUE
    } 
    if(!line_skip && grepl("\\{",conf_data[i,]) && "params" %in% clean_line && !grepl("def",conf_data[i,]) && !grepl("if",conf_data[i,]) && !grepl("else",conf_data[i,]) && !grepl("\\(params",conf_data[i,])  && !grepl("\\{params",conf_data[i,]) && !grepl("\\$\\{",conf_data[i,])){
      rlog::log_info(paste("ENTERING_PARAMS_ENCLOSURE:",conf_data[i,]))
      in_params_closure = TRUE
    } else if(!line_skip & grepl("=",conf_data[i,]) & !grepl("\\*",clean_line[1]) & !grepl("\\/\\/",clean_line[1])){
      rlog::log_info(paste("NOT_CHANGING_STATUS:",conf_data[i,]))
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
      } else if(!line_skip & grepl("=",conf_data[i,]) & !grepl("\\*",clean_line[1]) & !grepl("\\/\\/",clean_line[1])){
        rlog::log_info(paste("ADDING_LINE2:",conf_data[i,]))
        lines_to_keep = c(lines_to_keep,conf_data[i,])
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
              nested_param_key_split = strsplit(nested_param_key,"\\{")[[1]]
              nested_param_key = nested_param_key[!grepl("\\{",nested_param_key)]
              if(length(nested_param_key) == 0){
                nested_param_key = trimws(nested_param_key_split[1])
              }
              rlog::log_info(paste("initial_nested_param:",initial_nested_param,"\t","nested_param_key:",nested_param_key))
              rlog::log_info(paste("RESCUE_LINE2:",conf_data[i,]))
              lines_to_keep = c(lines_to_keep,conf_data[i,])
            } else{
              if(in_params_closure){
                rlog::log_info(paste("RESCUE_LINE:",conf_data[i,]))
                lines_to_keep = c(lines_to_keep,conf_data[i,])
              } else{
                rlog::log_info(paste("IGNORE_PARAM_LINE:",conf_data[i,]))
              }
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
      line_parsed_key_split = strsplit(line_parsed[1],"\\.")[[1]]
      # remove params
      line_parsed_key_split = line_parsed_key_split[line_parsed_key_split !="params"]
      line_parsed_key_split = paste(line_parsed_key_split,collapse=".")
      rlog::log_info(paste("Potential_Value:",paste(line_parsed[3:length(line_parsed)],sep=" ",collapse=" ")))
      rlog::log_info(paste("line_parsed_length:",length(line_parsed)))
      if(!grepl("=",line_parsed_key_split)){
        if(length(line_parsed) >= 3){
          params[[paste("params.",line_parsed_key_split,sep="")]] = paste(line_parsed[3:length(line_parsed)],sep=" ",collapse=" ")
        } else{
          rlog::log_warn(paste("Skipping key:",paste("params.",line_parsed_key_split,sep=""),"skipping value:",paste(line_parsed[3:length(line_parsed)],sep=" ",collapse=" ")))
          next
        }
        # initialize maps if necessary
        subkeys = stringr::str_extract_all( line_parsed_key_split,"(?<=\\[).*(?=\\])")[[1]]
        majorkeys = gsub("\\["," ",line_parsed_key_split)[[1]]
        majorkeys = strsplit(majorkeys,"\\s+")[[1]]
        majorkeys_bool = apply(t(majorkeys),2,function(x) !grepl("\\]",x))
        majorkeys = majorkeys[majorkeys_bool]
        majorkeys = majorkeys[!is.na(majorkeys)]
        subkeys = subkeys[!is.na(subkeys)]
        if(length(subkeys)>0){
          key_collection = c()
          for(sks in 1:length(subkeys)){
            key_prefix = "params."
            key_collection = c(key_collection,paste("[",subkeys[sks],"]",sep="",collapse=""))
            key_to_check = paste(key_prefix,key_collection,sep="",collapse="")
            key_to_check_stripped = sub("^params.","",key_to_check)
            if(!key_to_check %in% names(params) & !grepl("^\\[",key_to_check_stripped) & !grepl("^'",key_to_check_stripped)){
              params[[key_to_check]] = "[:]"
            } else{
              rlog::log_info(paste("Key:",key_to_check,"is already in params"))
            }
          }
        }
        if(length(majorkeys)>0 & length(subkeys) > 0){
          key_collection = c()
          for(sks in 1:length(majorkeys)){
            key_prefix = "params."
            key_collection = c(key_collection,paste(majorkeys[sks],sep="",collapse=""))
            key_to_check = paste(key_prefix,key_collection,sep="",collapse="")
            key_to_check_stripped = sub("^params.","",key_to_check)
            if(!key_to_check %in% names(params) & !grepl("^\\[",key_to_check_stripped) & !grepl("^'",key_to_check_stripped)){
              params[[key_to_check]] = "[:]"
            } else{
              rlog::log_info(paste("Key:",key_to_check,"is already in params"))
            }
          }
          
          for(sks in 1:length(majorkeys)){
            for(skss in 1:length(subkeys)){
              key_collection = c()
              key_prefix = "params."
              key_collection = c(key_collection,paste(majorkeys[sks],sep="",collapse=""))
              key_collection = c(key_collection,paste("[",subkeys[skss],"]",sep="",collapse=""))
              key_to_check = paste(key_prefix,paste(key_collection,sep="",collapse=""),sep="",collapse="")
              key_to_check_stripped = sub("^params.","",key_to_check)
              if(!key_to_check %in% names(params) & !grepl("^\\[",key_to_check_stripped) & !grepl("^'",key_to_check_stripped)){
                params[[key_to_check]] = "[:]"
              } else{
                rlog::log_info(paste("Key:",key_to_check,"is already in params"))
              }
            }
          }
        }
      } else{
        another_split = strsplit(line_parsed_key_split,"\\=")[[1]]
        print(another_split)
        idx_of_interest = 1
        idxs_of_interest = c(idx_of_interest)
        if(sum(another_split=="") > 0 ){
          idxs_of_interest = (1:length(another_split))[another_split==""]
          idx_of_interest = idxs_of_interest[length(idxs_of_interest)]
        }
        if(idxs_of_interest[1]-1 > 0){
          new_key =  paste(another_split[1:(idxs_of_interest[1]-1)],sep=".",collapse = "")
        } else{
          new_key =  paste(another_split[1],sep=".",collapse = "")
        }
        params[[paste("params.",new_key,sep="")]] = trimws(another_split[idx_of_interest + 1])
        # initialize maps if necessary
        subkeys = stringr::str_extract_all( new_key,"(?<=\\[).*(?=\\])")[[1]]
        majorkeys = gsub("\\["," ",new_key)[[1]]
        majorkeys = strsplit(majorkeys,"\\s+")[[1]]
        majorkeys_bool = apply(t(majorkeys),2,function(x) !grepl("\\]",x))
        majorkeys = majorkeys[majorkeys_bool]
        majorkeys = majorkeys[!is.na(majorkeys)]
        subkeys = subkeys[!is.na(subkeys)]
        if(length(subkeys)>0){
          key_collection = c()
          for(sks in 1:length(subkeys)){
            key_prefix = "params."
            key_collection = c(key_collection,paste("[",subkeys[sks],"]",sep="",collapse=""))
            key_to_check = paste(key_prefix,key_collection,sep="",collapse="")
            key_to_check_stripped = sub("^params.","",key_to_check)
            if(!key_to_check %in% names(params) & !grepl("^\\[",key_to_check_stripped) & !grepl("^'",key_to_check_stripped)){
              params[[key_to_check]] = "[:]"
            } else{
              rlog::log_info(paste("Key:",key_to_check,"is already in params"))
            }
          }
        }
        if(length(majorkeys)>0 & length(subkeys) > 0){
          key_collection = c()
          for(sks in 1:length(majorkeys)){
            key_prefix = "params."
            key_collection = c(key_collection,paste(majorkeys[sks],sep="",collapse=""))
            key_to_check = paste(key_prefix,key_collection,sep="",collapse="")
            key_to_check_stripped = sub("^params.","",key_to_check)
            if(!key_to_check %in% names(params) & !grepl("^\\[",key_to_check_stripped) & !grepl("^'",key_to_check_stripped)){
              params[[key_to_check]] = "[:]"
            } else{
              rlog::log_info(paste("Key:",key_to_check,"is already in params"))
            }
          }
          for(sks in 1:length(majorkeys)){
            for(skss in 1:length(subkeys)){
              key_collection = c()
              key_prefix = "params."
              key_collection = c(key_collection,paste(majorkeys[sks],sep="",collapse=""))
              key_collection = c(key_collection,paste("[",subkeys[skss],"]",sep="",collapse=""))
              key_to_check = paste(key_prefix,paste(key_collection,sep="",collapse=""),sep="",collapse="")
              key_to_check_stripped = sub("^params.","",key_to_check)
              if(!key_to_check %in% names(params) & !grepl("^\\[",key_to_check_stripped) & !grepl("^'",key_to_check_stripped)){
                params[[key_to_check]] = "[:]"
              } else{
                rlog::log_info(paste("Key:",key_to_check,"is already in params"))
              }
            }
          }
        }
        
      }
      # params[[paste("params.",line_parsed[1],sep="")]] = gsub("\'","",line_parsed[3])
    }
  }
  return(params)
}

get_other_lines_from_config <- function(conf_file){
  conf_data = read.delim(conf_file,quote="",header=F)
  config_parsed_lines = list()
  statement_prefixes  = c('if','when','def','else','else if','profiles')
  
  ## identify lines referring to params defined in config file
  lines_to_keep = c()
  lines_to_migrate = c()
  statement_left_brackets = c()
  statement_right_brackets = c()
  in_expression = FALSE
  in_comment_block = FALSE
  line_skip = FALSE
  in_params_closure = FALSE
  in_closure = FALSE
  out_closure = TRUE
  out_params_closure = TRUE
  initial_nested_param = NA
  nested_param_key = NA
  # 'manifest'
  custom_params_to_ignore = c('report','timeline','trace','dag','env')
  custom_params_to_ignore_closure = FALSE
  custom_params_to_migrate = c('manifest')
  in_manifest_closure = FALSE
  for(i in 1:nrow(conf_data)){
    line_split = strsplit(conf_data[i,],"\\s+")[[1]]
    clean_line = line_split
    for(t in 1:length(line_split)){
      sanitized_token = trimws(line_split[t])
      sanitized_token = gsub("'","\"",sanitized_token)
      clean_line[t] = sanitized_token
    }
    clean_line = clean_line[clean_line!=""]
    line_skip = FALSE
    rlog::log_info(paste(c("CLEAN_LINE:",clean_line),collapse = " ",sep = " "))
    if(grepl("/",clean_line[1])){
      line_skip = TRUE
    } 
    #########
    if(length(clean_line) == 0){
      rlog::log_info(paste(c("SKIPPING_LINE:",clean_line),collapse = " ",sep = " "))
      next
    }
    #for(t in 1:length(clean_line)){
    for(t in 1:1){
      if(grepl("\\/\\*",clean_line[t])){
     ## if(grepl("/*",clean_line[t])){
        ##line_skip = TRUE
        rlog::log_info(paste("FOUND comment token:",clean_line[t]))
        in_comment_block = TRUE
      } 
      if(grepl("\\*\\/",clean_line[t])){
      ##if(grepl("*/",clean_line[t])){
        rlog::log_info(paste("FOUND comment token:",clean_line[t]))
        in_comment_block = FALSE
      }
    } 
    ###rlog::log_info(paste(line_skip,in_comment_block,in_params_closure,in_closure,out_closure,out_params_closure,initial_nested_param,nested_param_key))
    ##############
    if(!line_skip && grepl("\\{",conf_data[i,]) && "params" %in% clean_line && !grepl("def",conf_data[i,]) && !grepl("if",conf_data[i,]) && !grepl("else",conf_data[i,]) && !grepl("\\(params",conf_data[i,])  && !grepl("\\{params",conf_data[i,]) && !grepl("\\$\\{",conf_data[i,])){
      rlog::log_info(paste("ENTERING_PARAMS_ENCLOSURE:",conf_data[i,]))
      in_params_closure = TRUE
    } else if(!line_skip & grepl("=",conf_data[i,]) & !grepl("\\*",clean_line[1]) & !grepl("\\/\\/",clean_line[1])){
      rlog::log_info(paste("NOT_CHANGING_STATUS:",conf_data[i,]))
    } else if(!line_skip && grepl("\\{",conf_data[i,]) && !grepl("\\$\\{",conf_data[i,]) && !grepl("params",conf_data[i,])){
      in_closure = TRUE
      out_closure = FALSE
      #if((clean_line[1] %in% custom_params_to_ignore) & grepl("\\{",conf_data[i,])){
      # custom_params_to_ignore_closure = TRUE
      #} else if((clean_line[1] %in% custom_params_to_migrate) & grepl("\\{",conf_data[i,])){
      # in_manifest_closure = TRUE
      #} else{
      #  custom_params_to_ignore_closure = TRUE
      #}
    } else if(!line_skip && grepl("\\}",conf_data[i,]) && !grepl("\\$\\{",conf_data[i,]) &&  !grepl("params",conf_data[i,]) && in_closure == TRUE){
      in_closure = FALSE
      out_closure = TRUE
      #if(custom_params_to_ignore_closure & grepl("\\}",conf_data[i,])){
      #  custom_params_to_ignore_closure = FALSE
      #  next
      #} else if(in_manifest_closure & grepl("\\}",conf_data[i,])){
      #  in_manifest_closure = FALSE
      #  next
      #}
    } else if(!line_skip && in_params_closure && grepl("\\}",conf_data[i,]) && !grepl("\\$\\{",conf_data[i,]) && in_closure ==FALSE ){
      ### initial check each line to see if we've exited the params closure
      out_params_closure = TRUE
      in_params_closure = FALSE
      rlog::log_info(paste("EXITED_PARAMS_ENCLOSURE:",conf_data[i,]))
      next
    } else{
      rlog::log_info(paste("NOT_CHANGING_STATUS:",conf_data[i,]))
    }
    rlog::log_info(paste("LINE_OF_INTEREST:",conf_data[i,],"LINE_SKIP:",line_skip,"OUT_PARAMS_ENCLOSURE:",out_params_closure,"PARAMS_ENCLOSURE:",in_params_closure,"IN_OTHER_CLOSURE:",in_closure,"IN_COMMENT_BLOCK:",in_comment_block))
    
    ### grab parameters in params enclosure
    if(!line_skip & out_params_closure & !in_params_closure & !in_comment_block){
      ##rlog::log_info(paste(line_skip,in_comment_block,in_params_closure,in_closure,out_closure,out_params_closure,initial_nested_param,nested_param_key))
      if(!custom_params_to_ignore_closure & !in_manifest_closure){
        lines_to_keep = c(lines_to_keep,conf_data[i,])
      } else if(!custom_params_to_ignore_closure & in_manifest_closure){
        # hacky override to allow for ICA compatible nextflow version

        lines_to_migrate = c(lines_to_migrate,conf_data[i,])
  
      } else{
        rlog::log_warn((paste("NOT KEEPING LINE:",conf_data[i,])))
      }
    } else if(grepl("process",conf_data[i,]) & grepl("container",conf_data[i,])){
      rlog::log_info(paste("RESCUE_LINE1:",conf_data[i,]))
      lines_to_keep = c(lines_to_keep,conf_data[i,])
    } else if(grepl("includeConfig",conf_data[i,]) & !grepl("test",conf_data[i,]) & (grepl("base",conf_data[i,]) | grepl("genome",conf_data[i,])) ){
      rlog::log_info(paste("RESCUE_LINE2:",conf_data[i,]))
      lines_to_keep = c(lines_to_keep,conf_data[i,])
    }
  }
  config_parsed_lines[["lines_to_keep"]] =  lines_to_keep
  config_parsed_lines[["lines_to_migrate"]] = lines_to_migrate
  return(config_parsed_lines)
}

second_pass_config_other_lines <- function(conf_other_lines){
  second_pass_lines = list()
  statement_prefixes  = c('if','when','def','else','else if')
  nested_params_to_ignore = c('profiles')
  statement_left_brackets = c()
  statement_right_brackets = c()
  lines_to_keep = c()
  lines_to_migrate = c()
  config_lines_rescued = c()
  # 'manifest'
  custom_params_to_ignore = c('report','timeline','trace','dag','env')
  custom_params_to_ignore_closure = FALSE
  custom_params_to_migrate = c('manifest')
  in_manifest_closure = FALSE
  in_expression = FALSE
  in_nested_expression_to_ignore = FALSE
  previous_line_status = list()
  previous_line_status[["custom_params_to_ignore_closure"]] = custom_params_to_ignore_closure
  previous_line_status[["in_manifest_closure"]] = in_manifest_closure
  previous_line_status[["in_nested_expression_to_ignore"]] = in_nested_expression_to_ignore
  for(i in 1:length(conf_other_lines)){
    rlog::log_info(paste("SECOND_PASS ON CONFIG LINE:",conf_other_lines[i]))
    if(length(conf_other_lines[i])>0){
      line_split = strsplit(conf_other_lines[i],"\\s+")[[1]]
      clean_line = line_split
    } else{
        next
    }
    for(t in 1:length(line_split)){
      sanitized_token = trimws(line_split[t])
      sanitized_token = gsub("'","\"",sanitized_token)
      clean_line[t] = sanitized_token
    }
    clean_line = clean_line[clean_line!=""]
    line_skip = FALSE
    rlog::log_info(paste(c("CLEAN_LINE:",clean_line),collapse = " ",sep = " "))

    if(length(clean_line)<1){
      line_skip = TRUE
    }
    if((clean_line[1] %in% custom_params_to_ignore) & grepl("\\{",conf_other_lines[i])){
     custom_params_to_ignore_closure = TRUE
    } else if((clean_line[1] %in% custom_params_to_migrate) & grepl("\\{",conf_other_lines[i])){
     in_manifest_closure = TRUE
    } else if(custom_params_to_ignore_closure & grepl("\\}",conf_other_lines[i])){
      custom_params_to_ignore_closure = FALSE
      next
    } else if(in_manifest_closure & grepl("\\}",conf_other_lines[i])){
      in_manifest_closure = FALSE
      next
    } else if( clean_line[1] %in% nested_params_to_ignore){
        rlog::log_info(paste("IN_NESTED_EXRESSION_TO_IGNORE:",paste(clean_line,collapse=" ")))
        in_nested_expression_to_ignore = TRUE
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
            in_nested_expression_to_ignore = FALSE
          statement_left_brackets = c()
          statement_right_brackets = c()
          rlog::log_info(paste("EXITING_EXPRESSION"))
          next
        }
      }
    } else if(in_nested_expression_to_ignore){
      right_brackets_to_add = unlist(str_extract_all(clean_line, "\\}"))
      if(length(right_brackets_to_add) >0){
        statement_right_brackets = c(statement_right_brackets,right_brackets_to_add)
      }
      left_brackets_to_add = unlist(str_extract_all(clean_line, "\\{"))
      if(length(left_brackets_to_add) >0){
        statement_left_brackets = c(statement_left_brackets,left_brackets_to_add)
      }
      ### check if we've exited the expression
      # if(clean_line[length(clean_line)] == "}" || grepl("\\{$",clean_line[length(clean_line)])){
      if(length(statement_right_brackets) >0 || length(statement_left_brackets) > 0){
        rlog::log_info(paste("right brackets:",paste(statement_right_brackets,collapse=", ")))
        rlog::log_info(paste("left brackets:",paste(statement_left_brackets,collapse=", ")))
        if(length(statement_right_brackets) == length(statement_left_brackets)){
          if(clean_line[1] %in% statement_prefixes){
            in_expression = FALSE
          } else{
            in_nested_expression_to_ignore = FALSE
          }
          statement_left_brackets = c()
          statement_right_brackets = c()
          rlog::log_info(paste("EXITING_EXPRESSION"))
          next
        }
      }
    }
    rlog::log_info(paste(c("custom_params_to_ignore_closure:",custom_params_to_ignore_closure),collapse = " ",sep = " "))
    rlog::log_info(paste(c("in_manifest_closure:",in_manifest_closure),collapse = " ",sep = " "))
    rlog::log_info(paste(c("in_nested_expression_to_ignore:",in_nested_expression_to_ignore),collapse = " ",sep = " "))
    #### add lines back based on critierion
    if(!in_manifest_closure & ! custom_params_to_ignore_closure & !in_nested_expression_to_ignore){
      if(!previous_line_status[["in_manifest_closure"]] & ! previous_line_status[["custom_params_to_ignore_closure"]] & !previous_line_status[["in_nested_expression_to_ignore"]]){
        rlog::log_info(paste("LINE TO KEEP:",conf_other_lines[i]))
        lines_to_keep = c(lines_to_keep,conf_other_lines[i])
      } else {
        if(length(clean_line) > 0){
          if(!grepl("\\{",clean_line[1]) & !grepl("\\}",clean_line[1])){
            rlog::log_info(paste("LINE TO KEEP - 2nd condition -:",conf_other_lines[i]))
            lines_to_keep = c(lines_to_keep,conf_other_lines[i])
          }
        }
      }
    } else if(in_manifest_closure & ! custom_params_to_ignore_closure & !in_nested_expression_to_ignore){
      rlog::log_info(paste("LINE TO MIGRATE:",conf_other_lines[i]))
      
      ### override nextflow version to an ICA compatible nextflow version
      if(grepl("nextflowVersion",conf_other_lines[i])){
        rlog::log_info(paste("NXF_VERSION POTENTIAL_OVERRIDE:",conf_other_lines[i]))
        update_line = conf_other_lines[i]
        update_line_split = strsplit(update_line,"\\=")[[1]]
        update_line_split = update_line_split[update_line_split != ""]
        update_line_split = strsplit(update_line_split[length(update_line_split)],"\\=")[[1]]
        rlog::log_info(paste(update_line_split))
        update_line_split[1] = gsub("'","",update_line_split[1])
        version_split = strsplit(update_line_split[1],"\\.")[[1]]
        rlog::log_info(paste("nextflow version:",paste(version_split,sep=".",paste=".")))
        if(strtoi(version_split[1]) > 21){
          update_line = sub(version_split[1],"21",update_line)
        }
        rlog::log_info(paste("LINE TO MIGRATE_UPDATED:",update_line))
        lines_to_migrate = c(lines_to_migrate,update_line)
      } else{
        lines_to_migrate = c(lines_to_migrate,conf_other_lines[i])
      }
    } else if(in_expression){
      rlog::log_info(paste("FOUND EXPRESSION LINE:",conf_other_lines[i]))
      lines_to_keep = c(lines_to_keep,conf_other_lines[i])
    } else if(grepl("includeConfig",conf_other_lines[i]) & !grepl("test",conf_other_lines[i]) & (grepl("base",conf_other_lines[i]) | grepl("genome",conf_other_lines[i])) ){
      rlog::log_info(paste("RESCUE_LINE2:",conf_other_lines[i]))
      config_lines_rescued = c(config_lines_rescued,paste(clean_line,collapse =  " "))
    }
    
    previous_line_status[["custom_params_to_ignore_closure"]] = custom_params_to_ignore_closure
    previous_line_status[["in_manifest_closure"]] = in_manifest_closure
    previous_line_status[["in_nested_expression_to_ignore"]] = in_nested_expression_to_ignore
    #else{
       #rlog::log_info(paste("FOUND UNCATEGORIZED LINE:",conf_other_lines[i]))
       #lines_to_keep = c(lines_to_keep,conf_other_lines[i])
    #}
  }
  ## double check manifest closure is done
  if(length(lines_to_migrate) > 0){
    if(!grepl("\\}",lines_to_migrate[length(lines_to_migrate)])){
      rlog::log_info(paste("Adding left bracket '}' to ensure manifest closure is a proper expression"))
      lines_to_migrate = c(lines_to_migrate,"}")
    }
  }
  
  if(length(config_lines_rescued) > 0){
    config_lines_rescued = unique(config_lines_rescued)
    for(clr in 1:length(config_lines_rescued)){
      if(!config_lines_rescued[clr] %in% lines_to_keep){
        lines_to_keep = c(lines_to_keep,config_lines_rescued)
      }
    }
  }
  
  ###
  second_pass_lines[["lines_to_keep"]] = lines_to_keep
  second_pass_lines[["lines_to_migrate"]] = lines_to_migrate
  
  return(second_pass_lines)
}

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
############
#rlog::log_warn(paste("Number of closures in config",config_file,sum(grepl("\\{",t(config_dat)))))
#if( sum(grepl("\\{",t(config_dat))) > 0 && !is_simple_config){
#  z1 = findOtherConfigs(conf_path=config_file,conf_data=config_dat)
#} else{
#  z1 = NULL 
#}
#final_config_list = c()
#if(length(configs_to_ignore) > 0 && !is.null(z1)){
#  z1 = z1[ !(z1 %in% configs_to_ignore)]
#}
### second pass configs to ignore
#configs_to_ignore = z1[ apply(t(z1),2,function(x) sum(strsplit(x,'/')[[1]] == "") > 1) ]
#rlog::log_info(paste("CONFIGS_TO_IGNORE:",configs_to_ignore))
#if(length(configs_to_ignore) > 0 && !is.null(z1)){
#  z1 = z1[ !(z1 %in% configs_to_ignore)]
#}
#rlog::log_info(paste("CONFIGS_FOUND:",z1))
#final_config_list = c()
#if(length(z1) > 0){
#  final_config_list = localConfigOrNot(paramsFiller(list_to_fill=z1,params_list=z))
#} else{
#  final_config_list = NULL
#}

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
          rlog::log_info(paste("EXPRESSION_CLOSURE:",in_expression))
          rlog::log_info(paste("CONDITION_FOR_CONFIGURATION:",condition_for_config))
          rlog::log_info(paste("PARAMETER_NAME:",parameter_name))
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
          } else if(in_process_closure && (clean_line[1] == "withName:" | grepl("withName:",clean_line[1]) | clean_line[1] == "withLabel:"|grepl("withLabel:",clean_line[1])) ){
            in_module_closure = TRUE
            if(!grepl("\\{",clean_line[2])){
              module_name = gsub("\\'","",trimws(clean_line[2]))
            } else{
              module_name = trimws(strsplit(clean_line[1],"\\:")[[1]][2])
            }
            if(grepl("withName:",clean_line[1])){
              module_name = paste("withName:",module_name,sep="")
            } else if(grepl("withLabel:",clean_line[1])){
              module_name = paste("withLabel:",module_name,sep="")
            }
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
                add_closure = FALSE
                if(grepl("\\{",value_collection) & grepl("\\}",value_collection)){
                  if((grepl("\\(",value_collection) & grepl("\\)",value_collection)) | (grepl("\\$",value_collection))){
                    add_closure = TRUE
                  }
                }
                value_collection = gsub("\\{","",value_collection)
                value_collection = gsub("\\}","",value_collection)
                if(value_collection != "["){
                  if(parameter_name != "unknown"){
                    rlog::log_info(paste("Point1 : Found value for parameter:",parameter_name,"value:",value_collection))
                    value_collection = sub("//","// ",value_collection)
                    value_collection_split  = strsplit(value_collection,"\\s+")[[1]]
                    idx_of_interest = 1
                    if(length(value_collection_split) > 0){
                      if(sum(value_collection_split %in%  "//") > 0){
                        idx_of_interest = (1:length(value_collection_split))[value_collection_split %in%  "//"]
                      }
                    }
                    if(add_closure){
                      if(idx_of_interest == 1){
                        value_collection = paste("{",value_collection,"}",collapse = " ")
                      } else{
                        value_collection = paste("{",paste(value_collection_split[1:idx_of_interest-1],collapse=" "),"}",paste(value_collection_split[idx_of_interest:length(value_collection_split)],collapse=" "),collapse=" ")
                      }
                    }
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
            } else{
              parameter_name = "general"
              rlog::log_info(paste("Point2 : Found value for parameter:",parameter_name,"value:",value_collection))
              moduleMetadata[[parameter_name]] = value_collection
              value_collection = c()
            }
          } else if(in_process_closure && in_module_closure && !("{" %in% clean_line & "}" %in% clean_line) && (clean_line[1] == "}"  || grepl("\\}$",clean_line[length(clean_line)]))){
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
              rlog::log_info(paste("FOUND publishDir configuration"))
              value_to_add = paste(clean_line[3:length(clean_line)],collapse=" ")
              value_to_add = trimws(value_to_add)
              value_to_add_split = strsplit(value_to_add,"\\s+")[[1]]
              if(value_to_add_split[1] != "path:"){
                value_collection = c(value_collection,paste(clean_line[3:length(clean_line)],collapse=" "))
              } else{
                value_collection = c(value_collection,paste("[",clean_line[3:length(clean_line)],collapse=" ",sep = " "))
              }
              # value_collection = c(value_collection,paste(clean_line[3:length(clean_line)],collapse=" "))
            } else{
              value_collection = paste(clean_line[3:length(clean_line)],collapse=" ")
              if(grepl("\\{",value_collection) & grepl("\\}",value_collection)){
                if((grepl("\\(",value_collection) & grepl("\\)",value_collection)) | (grepl("\\$",value_collection))){
                  add_closure = TRUE
                }
              }
              value_collection = gsub("\\{","",value_collection)
              value_collection = gsub("\\}","",value_collection)
              if(value_collection != "["){
                if(parameter_name != "unknown"){
                  rlog::log_info(paste("Point1 : Found value for parameter:",parameter_name,"value:",value_collection))
                  value_collection = sub("//","// ",value_collection)
                  value_collection_split = strsplit(value_collection,"\\s+")[[1]]
                  idx_of_interest = 1
                  if(length(value_collection_split) > 0){
                    if(sum(value_collection_split %in%  "//")>0){
                      idx_of_interest = (1:length(value_collection_split))[value_collection_split %in%  "//"]
                    }
                  }
                  if(idx_of_interest == 1){
                    value_collection = paste("{",value_collection,"}",collapse = " ")
                  } else{
                    value_collection = paste("{",paste(value_collection_split[1:idx_of_interest-1],collapse=" "),"}",paste(value_collection_split[idx_of_interest:length(value_collection_split)],collapse=" "),collapse=" ")
                  }
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
