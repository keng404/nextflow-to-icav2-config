source('dsl2_compatible_parser.R')
library(rlog)
library(stringr)
y = loadModuleMetadata("/Users/keng/nf-core/rnaseq/conf/modules.config")
#print(y)
z1 = findModules("/Users/keng/nf-core/rnaseq/workflows/rnaseq.nf")
# if module has just one avatar or multiple avatars, then add publish statement and add params.outdir_custom to the appropriate line(s) in the
# intermediate_data_structure1 : dictionary of line edits where key is line_number and the value is line_edits
# intermediate_data_structure2 : original lines
# inputs:
# output from findModules
# for each module grab process lines and add publishstatement
# - dictionary of process lines
# - pass this dictionary to addPublishStatement function
# output: new_lines
################################
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
makeFinalEdits <- function(nf_script,module_metadata,module_location){
  new_lines = c()
  new_lines = t(read.delim(nf_script,header=F,quote=""))
  line_edits = list()
  modules_of_interest = names(module_location)
  configurations_to_ignore = c("publishDir","errorStrategy","cpus","memory")
  for(i in 1:length(modules_of_interest)){
    module_of_interest = modules_of_interest[i]
    lines_to_add = c()
    # add publish statement and add params.outdir_custom to the appropriate line(s) in the new_lines
    line_numbers_of_interest = module_location[[module_of_interest]][["line_number"]]
    module_script = module_location[[module_of_interest]][["module_path"]]
    rlog::log_info(paste("Investigating :",module_script))
    module_lines = t(read.delim(module_script,header=F,quote=""))
    module_lines1 = addPublishStatement(module_lines)
    ###########################################
    if(paste(module_lines1,collapse="\n") != paste(module_lines,collapse="\n")){
      updated_module_script = gsub(".nf$",".dev.nf",module_script)
      write.table(x=module_lines1,file=updated_module_script,sep="\n",quote=F,row.names=F,col.names=F)
      rlog::log_info(paste("Generated updated module script to:",updated_module_script))
     # system(paste("cp",updated_module_script,module_script))
    }
    publish_dir_statement = paste("params.outdir_custom","=", paste("\"",getCustomOutdirName(module_of_interest),"\"",sep=""))
    if(length(grepl("\\{$",lines_to_add[length(lines_to_add)])) == 0){
      lines_to_add = c(lines_to_add,publish_dir_statement)
    } else if(!grepl("\\{$",lines_to_add[length(lines_to_add)])){
      lines_to_add = c(lines_to_add,publish_dir_statement)
    } else{
      lines_to_add = c(lines_to_add,c("\n",paste("\t",publish_dir_statement)))
    }
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
              lines_to_add = c(lines_to_add,paste("\t",configuration_parameters[k1],"=",parameter_value))
            }
            lines_to_add = c(lines_to_add,"}")
            
          } else{
            for(k1 in 1:length(configuration_parameters)){
              parameter_value = module_metadata[[module_name_for_configuration]][[configuration_scenarios[k]]][[configuration_parameters[k1]]]
              if(length(parameter_value) > 1){
                parameter_value = paste(parameter_value , collapse = " ")
              }
              lines_to_add = c(lines_to_add,paste(configuration_parameters[k1],"=",parameter_value))
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
        #new_lines[lidx] = paste(new_lines[lidx],paste(line_edits[[toString(lidx + 1)]],collapse="\n"),collapse="\n")
        new_lines[lidx] =  paste(line_edits[[toString(lidx + 1)]],collapse="\n")
      }
    }
  }
  updated_nf_script = gsub(".nf$",".final_edits.nf",nf_script)
  rlog::log_info(paste("Writing out final edits to :",updated_nf_script))
  write.table(x=new_lines,file=updated_nf_script,sep="\n",quote=F,row.names=F,col.names=F)
  system(paste("cp",updated_nf_script,nf_script))
}

makeFinalEdits("/Users/keng/nf-core/rnaseq/workflows/rnaseq.nf",y,z1)