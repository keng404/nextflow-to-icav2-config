options(stringsAsFactors=FALSE)
library(argparse)
library(rlog)
library(XML)
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
parser$add_argument("-i","--configs-to-hardcode","--configs_to_hardcode", default="params_to_inject.txt",
                    help="config params to inject")
parser$add_argument("-u","--instance-type-url","--instance_type_url", default="https://help.ica.illumina.com/project/p-flow/f-pipelines#definition",
                    help = "URL that contains ICA instance type table")
parser$add_argument("-a","--base-config-files","--base_config_files", default=c(""),nargs="?",
                    action="append", help = "configuration file for base configs of a pipeline")
parser$add_argument("-t","--params-override-template","--params_override_template", default = "params_to_strip.txt",
                    help = "params to remove from nextflow.config")
parser$add_argument("-w","--config-template","--config_template", default = "template.base.config",
                    help = "nextflow config_template file")
parser$add_argument("-e","--ica-config","--ica_config", default = "nextflow.ica.config",
                    help = "basename of ICA config file")
parser$add_argument("-f","--base-config-relative-path","--base_config_relative_path", default = "conf/base.ica.config",
                    help = "nextflow base_config_relative_path file")
# get command line options, if help option encountered print help and exit,
# otherwise if options not found on command line then set defaults, 
args <- parser$parse_args()
is_simple_config = args$is_simple_config
config_file = args$config_file
config_dat  = read.delim(config_file,quote="",header=F)
parameters_xml  = args$parameters_xml
configs_to_hardcode = args$configs_to_hardcode
params_override_template = args$params_override_template
ica_config = args$ica_config
generate_parameters_xml = args$generate_parameters_xml
base_config_files = args$base_config_files
base_config_files = base_config_files[base_config_files != ""]
if(length(base_config_files) < 1){
  base_config_files = NULL
}
rlog::log_info(paste("BASE_CONFIG_FILES:",paste(base_config_files,sep=", "),collapse=" "))
base_config_template = args$config_template
base_config_relative_path = args$base_config_relative_path
if(!is.null(parameters_xml)){
  generate_parameters_xml = TRUE
}
###########
ica_instance_namespace = args$ica_instance_namespace
default_instance = args$default_instance
instance_type_table_url = args$instance_type_url
rlog::log_debug(paste("URL_OF_INTEREST:",instance_type_table_url,collapse = " "))
ica_instance_table = get_instance_type_table(url=instance_type_table_url)
ica_instance_table$CPUs = as.numeric(ica_instance_table$CPUs)
ica_instance_table$`Mem (GB)` = as.numeric(ica_instance_table$`Mem (GB)`)
additional_lines = c("process {",'\twithName:\'CUSTOM_DUMPSOFTWAREVERSIONS\' {',"\terrorStrategy = 'ignore'","\t}","}")
##############################################
parameters_to_list <- function(parameter_xml){
  parameter_names = names(parameter_xml)
  parameter_list = list()
  for(i in 1:length(parameter_names)){
    tools = parameter_xml[i][["step"]][["tool"]]
    for(j in 1:length(tools)){
      param_setting = tools[j]
      if("parameter" %in% names(param_setting)){
        rlog::log_info(paste("looking at",param_setting))
        parameter_name = param_setting[["parameter"]][[".attrs"]][["code"]]
        parameter_attributes = names(param_setting[["parameter"]])
        parameter_list[[parameter_name]] = list()
        type_boolean = apply(t(parameter_attributes),2,function(x) grepl("Type",x))
        if(sum(type_boolean) > 0){
          parameter_list[[parameter_name]][["type"]] = parameter_attributes[type_boolean]
        } else{
          parameter_list[[parameter_name]][["type"]] = NULL
        }
        # rlog::log_info(paste("type_boolean:",parameter_list[[parameter_name]][["type"]]))
        parameter_list[[parameter_name]][["minValue"]] = param_setting[["parameter"]][[".attrs"]][["minValues"]]
        parameter_list[[parameter_name]][["maxValue"]] = param_setting[["parameter"]][[".attrs"]][["maxValues"]]
        ##print(param_setting[["parameter"]])
        if(is.null(param_setting[["parameter"]][["value"]]) & parameter_name != "input"){
          parameter_list[[parameter_name]][["value"]] = "STRING"
        } else if( is.null(param_setting[["parameter"]][["value"]]) & parameter_name == "input"){
          parameter_list[[parameter_name]][["value"]] = ""
        } else if(!is.null(param_setting[["parameter"]][["value"]]) & param_setting[["parameter"]][["value"]] != "null"){
          rlog::log_info(paste("Found default value",param_setting[["parameter"]][["value"]] ))
          parameter_list[[parameter_name]][["value"]] = param_setting[["parameter"]][["value"]] 
        } else if(parameter_attributes[type_boolean] == "optionsType" & "value" %in% names(param_setting[["parameter"]])){
          if((is.null(param_setting[["parameter"]][["value"]]) || param_setting[["parameter"]][["value"]] == "null" )){
            option_settings = param_setting[["parameter"]][[parameter_attributes[type_boolean]]][["option"]]
            # default to GRCh38 if possible
            found_genome_setting = apply(t(option_settings),2,function(x) grepl("GRCh38",x))
            rlog::log_info(paste("found_genome_setting:",paste(found_genome_setting,sep=", ",collapse=", ")))
            if(sum(found_genome_setting) == 0){                                                                                                   
              parameter_list[[parameter_name]][["value"]] = param_setting[["parameter"]][[parameter_attributes[type_boolean]]][["option"]][1]
            } else{
              parameter_list[[parameter_name]][["value"]] = option_settings[found_genome_setting][1]
            }
          } else{
            parameter_list[[parameter_name]][["value"]] = "STRING"
          }
        } else{
          rlog::log_warn(paste("Could not find default value for ",parameter_name))
          parameter_list[[parameter_name]][["value"]] = "STRING"
        }
      }
    }
  }
  return(parameter_list)
}

add_parameters_to_xml <- function(keys_to_add,xml_file,option_list){
  doc = xmlTreeParse(xml_file,useInternalNodes = TRUE)
  root = xmlRoot(doc)
  ###### Grab all parameters from XML under each tool and output to list ----
  tools = root[["steps"]][[1]][["tool"]]
  #######
  if(length(keys_to_add)>0){
    for(lv in 1:length(keys_to_add)){
      key_to_add_alias = keys_to_add[lv]
      key_to_add_alias = gsub("^params.","",keys_to_add[lv])
      rlog::log_info(paste("Adding ",key_to_add_alias,"to",xml_file))
      nested_parameter_node = XML::newXMLNode("parameter",parent=tools)
      xmlAttrs(nested_parameter_node) = c(code = key_to_add_alias ,minValues = "0",maxValues="1",classification="USER")
      newXMLNode("label",key_to_add_alias,parent=nested_parameter_node)
      newXMLNode("description",paste("Configure",keys_to_add[lv]),parent=nested_parameter_node)
      # remove double-quotes
      option_list[[keys_to_add[lv]]] = gsub("\"","",option_list[[keys_to_add[lv]]])
      ##############
      if(!is.na(strtoi(option_list[[keys_to_add[lv]]]))){
        parameter_type = 'integerType'
      } else if(!is.na(as.numeric(option_list[[keys_to_add[lv]]]))){
        parameter_type = 'floatType'
      } else if(option_list[[keys_to_add[lv]]] %in% c('true','false',TRUE,FALSE)){
        parameter_type = 'booleanType'
      } else{
        parameter_type = 'stringType'
      }
      ###############
      options_node = XML::newXMLNode(paste(parameter_type),parent=nested_parameter_node)
      newXMLNode("value",option_list[[keys_to_add[lv]]],parent=nested_parameter_node)
    }
  }
  outputPath = gsub(".xml$",".updated.xml",xml_file)
  #rlog::log_info(paste("Updating parameters XML here:",outputPath))
  #prefix='<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n'
  prefix.xml <- "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n"
  saveXML(doc , file=outputPath,encoding="utf-8")
  system(paste("mv",outputPath,xml_file))
  rlog::log_info(paste("Updating parameters XML to:",xml_file))
}
test_config_update_xml <- function(config_file,option_list,parameters_to_check = NULL){
  if(is.null(parameters_to_check)){
    parameters_to_check = names(option_list)
  }
  parameter_xml_file = paste(dirname(config_file),paste(basename(dirname(config_file)),".pipeline.xml",sep=""),sep="/")
  rlog::log_info(paste("UPDATING parameters XML file:",parameter_xml_file))
  doc = xmlToList(parameter_xml_file)
  tool_names = doc[["steps"]]
  final_tool_params_list = parameters_to_list(tool_names)
  existing_parameters = names(final_tool_params_list)
  existing_parameters = apply(t(existing_parameters),2,function(x) paste("params.",x,sep=""))
  parameters_to_check_bool = !parameters_to_check %in% existing_parameters
  if(sum(parameters_to_check_bool) > 0){
    parameters_to_check = parameters_to_check[parameters_to_check_bool]
    if(length(parameters_to_check) > 0){
      add_parameters_to_xml(parameters_to_check,parameter_xml_file,option_list)
    } else{
      rlog::log_info(paste("No updates needed for:",parameter_xml_file))
    }
  } else{
    rlog::log_info(paste("No updates needed for:",parameter_xml_file))
  }
}
#################################################
add_test_config <- function(dir_of_interest){
  test_config = NULL
  configs = list.files(dir_of_interest,pattern="config",full.names = T,recursive = T)
  if(length(configs) >0){
    if(sum(basename(configs) == "test_full.config") > 0 ){
      test_config = configs[basename(configs) == "test_full.config"]
      rlog::log_info(paste("Choosing the following test config:",test_config))
      return(test_config)
    } else if(sum(basename(configs) == "test.config") > 0 ){
      test_config = configs[basename(configs) == "test.config"]
      rlog::log_info(paste("Choosing the following test config:",test_config))
      return(test_config)
    } else{
      rlog::log_info(paste("Choosing the following test config:",configs[1]))
      return(configs[1])
    }
  } else{
    rlog::log_warn(paste("No test configs found for:",dir_of_interest))
    return(test_config)
  }
}
#######################
add_testing_config <- function(test_config_file_path,config_file){
  reference_statement = paste("   includeConfig",paste("'",test_config_file_path,"'",collapse="",sep=""),collapse=" ")
  #robust_input_handling_cmd = c("if(params.ica_smoke_test) {","\tif(params.input == \"\") {","\t\tparams.input = null","\t}","}")
  robust_input_handling_cmd = c("if(params.ica_smoke_test) {","    params.input = null",reference_statement,"}")
  test_config_file_path = getRelativePath(to=test_config,from=config_file)
  conf_dat = t(read.delim(config_file,header=F,quote=""))
  all_lines = c(paste(robust_input_handling_cmd,collapse="\n"),conf_dat)
  rlog::log_info(paste("UPDATING",config_file))
  updated_nextflow_config_file = gsub(".config$",".config.tmp",config_file)
  write.table(x=all_lines,file=updated_nextflow_config_file,sep="\n",quote=F,row.names=F,col.names=F)
  system(paste("cp",updated_nextflow_config_file,config_file))
}
################################################
if(is_simple_config | is.null(base_config_files)){
  # parse config file
  config_params = get_params_from_config(conf_file=config_file)
  config_lines = get_other_lines_from_config(conf_file=config_file)
  if(length(config_lines) < 1){
    config_lines = NULL
  }
  config_params_updated = strip_params(params_list = config_params,params_to_strip=params_to_strip(params_override_template))
  config_params = inject_params(params_list = config_params_updated, params_to_inject=params_to_override(configs_to_hardcode))
  # generate ICA nextflow config file
  write_params(params_list = config_params,additional_lines = config_lines,output_file=paste(dirname(config_file),ica_config,sep="/"))
  # copy ICA module template
  copy_modules_template(template_file=base_config_template,destination_dir=dirname(config_file))
  get_file_extension = strsplit(basename(base_config_template),"\\.")[[1]]
  base_config_relative_path = gsub(paste(".",get_file_extension[length(get_file_extension)],"$",sep="",collapse=""),".ica.config",base_config_file_path)
  
  # add reference to your module config file
  add_module_reference(nextflow_config=paste(dirname(config_file),ica_config,sep="/"),existing_module_file=NULL,additional_config=base_config_relative_path)
  test_config = add_test_config(dirname(config_file))
  if(!is.null(test_config)){
    test_config_file_path = getRelativePath(to=test_config,from=config_file)
    test_config_params = get_params_from_config(conf_file=test_config)
    #add_module_reference(nextflow_config=paste(dirname(config_file),ica_config,sep="/"),existing_module_file=NULL,additional_config=test_config_file_path,for_testing=TRUE)
    add_testing_config(test_config_file_path,paste(dirname(config_file),ica_config,sep="/"))
    test_config_update_xml(config_file,test_config_params)
  } else{
    rlog::log_info(paste("No testing config found"))
  }
} else{
  # parse config file
  config_params = get_params_from_config(conf_file=config_file)
  config_lines = get_other_lines_from_config(conf_file=config_file)
  
  if(length(config_lines[["lines_to_keep"]]) < 1){
    config_lines[["lines_to_keep"]] = NULL
  }
  if(length(config_lines[["lines_to_migrate"]]) < 1){
    config_lines[["lines_to_migrate"]] = NULL
  }
  second_pass_lines = second_pass_config_other_lines(config_lines[["lines_to_keep"]])
  if(length(second_pass_lines[["lines_to_keep"]]) < 1){
    second_pass_lines[["lines_to_keep"]] = NULL
  }
  if(length(second_pass_lines[["lines_to_migrate"]]) < 1){
    second_pass_lines[["lines_to_migrate"]] = NULL
  }
  config_params_updated = strip_params(params_list = config_params,params_to_strip=params_to_strip(params_override_template))
  config_params = inject_params(params_list = config_params_updated, params_to_inject=params_to_inject(configs_to_hardcode))
  # generate ICA nextflow config file
  write_params(params_list = config_params,additional_lines = second_pass_lines[["lines_to_keep"]],output_file=paste(dirname(config_file),ica_config,sep="/"))
  # add reference to your module config file
  for(i in 1:length(base_config_files)){
    base_config_file = base_config_files[i]
    base_config_file_path = getRelativePath(to=base_config_file,from=config_file)
    rlog::log_info(paste("BASE_CONFIG_RELATIIVE_PATH:",base_config_relative_path,collapse=" "))
    get_file_extension = strsplit(base_config_file,"\\.")[[1]]
    rlog::log_info(paste("file_extension:",get_file_extension[length(get_file_extension)],collapse=" "))
    base_config_relative_path = gsub(paste(".",get_file_extension[length(get_file_extension)],"$",sep="",collapse=""),".ica.config",base_config_file_path)
    rlog::log_info(paste("BASE_CONFIG_UPDATED_RELATIIVE_PATH:",base_config_relative_path,collapse=" "))
    # parse modules file
    base_configuration = loadModuleMetadata(c(base_config_file))
    updated_base_configuration = override_module_config(module_list = base_configuration,ica_instance_table = ica_instance_table)
    
    ## add additional overrides
    second_pass_lines[["lines_to_migrate"]]  = c(second_pass_lines[["lines_to_migrate"]] ,additional_lines)
    write_modules(modules_list = updated_base_configuration,output_file=paste(dirname(config_file),base_config_relative_path,sep="/"),template_file=base_config_template,additional_lines = second_pass_lines[["lines_to_migrate"]])
    add_module_reference(nextflow_config=paste(dirname(config_file),ica_config,sep="/"),existing_module_file=base_config_file_path,additional_config=base_config_relative_path)
  }
  test_config = add_test_config(dirname(config_file))
  if(!is.null(test_config)){
    test_config_file_path = getRelativePath(to=test_config,from=config_file)
    test_config_params = get_params_from_config(conf_file=test_config)
    #add_module_reference(nextflow_config=paste(dirname(config_file),ica_config,sep="/"),existing_module_file=NULL,additional_config=test_config_file_path,for_testing=TRUE)
    add_testing_config(test_config_file_path,paste(dirname(config_file),ica_config,sep="/"))
    test_config_update_xml(config_file,test_config_params)
  } else{
    rlog::log_info(paste("No testing config found"))
  }
}
