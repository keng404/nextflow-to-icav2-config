library(XML)
library(rjson)
library(jsonlite)
options(stringsAsFactors=FALSE)
suppressPackageStartupMessages(library("argparse"))
##
data_inputs_to_list <- function(data_input_xml){
  data_input_names = names(data_input_xml)
  data_input_list = list()
  for(i in 1:length(data_input_names)){
    rlog::log_info(paste("LOOKING at:", data_input_xml[i]))
    data_input = data_input_xml[i][["dataInput"]]
    data_input_name = data_input[[".attrs"]][["code"]]
    data_input_list[[data_input_name]] = list()
    data_input_list[[data_input_name]][["type"]] = data_input[[".attrs"]][["type"]]
    data_input_list[[data_input_name]][["required"]] = data_input[[".attrs"]][["required"]]
    data_input_list[[data_input_name]][["multiValue"]] = data_input[[".attrs"]][["multiValue"]]
    if(data_input[[".attrs"]][["multiValue"]]){
      data_input_list[[data_input_name]][["value"]] = c()
    } else{
      data_input_list[[data_input_name]][["value"]] = "STRING"
    }
  }
  return(data_input_list)
}

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
        if(!is.null(param_setting[["parameter"]][["value"]]) & param_setting[["parameter"]][["value"]] != "null"){
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

data_list_for_template <- function(list_of_params,restrict_to_required_params){
  list_of_params_final = list()
  if(restrict_to_required_params){
    for(i in 1:length(names(list_of_params))){
      if(list_of_params[[names(list_of_params)[i]]]$required == "true"){
        list_of_params_final[[names(list_of_params)[i]]] = list_of_params[[names(list_of_params)[i]]]
      } else{
        rlog::log_warn(paste("Not adding",names(list_of_params)[i], "to template"))
      }
    }
  } else{
    list_of_params_final = list_of_params
  }
  return(list_of_params_final)
}
param_list_for_template <- function(list_of_params,restrict_to_required_params){
  list_of_params_final = list()
  if(restrict_to_required_params){
    for(i in 1:length(names(list_of_params))){
      if(strtoi(list_of_params[[names(list_of_params)[i]]]$minValue) > 0){
        list_of_params_final[[names(list_of_params)[i]]] = list_of_params[[names(list_of_params)[i]]]
      } else{
        rlog::log_warn(paste("Not adding",names(list_of_params)[i], "to template"))
      }
    }
  } else{
    list_of_params_final = list_of_params
  }
  return(list_of_params_final)
}
###
create_json_template <- function(data_input_list,parameter_list,file_output){
  template_list = list()
  empty_data_inputs = FALSE
  empty_parameter_list = FALSE
  if(length(names(parameter_list)) < 1){
    rlog::log_warn(paste("Could not find any parameters for template"))
    empty_parameter_list = TRUE
  }
  if(length(names(data_input_list)) < 1){
    rlog::log_warn(paste("Could not find any data inputs for template"))
    empty_data_inputs = TRUE
  }
  template_list[["input"]] = data_input_list
  template_list[["parameters"]] = parameter_list
  if(!empty_parameter_list | !empty_data_inputs){
    template_JSON = jsonlite::toJSON(template_list,pretty=TRUE)
    rlog::log_info(paste("Writing template to",file_output))
    write(template_JSON,file=file_output)
    return(TRUE)
  } else{
    rlog::log_warn(paste("Did not generate template. No parameters or inputs found"))
    rlog::log_warn(paste("EMPTY_DATA_INPUTS:",empty_data_inputs,"EMPTY_PARAMETER_LIST:",empty_parameter_list))
    return(FALSE)
  }
  
}
cli_preview <- function(json_template,workflow_language){
  boilerplate_cli_prefix = c("icav2 projectpipelines","start",workflow_language,"PIPELINE_NAME")
  boilerplate_cli_suffix = c("--storage-size Small","--project-id PROJECT_ID")
  cli_interstitial = c()
  json_template_data = rjson::fromJSON(file=json_template)
  for(i in 1:length(names(json_template_data))){
    cli_prefix = names(json_template_data)[i]
    keys_to_add = names(json_template_data[[cli_prefix]])
    for(j in 1:length(keys_to_add)){
      key_name = keys_to_add[j]
      key_value = ""
      if("value" %in% names(json_template_data[[cli_prefix]][[key_name]])){
        key_value = json_template_data[[cli_prefix]][[key_name]][["value"]]
      }
      if(length(key_value) > 1 ){
        key_value = paste(key_value,sep=",",collapse=",")
      }
      rlog::log_info(paste("KEY_VALUE:",key_value,"KEY_NAME:",key_name))
      if(key_value == "STRINGS" & cli_prefix == "input"){
        key_value = "DATA_ID1,DATA_ID2,...,DATA_IDxx"
      } 
      if(key_name == "outdir"){
        key_value = "out"
      }
      if(grepl("\\.|\\-",key_value) & !grepl("fil",key_value) & !grepl("fol",key_value) & !grepl("'",key_value)){
        key_value = paste("'",key_value,"'",sep="")
      }
      string_to_add = paste(paste("--",cli_prefix,sep=""),paste(key_name,":",key_value,sep=""),collapse = " ",sep = " ")
      #rlog::log_info(paste("ADDING:",string_to_add))
      cli_interstitial = c(cli_interstitial,string_to_add)
    }
  }
  final_string_components = c(boilerplate_cli_prefix,cli_interstitial,boilerplate_cli_suffix)
  final_string_components = unlist(final_string_components)
  return(paste(final_string_components,collapse = " ",sep = " "))
}
# create parser object
parser <- ArgumentParser()
parser$add_argument("-r","--only-required","--only_required", default=FALSE,
                    action="store_true", help = "parameters XML file")
parser$add_argument("-w","--workflow-language","--workflow_language", default="nextflow",
                    choices = c("nextflow","cwl"), help = "workflow language")
parser$add_argument("-x","--parameters-xml","--parameters_xml", default=NULL,
                    required=TRUE, help = "parameters XML file")
parser$add_argument("-o","--output-json","--output_json",
                    default=NULL, help = "output JSON generated to be used as template")
parser$add_argument("-t","--template-json","--template_json",
                    default=NULL, help = "template file to get CLI command")
args <- parser$parse_args()
parameter_xml_file = args$parameters_xml
only_required = args$only_required
output_json = args$output_json
workflow_language = args$workflow_language[1]
template_json = args$template_json
if(is.null(template_json)){
  # don't create template JSON if template JSON is provided.
  if(is.null(output_json)){
    output_json = gsub(".xml$",".ICAv2_CLI_template.json",parameter_xml_file)
    rlog::log_info(paste("Saving template to: ",output_json))
  }
  #########################################
  rlog::log_info(paste("PARSING parameters XML file:",parameter_xml_file))
  doc = xmlToList(parameter_xml_file)
  ##### Grab all dataInputs from XML and output to list ---- 
  data_input_names = doc[["dataInputs"]]
  final_data_input_list = data_inputs_to_list(data_input_names)
  final_input = data_list_for_template(final_data_input_list,only_required)
  ###### Grab all parameters from XML under each tool and output to list ----
  tool_names = doc[["steps"]]
  final_tool_params_list = parameters_to_list(tool_names)
  final_params = param_list_for_template(final_tool_params_list,only_required)
  
  #### create output json
  JSON_TEMPLATE_CREATED = create_json_template(final_input,final_params,output_json)
  if(JSON_TEMPLATE_CREATED){
    CLI_PREVIEW = cli_preview(json_template=output_json,workflow_language=workflow_language)
    rlog::log_info(paste("CLI_COMMAND_TEMPLATE:",CLI_PREVIEW))
  } else{
    rlog::log_warn(paste("NO template generated at:",output_json))
  }
} else{
  CLI_PREVIEW = cli_preview(json_template=template_json,workflow_language=workflow_language)
  rlog::log_info(paste("CLI_COMMAND_TEMPLATE:",CLI_PREVIEW))
}
