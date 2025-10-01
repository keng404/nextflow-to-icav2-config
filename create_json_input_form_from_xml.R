library(XML)
options(stringsAsFactors=FALSE)
suppressPackageStartupMessages(library("argparse"))
suppressPackageStartupMessages(library("jsonlite"))
suppressPackageStartupMessages(library("rjson"))
##
data_inputs_to_list <- function(data_input_xml){
  data_input_names = names(data_input_xml)
  data_input_list = list()
  for(i in 1:length(data_input_names)){
    rlog::log_info(paste("LOOKING at:", data_input_xml[i]))
    data_input = data_input_xml[i][["dataInput"]]
    data_input_name = data_input[[".attrs"]][["code"]]
    data_input_list[[data_input_name]] = list()
    data_input_list[[data_input_name]][["label"]] =  data_input[["label"]]
    data_input_list[[data_input_name]][["description"]] =  data_input[["description"]]
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
        parameter_list[[parameter_name]][["label"]] =  param_setting[["label"]]
        parameter_list[[parameter_name]][["description"]] =  param_setting[["description"]]
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
          default_value = gsub("'","",param_setting[["parameter"]][["value"]])
          rlog::log_info(paste("Found default value",default_value ))
          parameter_list[[parameter_name]][["value"]] = default_value
          if(parameter_attributes[type_boolean] == "optionsType"){
              option_settings = unlist(param_setting[["parameter"]][[parameter_attributes[type_boolean]]])
              rlog::log_info(paste("collected options:", t(option_settings)))
              parameter_list[[parameter_name]][["options"]] = t(option_settings)
          }
        } else if(parameter_attributes[type_boolean] == "optionsType" & "value" %in% names(param_setting[["parameter"]])){
            if((is.null(param_setting[["parameter"]][["value"]]) || param_setting[["parameter"]][["value"]] == "null" )){
              option_settings = param_setting[["parameter"]][[parameter_attributes[type_boolean]]][["option"]]
              rlog::log_info(paste("collected options:", t(option_settings)))
              parameter_list[[parameter_name]][["options"]] = t(option_settings)
              # default to GRCh38 if possible
              found_genome_setting = apply(t(option_settings),2,function(x) grepl("GRCh38",x))
              rlog::log_info(paste("found_genome_setting:",paste(found_genome_setting,sep=", ",collapse=", ")))
              if(sum(found_genome_setting) == 0){   
                default_value = param_setting[["parameter"]][[parameter_attributes[type_boolean]]][["option"]][1]
                parameter_list[[parameter_name]][["value"]] = gsub("'","",default_value)
              } else{
                default_value = option_settings[found_genome_setting][1]
                parameter_list[[parameter_name]][["value"]] = gsub("'","",default_value)
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
############
###dataInputs -> field-data
###booleanType -> CHECKBOX type
####stringType -> TEXTBOX type
####doubleType -> NUMBER type
###INTEGER type
###optionType -> SELECT type
#############
convert_json_import_form_types <- function(info_type){
  type_returned = info_type
  if(info_type == "booleanType"){
    type_returned = "checkbox"
  } else if(info_type == "stringType"){
    type_returned = "textbox"
  } else if(info_type == "doubleType"){
    type_returned = "number"
  } else if(info_type == "integerType"){
    type_returned = "integer"
  } else if(info_type == "optionsType"){
    type_returned = "select"
  } 
  return(type_returned)
}

create_json_input_form <- function(data_input_list,full_parameter_list,file_output){
  input_form = list()
  empty_data_inputs = FALSE
  empty_parameter_list = FALSE
  if(length(names(full_parameter_list)) < 1){
    rlog::log_warn(paste("Could not find any parameters for template"))
    empty_parameter_list = TRUE
  }
  if(length(names(data_input_list)) < 1){
    rlog::log_warn(paste("Could not find any data inputs for template"))
    empty_data_inputs = TRUE
  }
  data_input_list_collection = list()
  if(!empty_data_inputs){
    section_list = list()
    section_list[["id"]] = "section_datainputs"
    section_list[["type"]] = "section"
    section_list[["label"]] = "Section for selecting data inputs"
    data_input_list_collection[[1]] = section_list
    for(i in 1:length(names(data_input_list))){
      input_name = names(data_input_list)[i]
      input_list = list()
      input_list[["type"]] = "data"
      input_list[["id"]] = input_name
      input_list_required = data_input_list[[input_name]][["required"]]
      input_list[["helpText"]] =  data_input_list[[input_name]][["description"]]
      input_list[["label"]] =   data_input_list[[input_name]][["label"]]
      input_list_multi_value = data_input_list[[input_name]][["multiValue"]]
      input_list[["minValues"]] = 0
      if(!is.null(input_list_required)){
        if(input_list_required == "true"){
          input_list[["minValues"]] = 1
        } else{
          input_list[["minValues"]] = 0
        }
      }
      if(!is.null(input_list_multi_value)){
        if(input_list_multi_value == "true"){
          input_list[["maxValues"]] = 999
        } else{
          input_list[["maxValues"]] = 1
        }
      } else{
        input_list[["maxValues"]] = 1
      }
      if(data_input_list[[input_name]][["type"]] == "DIRECTORY"){
        input_list[["dataFilter"]] = list()
        ###input_list[["dataFilter"]][["dataType"]] = list()
        input_list[["dataFilter"]][["dataType"]]  = "directory"
        ##input_list[["dataFilter"]][["dataType"]][["enum"]] = list("directory")
      } else if(data_input_list[[input_name]][["type"]] == "FILE"){
        input_list[["dataFilter"]] = list()
        ###input_list[["dataFilter"]][["dataType"]] = list()
        input_list[["dataFilter"]][["dataType"]] = "file"
        ###input_list[["dataFilter"]][["dataType"]][["enum"]] = list("file")
      }
      data_input_list_collection[[i+1]] = input_list
    }
  }
  parameter_list_collection = list()
  if(!empty_parameter_list){
    total_fields_idx = 0
    for(k in 1:length(names(full_parameter_list))){
      total_fields_idx = total_fields_idx + 1
      section_name = names(full_parameter_list)[k]
    section_list = list()
    section_list[["id"]] = paste(section_name,"section parameters")
    section_list[["type"]] = "section"
    section_list[["label"]] = section_name
    parameter_list = full_parameter_list[[section_name]]
    parameter_list_collection[[total_fields_idx]] = section_list
    for(i in 1:length(names(parameter_list))){
      field_list = list()
      parameter_name = names(parameter_list)[i]
      field_list[["type"]] = convert_json_import_form_types(parameter_list[[parameter_name]][["type"]])
      field_list[["id"]] = parameter_name
      field_list_required = parameter_list[[parameter_name]][["required"]]
      field_list[["helpText"]] = parameter_list[[parameter_name]][["description"]]
      field_list[["label"]] = parameter_list[[parameter_name]][["label"]]
      field_list_multi_value = parameter_list[[parameter_name]][["multiValue"]]
      if(!is.null(field_list_multi_value)){
        if(!field_list_multi_value){
          field_list_multi_value = NULL
        }
      }
      field_list_default_value = parameter_list[[parameter_name]][["value"]]
      ### make sure we collect choises for options that can be selected
      if(field_list[["type"]] == "select"){
        choices_list = list()
        options_to_pick = parameter_list[[parameter_name]][["options"]]
        for(o_idx in 1:length(options_to_pick)){
          option_list = list()
          option_list[["text"]] = options_to_pick[o_idx]
          option_list[["value"]] = options_to_pick[o_idx]
          if(o_idx == 1){
            option_list[["selected"]] = as.logical("true")
          }
          choices_list[[o_idx]] = option_list
        }
        field_list[["choices"]] = choices_list
      }
      ### convert strings to numeric values
      if(field_list[["type"]] == "number" | field_list[["type"]] == "integer"){
        field_list_default_value = as.numeric(field_list_default_value)
      }
      #### convert string to boolean values
      if(field_list[["type"]] == "checkbox"){
        if(grepl("true",field_list_default_value,ignore.case = T)){
          field_list_default_value = TRUE
        } else{
          field_list_default_value = FALSE
        }
        field_list_default_value = as.logical(tolower(field_list_default_value))
      }
      
      
      field_list[["minValues"]] = 0
      if(!is.null(field_list_required)){
        if(field_list_required == "true"){
          field_list[["minValues"]] = 1
        } else{
          field_list[["minValues"]] = 0
        }
      }
      if(!is.null(field_list_multi_value)){
        if(field_list_multi_value == "true"){
          field_list[["maxValues"]] = 999
        } else{
          field_list[["maxValues"]] = 1
        }
      } else{
        field_list[["maxValues"]] = 1
      }
      if(!is.null(field_list_default_value) & !is.na(field_list_default_value)){
        if(field_list_default_value != "STRING" & field_list_default_value != ""){
          if(!is.null(field_list_multi_value)){
            field_list[["value"]] = list(field_list_default_value)
          } else{
            if(is.na(as.logical(field_list_default_value))){
              field_list[["value"]] = field_list_default_value
            } else if(!is.na(strtoi(field_list_default_value))){
              field_list[["value"]] = strtoi(field_list_default_value)
            } else{
              field_list[["value"]] = as.logical(field_list_default_value)
            }
          }
        } else{
          if(!is.null(field_list_multi_value)){
          field_list[["value"]] = list()
          } else{
            field_list[["value"]] = "STRING"
          }
        }
      } else{
        if(!is.null(field_list_multi_value)){
          field_list[["value"]] = c()
        } else{
          field_list[["value"]] = "STRING"
        }
      }
      total_fields_idx = total_fields_idx + 1
      parameter_list_collection[[total_fields_idx]] = field_list
    }  
    }
  }
  if(length(parameter_list_collection) > 0 & length(data_input_list_collection) > 0){
    full_collection = c(data_input_list_collection,parameter_list_collection)
  } else if(length(parameter_list_collection) > 0 & length(data_input_list_collection) == 0){
    full_collection = parameter_list_collection
  } else if(length(parameter_list_collection) == 0 & length(data_input_list_collection) > 0){
    full_collection = data_input_list_collection
  } else{
    rlog::log_warn(paste("Did not generate template. No parameters and inputs found"))
    rlog::log_warn(paste("EMPTY_DATA_INPUTS:",empty_data_inputs,"EMPTY_PARAMETER_LIST:",empty_parameter_list))
    return(FALSE)
  }
  for(idx in 1:length(full_collection)){
    print(paste("IDX:",idx,"VAL:",full_collection[idx]))
    input_form[[idx]] = full_collection[[idx]]
  }
  ###setNames(input_form,character(0))
  input_form_final_list = list()
  input_form_final_list[["fields"]] = input_form
  input_form_JSON = jsonlite::toJSON(input_form_final_list,pretty=TRUE,auto_unbox=TRUE,flatten = TRUE)
  rlog::log_info(paste("Writing input form JSON to",file_output))
  write(input_form_JSON,file=file_output)
  return(TRUE)
}

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
  template_list[["field-data"]] = data_input_list
  template_list[["field"]] = list()
  for(i in 1:length(names(parameter_list))){
    template_list[["field"]] = append(template_list[["field"]],parameter_list[[names(parameter_list)[i]]])
  }
  if(!empty_parameter_list | !empty_data_inputs){
    template_JSON = jsonlite::toJSON(template_list)
    rlog::log_info(paste("Writing template to",file_output))
    jsonlite::write_json(template_list,path=file_output,pretty=T,auto_unbox=T)
    return(TRUE)
  } else{
    rlog::log_warn(paste("Did not generate template. No parameters or inputs found"))
    rlog::log_warn(paste("EMPTY_DATA_INPUTS:",empty_data_inputs,"EMPTY_PARAMETER_LIST:",empty_parameter_list))
    return(FALSE)
  }
  
}
cli_preview <- function(json_template,workflow_language){
  boilerplate_cli_prefix = c("icav2 projectpipelines","start",paste(workflow_language,"Json",sep=""),"PIPELINE_NAME")
  boilerplate_cli_suffix = c("--storage-size Small","--project-id PROJECT_ID")
  cli_interstitial = c()
  json_template_data = rjson::fromJSON(file=json_template)
  for(i in 1:length(names(json_template_data))){
    cli_prefix = names(json_template_data)[i]
    keys_to_add = names(json_template_data[[cli_prefix]])
    if(length(keys_to_add) > 0){
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
        #if(grepl("\\.|\\-",key_value) & !grepl("fil",key_value) & !grepl("fol",key_value) & !grepl("'",key_value)){
        #  key_value = ke
        ##}
        if(grepl(" ",key_value)){
          key_value = paste("'",key_value,"'",sep="")
        }
        
        string_to_add = c()
        if(!is.null(key_value) & key_value != ""){
          string_to_add = paste(paste("--",cli_prefix,sep=""),paste(key_name,":",key_value,sep=""),collapse = " ",sep = " ")
        } else{
          ##rlog::log_warn(paste("Not adding:",key_name,"to the CLI stub"))
          if(grepl("data",cli_prefix)){
            key_value = "DATA_ID1,DATA_ID2,...,DATA_IDxx"
          } else{
            key_value = "STRING"
          }
          string_to_add = paste(paste("--",cli_prefix,sep=""),paste(key_name,":",key_value,sep=""),collapse = " ",sep = " ")
        }
        #rlog::log_info(paste("ADDING:",string_to_add))
        if(length(string_to_add) > 0){
          cli_interstitial = c(cli_interstitial,paste(string_to_add,"\\\n"))
        }
      }
    }
  }
  final_string_components = c(boilerplate_cli_prefix,"\\\n",cli_interstitial,boilerplate_cli_suffix)
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
                    default=NULL, help = "output JSON generated to be used as template for ICA CLI snippet")
parser$add_argument("-t","--template-json","--template_json",
                   default=NULL, help = "template file to get CLI command")
args <- parser$parse_args()
parameter_xml_file = args$parameters_xml
only_required = args$only_required
output_json = args$output_json
workflow_language = args$workflow_language[1]
template_json = args$template_json
#template_json = NULL
if(is.null(template_json)){
  # don't create template JSON if template JSON is provided.
  if(is.null(output_json)){
    output_json = gsub(".xml$",".ica_input_form.json",parameter_xml_file)
    output_json = gsub("\\.pipeline","",output_json)
    rlog::log_info(paste("Saving ICA JSON input form to: ",output_json))
  }
  #########################################
  rlog::log_info(paste("PARSING parameters XML file:",parameter_xml_file))
  doc = xmlToList(parameter_xml_file)
  ##### Grab all dataInputs from XML and output to list ---- 
  data_input_names = doc[["dataInputs"]]
  if(length(data_input_names)>0){
    final_data_input_list = data_inputs_to_list(data_input_names)
    final_input = data_list_for_template(final_data_input_list,only_required)
  } else{
    final_input = list()
  }
  ###### Grab all parameters from XML under each tool and output to list ----
  tool_names = doc[["steps"]]
  final_params = list()
  if(length(tool_names) > 0 ){
    for(i in 1:length(tool_names)){
      tool_section_name = tool_names[[i]]$label
      final_tool_params_list = parameters_to_list(tool_names[i])
      #print(final_tool_params_list)
      final_params_section = param_list_for_template(final_tool_params_list,only_required)
      final_params[[tool_section_name]] = final_params_section
    }  
  } else{
    final_params = list()
  }
  #print(final_params)
  #### create output json
  create_json_input_form(final_input,final_params,output_json)
  output_json = gsub(".xml$",".ica_cli_template.json",parameter_xml_file)
  output_json = gsub("\\.pipeline","",output_json)
  rlog::log_info(paste("Saving ICA JSON input form to: ",output_json))
  JSON_TEMPLATE_CREATED = create_json_template(final_input,final_params,output_json)
  if(JSON_TEMPLATE_CREATED){
    CLI_PREVIEW = cli_preview(json_template=output_json,workflow_language=workflow_language)
    rlog::log_info(paste("CLI_COMMAND_TEMPLATE: \n",CLI_PREVIEW))
  } else{
    rlog::log_warn(paste("NO template generated at:",output_json))
  }
} else{
  CLI_PREVIEW = cli_preview(json_template=template_json,workflow_language=workflow_language)
  rlog::log_info(paste("CLI_COMMAND_TEMPLATE: \n",CLI_PREVIEW))
}
