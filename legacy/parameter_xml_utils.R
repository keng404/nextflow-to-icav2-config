library(XML)
library(rlog)
getParametersFromXML <- function(xml_doc){
  xml_subset = xmlToList(xml_doc)[["steps"]][["step"]][["tool"]]
  parameter_list = list()
  for(i in 1:length(xml_subset)){
    if("parameter" %in% names(xml_subset[i])){
      found_type = apply(t(names(xml_subset[i][["parameter"]])),2, function(elem) grepl("Type",elem))
      if(sum(found_type) > 0 ){
        parameter_type = names(xml_subset[i][["parameter"]])[found_type]
      } else{
        parameter_type = "string"
      }
      parameter_name = xml_subset[i][["parameter"]][[".attrs"]][["code"]]
      parameter_default_value = xml_subset[i][["parameter"]][["value"]]
      parameter_description = xml_subset[i][["parameter"]][["description"]]
      parameter_list[[parameter_name]][["default"]] = parameter_default_value
      parameter_list[[parameter_name]][["description"]] = parameter_description
      parameter_list[[parameter_name]][["parameter_type"]] = parameter_type
    }
  }
  if(length(names(parameter_list)) > 0){
    return(parameter_list)
  } else{
    return(NULL)
  }
}

getDataInputsFromXML <- function(xml_doc){
  xml_subset = xmlToList(xml_doc)[["dataInputs"]]
  data_input_list = list()
  for(i in 1:length(xml_subset)){
    if("dataInput" %in% names(xml_subset[i])){
      parameter_type =xml_subset[i][["dataInput"]][[".attrs"]][["type"]]
      parameter_name = xml_subset[i][["dataInput"]][[".attrs"]][["code"]]
      parameter_description = xml_subset[i][["dataInput"]][["description"]]
      data_input_list[[parameter_name]][["description"]] = parameter_description
      data_input_list[[parameter_name]][["type"]] = parameter_type
    }
  }
  if(length(names(data_input_list)) > 0){
    return(data_input_list)
  } else{
    return(NULL)
  }
}

craftCLIparameters <- function(parameter_list){
  parameter_names = c()
  for(i in 1:length(names(parameter_list))){
    parameter_names[i] = paste(names(parameter_list)[i],parameter_list[[names(parameter_list)[i]]][["default"]],sep=":")
  }
  if(length(parameter_names > 0)){
    params_cli = paste(apply(t(parameter_names),2,function(elem) paste("--parameters",elem)),collapse = " ")
    return(params_cli)
  } else{
    return("")
  }
}

craftCLIdataInputs <- function(input_list){
  input_names = c()
  for(i in 1:length(names(input_list))){
    input_names[i] = paste(names(input_list)[i],input_list[[names(input_list)[i]]][["type"]],sep=":")
  }
  if(length(input_names > 0)){
    input_cli = paste(apply(t(input_names),2,function(elem) paste("--input",elem)),collapse = " ")
    return(input_cli)
  } else{
    return("")
  }
}

mockCLIcommand <- function(workflow_language=NULL,pipeline_name=NULL,user_reference=NULL,input_cli,parameter_cli){
  workflow_language = tolower(workflow_language)
  tags = c('--reference-tag','--technical-tag', '--user-tag')
  if(is.null(workflow_language)){
    workflow_language = "nextflow"
  }
  if(!workflow_language %in% c("cwl","nextflow")){
    stop(paste("Please enter a valid workflow language: cwl or nextflow"))
  }
  if(is.null(pipeline_name)){
    stop(paste("Please define pipeline name"))
  }
  if(is.null(user_reference)){
    user_reference = "test_run"
  }
  tag_str = paste(tags,user_reference,collapse = " ")
  full_cli = paste("icav2 projectpipelines start",workflow_language,pipeline_name,"--user-reference",user_reference,input_cli,parameter_cli,tag_str,collapse = " ")
  full_cli = paste(full_cli,"[ OPTIONAL:","--x-api-key","API_KEY","--project-id","PROJECT_ID","]",collapse = " ")
  return(full_cli)
}