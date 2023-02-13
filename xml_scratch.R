library(XML)
library(rlog)
dummy_xml = '/Users/keng/nf-core/rnaseq/rnaseq.pipeline.updated.xml'
doc = xmlTreeParse(dummy_xml,addAttributeNamespaces = TRUE,useInternalNodes = TRUE)
root = xmlRoot(doc)
dataInputsNode = root[["dataInputs"]]
parametersInputNode = root[["steps"]]
new_input_node_attributes = c(code = "project_dir",format = "UNKNOWN",type = "FOLDER",required = "true",multiValue = "true")  

node_object = newXMLNode("pd:dataInput",attrs=new_input_node_attributes,parent = dataInputsNode)
newXMLNode("pd:label", "project_dir", parent=node_object)
newXMLNode("pd:description", "directory with additional files/input to run pipeline --- other files in your github project", parent=node_object)

outputPath = gsub(".xml$",".updated.xml",dummy_xml)
rlog::log_info(paste("Updating parameters XML here:",outputPath))
#prefix='<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n'
prefix.xml <- "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n"
saveXML(doc , file=outputPath,encoding="utf-8")


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
    params_cli = paste(apply(t(parameter_names),2,function(elem) paste("--parameter",elem)),collapse = " ")
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