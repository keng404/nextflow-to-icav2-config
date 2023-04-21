smoke_test_in_config <- function(config_file){
 conf_data = t(read.delim(config_file,header=F,quote=""))
 smoke_test_in_config_bool = FALSE
 line_bool = apply(t(conf_data),2,function(x) grepl("includeConfig",x) & grepl("test",x))
 if(sum(line_bool) >0){
   smoke_test_in_config_bool = TRUE
 }
 return(smoke_test_in_config_bool)
}

ica_smoke_test_in_parameters_list <- function(parameter_xml){
  parameter_names = names(parameter_xml)
  found_ica_smoke_test = FALSE
  parameter_list = list()
  for(i in 1:length(parameter_names)){
    tools = parameter_xml[i][["step"]][["tool"]]
    for(j in 1:length(tools)){
      param_setting = tools[j]
      if("parameter" %in% names(param_setting)){
        rlog::log_info(paste("looking at",param_setting))
        parameter_name = param_setting[["parameter"]][[".attrs"]][["code"]]
        if(parameter_name == "ica_smoke_test"){
          found_ica_smoke_test  = TRUE
        }
      }
    }
  }
  return(found_ica_smoke_test)
}


add_ica_smoke_test_to_parameters_list <- function(xml_file){
  doc = xmlTreeParse(parameter_xml_file,useInternalNodes = TRUE)
  root = xmlRoot(doc)
  ###### Grab all parameters from XML under each tool and output to list ----
  tools = root[["steps"]][[1]][["tool"]]
  #######
  nested_parameter_node = XML::newXMLNode("parameter",parent=tools)
  xmlAttrs(nested_parameter_node) = c(code = "ica_smoke_test",minValues = "0",maxValues="1",classification="USER")
  newXMLNode("label","ica_smoke_test",parent=nested_parameter_node)
  newXMLNode("description","Boolean to trigger smoke test:",parent=nested_parameter_node)
  # default params.ica_smoke_test false, can be triggered to true later
  XML::newXMLNode(paste("booleanType"),parent=nested_parameter_node)
  newXMLNode("value","false",parent=nested_parameter_node)
  outputPath = gsub(".xml$",".updated.xml",xml_file)
  #rlog::log_info(paste("Updating parameters XML here:",outputPath))
  #prefix='<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n'
  prefix.xml <- "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n"
  saveXML(doc , file=outputPath,encoding="utf-8")
  system(paste("mv",outputPath,xml_file))
  rlog::log_info(paste("Updating parameters XML to:",xml_file))
}