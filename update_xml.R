library(XML)
options(stringsAsFactors=FALSE)
suppressPackageStartupMessages(library("argparse"))
# create parser object
parser <- ArgumentParser()
parser$add_argument("-x","--parameters-xml","--parameters_xml", default=NULL,
                    required=TRUE, help = "parameters XML file")
args <- parser$parse_args()

parameter_xml_file = args$parameters_xml
data_inputs_to_add = c("input_files","project_dir")

rlog::log_info(paste("UPDATING parameters XML file:",parameter_xml_file))
doc = xmlTreeParse(parameter_xml_file,useInternalNodes = TRUE)
root = xmlRoot(doc)
dataInputsNode = root[["dataInputs"]]
data_input_names = xmlAttrs(root[["dataInputs"]][["dataInput"]])[["code"]]
tool_names = xmlAttrs(root[["steps"]][["step"]][["tool"]])[["code"]]
if("project_dir" %in% data_input_names){
  new_input_node_attributes = c(code = "project_dir",format = "UNKNOWN",type = "DIRECTORY",required = "true",multiValue = "true")  
  node_object = newXMLNode("dataInput",attrs=new_input_node_attributes,parent = dataInputsNode)
  newXMLNode("label", "project_dir", parent=node_object)
  newXMLNode("description", "directory with additional files/input to run pipeline --- other files in your github project", parent=node_object)
}
if("input_files" %in% data_input_names){
  new_input_node_attributes = c(code = "input_files",format = "UNKNOWN",type = "FILE",required = "true",multiValue = "true")  
  node_object = newXMLNode("dataInput",attrs=new_input_node_attributes,parent = dataInputsNode)
  newXMLNode("label", "input_files", parent=node_object)
  newXMLNode("description", "additional files/input to run pipeline --- other files in your github project", parent=node_object)
}

outputPath = gsub(".xml$",".updated.xml",parameter_xml_file)
rlog::log_info(paste("Updating parameters XML here:",outputPath))
#prefix='<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n'
prefix.xml <- "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n"
saveXML(doc , file=outputPath,encoding="utf-8")
xml_file = outputPath
system(paste("mv",outputPath,parameter_xml_file))
rlog::log_info(paste("Updating parameters XML to:",parameter_xml_file))