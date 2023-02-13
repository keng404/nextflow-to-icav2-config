options(stringsAsFactors=FALSE)
suppressPackageStartupMessages(library("argparse"))
library(rlog)
library(stringr)
library(rvest)
source('reading_utils.R')
source('writing_utilities.R')
source('functional_utilities.R')
# create parser object
parser <- ArgumentParser()

# specify our desired options 
# by default ArgumentParser will add an help option 

parser$add_argument("-s", "--nf-script","--nf_script", default=NULL, required=TRUE,
                    help="Main NF script")
parser$add_argument("-c","--config-file","--config_file", default=NULL,required=TRUE,
                    help = "main config file")
parser$add_argument("-e", "--dsl2-enabled","--dsl2_enabled", action="store_true",default=FALSE,
                    help="Create pipeline in ICA")
parser$add_argument("-f","--nf-core-mode","--nf_core_mode",action="store_true",
                    default=FALSE, help = "flag to indicate nf-core pipeline")
parser$add_argument("-x","--parameters-xml","--parameters_xml",
                    default=NULL, help = " parameters XML file output")
parser$add_argument("-k","--is-simple-config","--is_simple_config",
                    action="store_true",default=FALSE, help = "Use config")
parser$add_argument("-g","--generate-parameters-xml","--generate_parameters_xml",
                    action="store_true",default=FALSE, help = "Generate parameters XML file")
parser$add_argument("-i","--configs-to-ignore","--configs_to_ignore", default=c(""),nargs="?",
                    action="append",help="config files to ignore")
parser$add_argument("-u","--instance-type-url","--instance_type_url", default="https://help.ica.illumina.com/project/p-flow/f-pipelines#compute-types",
                    help = "URL that contains ICA instance type table")
parser$add_argument("-d","--default-instance","--default_instance", default = "himem-small",
                    help = "default instance value")
parser$add_argument("-a","--modules-config-file","--modules_config_file", default = NULL,
                    help = "configuration file for modules of a pipeline")
parser$add_argument("-m","--dummy-docker-image","--dummy_docker_image", default = "library/ubuntu:21.04",
                    help = "default Docker image to copy intermediate and report files from ICA")
parser$add_argument("-t","--intermediate-copy-template","--intermediate_copy_template", default = "dummy_template.txt",
                    help = "default NF script to copy intermediate and report files from ICA")
parser$add_argument("-w","--input-files-override","--input_files_override", default = "input_override.non_dsl2_pipelines.txt",
                    help = "default stub to override input")
parser$add_argument("-n","--ica-instance-namespace","--ica_instance_namespace", default="scheduler.illumina.com/presetSize",
                    help = "ICA instance type namespace : will allow ICA scheduler to know  what instances to use")
# get command line options, if help option encountered print help and exit,
# otherwise if options not found on command line then set defaults, 
args <- parser$parse_args()
is_simple_config = args$is_simple_config
nf_script = args$nf_script
config_file = args$config_file
config_dat  = read.delim(config_file,quote="",header=F)
input_override = read.delim(args$input_files_override,header=F,quote="")
parameters_xml  = args$parameters_xml
configs_to_ignore = args$configs_to_ignore
configs_to_ignore = configs_to_ignore[configs_to_ignore!=""]
generate_parameters_xml = args$generate_parameters_xml
if(!is.null(parameters_xml)){
  generate_parameters_xml = TRUE
}
###########
ica_instance_namespace = args$ica_instance_namespace
default_instance = args$default_instance
instance_type_table_url = args$instance_type_url


###############

z = getParamsFromConfig(conf_data=config_dat)

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

rlog::log_warn(paste("Number of closures in config",config_file,sum(grepl("\\{",t(config_dat)))))
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
configs_to_ignore = z1[ apply(t(z1),2,function(x) sum(strsplit(x,'/')[[1]] == "") > 1) ]
rlog::log_info(paste("CONFIGS_TO_IGNORE:",configs_to_ignore))
if(length(configs_to_ignore) > 0 && !is.null(z1)){
  z1 = z1[ !(z1 %in% configs_to_ignore)]
}
rlog::log_info(paste("CONFIGS_FOUND:",z1))
final_config_list = c()
if(length(z1) > 0){
  final_config_list = localConfigOrNot(paramsFiller(list_to_fill=z1,params_list=z))
} else{
  final_config_list = NULL
}
#############################
#### for DSL2 workflows ... find module config file and load in the configruation metadata.
#### For now we hard-code 'modules.config' by nf-core convention .... may expose this as command-line parameter
############################################
modules_config_file = NULL
modules_config = list()
is_module_config_file = NULL
if(!is.null(z1)){
  is_module_config_file = apply(t(z1),2,function(elem) basename(elem) == "modules.config")
}
# STEP1: Grab nextflow config and propagate to all configs to grab the appropriate params
paramCollection = list()
if(length(final_config_list) > 0 ){
  for(config_idx in 1:length(final_config_list)){
    key_name = strsplit(basename(final_config_list[config_idx]),"\\.")[[1]][1]
    current_file = final_config_list[config_idx]
    rlog::log_info(paste("Reading in",current_file))
    parsedParams  = getParamsFromConfig(conf_data=read.delim(current_file,header=F))
    if(length(parsedParams) > 0) {
      paramCollection[[key_name]] = parsedParams
    }
  }
}
compareConfigs <- function(defaultConfig, otherConfigs){
  updatedConfig = defaultConfig
  configs_to_check  = names(otherConfigs)
  default_config_params = names(defaultConfig)
  params_added = 0
  for(i in 1:length(configs_to_check)){
    optional_config_params = names(otherConfigs[[configs_to_check[i]]])
    params_to_add = optional_config_params[!(optional_config_params %in% default_config_params)]
    if(length(params_to_add) > 0){
      rlog::log_info(paste("Adding params",paste(params_to_add,collapse = ", ",sep=", ")))
      params_added = params_added + length(params_to_add)
      for(j in 1:length(params_to_add)){
        updatedConfig[[params_to_add[j]]] = otherConfigs[[configs_to_check[i]]][[params_to_add[j]]]
      }
    }
  }
  rlog::log_info(paste("Added",params_added,"params"))
  return(updatedConfig)
}
# compare param_collection with z
if(length(paramCollection) > 0){
  y = compareConfigs(defaultConfig = z,otherConfigs = paramCollection)
} else{
  y = z
}


####################
createDummyValue <- function(param_value,param_type){
  param_value1 = param_value
  if(is.na(param_value) || is.null(param_value) || param_value == ""){
    if(param_type == "boolean"){
      param_value1 = "false"
    } else if(param_type == "integer"){
      param_value1 = 0
    } else{
      param_value1 = "null"
    }
  }
  return(param_value1)
}
##########################################
classifyParameters <- function(paramsToXML){
  xmlSections = list()
  xmlSections[["dataInputs"]] = list()
  xmlSections[["parameterSettings"]] = list()
  xmlSections[["parameterSettings"]][["general"]] = list()
  for(i in 1:length(names(paramsToXML))){
    param_name = names(paramsToXML)[i]
    rlog::log_info(paste("LOOKING into",param_name))
    param_value = paramsToXML[[param_name]]
    param_final_name = gsub("params\\.","",param_name)
    if(grepl("\\$\\{params\\.",param_value)){
      rlog::log_info(paste("LOOKING up",param_name,"PARAM_VALUE:",param_value))
      param_value = paramsFiller(list_to_fill=c(param_value),paramsToXML)[1]
      #rlog::log_info(paste("VALUE is",param_value))
    }
    parameter_type = 'string'
    description = paste(parameter_type,"that defines",param_name)
    parameter_metadata = list()
    filename = basename(param_value)
    aram_value = gsub("'", "", param_value)
    # initial algo will determine if parameter is string, int, boolean, or path
    if(grepl("/",param_value) || grepl("input",param_name)){
      split_param_value = strsplit(param_value,"/")[[1]]
      rlog::log_info(paste("CHECKING if",param_name,"is file or folder. value:",param_value))
      if((length(split_param_value) > 2 || grepl("input",param_name)) && param_value != "/" && !grepl("https://",param_value)  && !grepl("s3://",param_value) && !grepl("gs://",param_value) && !grepl("core.windows.net",param_value)){
        parameter_type = 'data'
        # if param value contains s3://, *.core.windows.net, or gs://, ignore, we won't put this in the XML
        # if path and contains file extension ('.') --- it's a file, if not it's a dir.
        #  or if the param contains 'dir' 
        data_type = "FILE"
        if(grepl("\\.",filename)){
          data_type = "FILE" 
        } 
        if(grepl("dir",param_name,ignore.case = TRUE)){
          data_type = "FOLDER"
        }
        if(!grepl("\\*",param_value) && !grepl("\\{",param_value) && !(grepl("\\[",param_name)) && !(grepl("\\$\\{",paramsToXML[[param_name]]))){
          rlog::log_info(paste("ADDING",param_name,"to dataInputs"))
          description = paste("Path",parameter_type,"that defines",data_type,param_name)
          parameter_metadata[["description"]] = description
          xmlSections[["dataInputs"]][[param_final_name]] = parameter_metadata
        } else{
          rlog::log_info(paste("ADDING",param_name,"to parameterSettings"))
          description = paste(parameter_type,"that defines",data_type,param_name)
          parameter_metadata[["description"]] = description
          parameter_metadata[["type"]] = parameter_type
          if(param_name != "params.help"){
            xmlSections[["parameterSettings"]][["general"]][[param_final_name]] = parameter_metadata
          }
        }
      }
      # all others will be put in options
      
    } else{
      if(!grepl("\\$\\{",param_value) && !(grepl("\\[",param_name))){
        if(!is.na(strtoi(param_value))){
          parameter_type = "integer"
          param_value = strtoi(param_value)
        }
        if(grepl("true",param_value,ignore.case=T) || grepl("false",param_value,ignore.case=T)){
          parameter_type = "boolean"
        }
        param_value = gsub("'","",param_value)
        parameter_metadata[["description"]] = description
        parameter_metadata[["type"]] = parameter_type
        parameter_metadata[["default"]] = param_value
        if(param_name != "params.help"){
          xmlSections[["parameterSettings"]][["general"]][[param_final_name]] = parameter_metadata
        }
      }
    }
    if(parameter_type == "string"){
      if(!grepl("\\$\\{",param_value) && !(grepl("\\[",param_name))){
        if(!is.na(strtoi(param_value))){
          parameter_type = "integer"
          param_value = strtoi(param_value)
        }
        if(grepl("true",param_value,ignore.case=T) || grepl("false",param_value,ignore.case=T)){
          parameter_type = "boolean"
        }
        param_value = createDummyValue(param_value=param_value,param_type=parameter_type)
        parameter_metadata[["description"]] = description
        parameter_metadata[["type"]] = parameter_type
        parameter_metadata[["default"]] = param_value
        if(param_name != "params.help"){
          xmlSections[["parameterSettings"]][["general"]][[param_final_name]] = parameter_metadata
        }
      }
    }
  }
  return(xmlSections)
}

if(generate_parameters_xml){
  library(XML)
  rlog::log_info(paste("parameters found:",names(y)))
  getXMLSections = classifyParameters(paramsToXML = y)
  data_input_configurations = getXMLSections[["dataInputs"]]
  if(args$nf_core_mode){
    #if(!"input" %in% names(data_input_configurations) || length(names(data_input_configurations)) == 0){
    ########### workaround add input files --- will not be used by pipeline , but by ICA to stage the data
    data_input_configurations[["input_files"]] = list()
    data_input_configurations[["input_files"]][["description"]] = 'input files for pipeline.\nAll files will be staged in workflow.launchDir'
    #}
  }
  #####################
  step_configurations = getXMLSections[["parameterSettings"]]
  rlog::log_info(paste("STEP3:Generating ICA XML based off of",nf_script))
  doc = newXMLDoc()
  #root = xmlRoot(doc)                                      # FIND ROOT
  #pipeline_node = newXMLNode("pipeline",parent=root)
  root = newXMLNode("pipeline",doc=doc)
  xmlAttrs(root) = c(code=paste(basename(dirname(nf_script)),"pipeline"),version="1.0",xmlns="xsd://www.illumina.com/ica/cp/pipelinedefinition")
  # add data inputs
  # create new sections for steps
  ## then add options for each section
  ############
  #code="left" format="FASTQ" type="FILE" required="true" multiValue="false"
  #<pd:dataInput code="ref" format="FASTA" type="FILE" required="true" multiValue="false">
  #  <pd:label>ref</pd:label>
  #  <pd:description>the value for File transcriptome</pd:description>
  #  </pd:dataInput>
  ############
  #############################
  #### add data inputs
  rlog::log_info(paste("STEP3a: Adding dataInputs"))
  dataInputsNode = newXMLNode("dataInputs",parent=root)
  if(length(data_input_configurations) >0){
    for(i in 1:length(names(data_input_configurations))){
      dataInputNode = newXMLNode("dataInput",parent=dataInputsNode)
      if(grepl("folder",data_input_configurations[[names(data_input_configurations)[i]]][["description"]],ignore.case=T)){
        xmlAttrs(dataInputNode) = c(code = names(data_input_configurations)[i] ,format = "UNKNOWN",type = "DIRECTORY",required = "true",multiValue = "true")   
      } else{
        if(names(data_input_configurations)[i]  == "input_files" && data_input_configurations[[names(data_input_configurations)[i]]][["description"]] == 'input files for pipeline.\nAll files will be staged in workflow.launchDir'){
          xmlAttrs(dataInputNode) = c(code = names(data_input_configurations)[i] ,format = "UNKNOWN",type = "FILE",required = "false",multiValue = "true")   
        } else{
          xmlAttrs(dataInputNode) = c(code = names(data_input_configurations)[i] ,format = "UNKNOWN",type = "FILE",required = "true",multiValue = "true")   
        }
      }
      newXMLNode("label", names(data_input_configurations)[i], parent=dataInputNode)
      newXMLNode("description", data_input_configurations[[names(data_input_configurations)[i]]][["description"]], parent=dataInputNode)
    }
  } else{
    rlog::log_warn(paste("STEP3a: No dataInputs found"))
  }
  ############## add parameter options
  ## <pd:step execution="MANDATORY" code="General">
  #<pd:label>General</pd:label>
  #  <pd:description>General parameters</pd:description>
  #  <pd:tool code="generalparameters">
  #  <pd:label>generalparameters</pd:label>
  #  <pd:description>General Parameters</pd:description>
  
  #code="threads" minValues="1" maxValues="1" classification="USER"
  #
  #                <pd:parameter code="threads" minValues="1" maxValues="1" classification="USER">
  #                    <pd:label>threads</pd:label>
  #                    <pd:description>the value for threads</pd:description>
  #                    <pd:integerType/>
  #                    <pd:value>8</pd:value>
  #                </pd:parameter>
  ############  
  ########################
  rlog::log_info(paste("STEP3b: Adding parameter options"))
  stepsNode = newXMLNode("steps",parent=root)
  if(length(step_configurations)>0){
    for(i in 1:length(names(step_configurations))){
      initial_step_node = newXMLNode("step",parent=stepsNode)
      xmlAttrs(initial_step_node) = c(execution = "MANDATORY",code = names(step_configurations)[i])    
      step_label_node  = newXMLNode("label",names(step_configurations)[i],parent=initial_step_node)
      description_label_node  = newXMLNode("description",paste(names(step_configurations)[i],"parameters"),parent=initial_step_node)
      tool_description_node = newXMLNode("tool",parent=initial_step_node)
      xmlAttrs(tool_description_node) = c(code=paste(names(step_configurations)[i],"parameters"))
      nested_step_label_node  = newXMLNode("label",names(step_configurations)[i],parent=tool_description_node)
      nested_description_label_node  = newXMLNode("description",paste(names(step_configurations)[i],"parameters"),parent=tool_description_node)
      parameter_names = names(step_configurations[[names(step_configurations)[i]]])
      for(j in 1:length(parameter_names)){
        ##  input override for params.input
        if(parameter_names[j] == "input"){
          parameter_names[j] = "input_string"
        }
        parameter_metadata = step_configurations[[names(step_configurations[i])]][[parameter_names[j]]]
        nested_parameter_node = newXMLNode("parameter",parent=tool_description_node)
        xmlAttrs(nested_parameter_node) = c(code = names(step_configurations[[names(step_configurations)[i]]])[j],minValues = "1",maxValues="1",classification="USER")
        newXMLNode("label",parameter_names[j],parent=nested_parameter_node)
        newXMLNode("description",parameter_metadata[["description"]],parent=nested_parameter_node)
        if(parameter_names[j] %in% options_to_add){
          list_vals = options_to_add[[parameter_names[j]]]
          if(length(list_vals) > 0){
            options_node = newXMLNode(paste("optionsType"),parent=nested_parameter_node)
            for(lv in 1:length(list_vals)){
              newXMLNode("option",list_vals[lv],parent=options_node)
            }
          }
        } else{
          if(grepl("number",parameter_metadata[["type"]],ignore.case = T)){
            newXMLNode(paste("integer","Type",sep=""),parent=nested_parameter_node)
          } else{
            newXMLNode(paste(paste(parameter_metadata[["type"]],"Type",sep=""),sep=""),parent=nested_parameter_node)
          }
          if("default" %in% names(parameter_metadata)){
            newXMLNode("value",parameter_metadata[["default"]],parent=nested_parameter_node)
          } else{
            newXMLNode("value",createDummyValue(param_value ="" ,param_type = parameter_metadata[["type"]]),parent=nested_parameter_node)
          }
        }
      }
    }
  } else{
    rlog::log_warn(paste("STEP3a: No parameters found"))
  }
  # VIEW XML
  #print(doc)
  
  # SAVE XML TO FILE
  outputFile = paste(basename(dirname(nf_script)), "pipeline","xml",sep=".")
  if(!is.null(parameters_xml)){
    outputPath = parameters_xml
  } else{
    outputPath = paste(dirname(nf_script),"/",outputFile,sep="")
  }
  rlog::log_info(paste("STEP4: Generating parameters XML to",outputPath))
  saveXML(doc, file=outputPath,encoding="utf-8")
}