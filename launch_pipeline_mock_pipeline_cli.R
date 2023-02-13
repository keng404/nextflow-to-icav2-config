options(stringsAsFactors=FALSE)
suppressPackageStartupMessages(library("argparse"))
library(rlog)
library(stringr)
library(XML)
source('parameter_xml_utils.R')
# create parser object
parser <- ArgumentParser()

# specify our desired options 
# by default ArgumentParser will add an help option 
parser$add_argument("-l", "--workflow-language","--workflow_language",default="nextflow",
                    help="Workflow language.Currently only cwl and nextflow are supported")
parser$add_argument("-r","--pipeline-run-name","--pipeline_run_name",default=NULL,
                    help = "Name of pipeline run --- for easier searching later")
parser$add_argument("-n","--pipeline-name","--pipeline_name",required=TRUE, 
                    help = "name of pipeline of interest")
parser$add_argument("-x","--parameters-xml","--parameters_xml",
                    default=NULL, help = " parameters XML file output",required=TRUE)
# get command line options, if help option encountered print help and exit,
# otherwise if options not found on command line then set defaults, 
args <- parser$parse_args()
xml_file = args$parameters_xml
if(!file.exists(xml_file)){
  stop(paste("Please define a valid path to the parameter XML file"))
}
# parse XML file
xml_doc =  xmlTreeParse(xml_file,addAttributeNamespaces = TRUE,useInternalNodes = TRUE)

rlog::log_info(paste("Looking for parameters in :",xml_file))
parameter_list = getParametersFromXML(xml_doc)

rlog::log_info(paste("Looking for dataInputs in :",xml_file))
data_input_list = getDataInputsFromXML(xml_doc)

rlog::log_info(paste("Drafting parameter template"))
parameter_template = craftCLIparameters(parameter_list)

rlog::log_info(paste("Drafting input template"))
data_input_template  = craftCLIdataInputs(data_input_list)

command_template = mockCLIcommand(workflow_language=args$workflow_language,pipeline_name=args$pipeline_name,user_reference=args$pipeline_run_name,input_cli=data_input_template,parameter_cli=parameter_template)
rlog::log_info(paste("Here is your command-line template to run this pipeline. Be sure to replace defaults and place-holders with values of your choosing"))
cat(command_template)