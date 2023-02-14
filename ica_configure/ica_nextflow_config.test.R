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

parser$add_argument("-c","--config-file","--config_file", default=NULL,required=TRUE,
                    help = "main config file")
parser$add_argument("-k","--is-simple-config","--is_simple_config",
                    action="store_true",default=FALSE, help = "Use config")
parser$add_argument("-i","--configs-to-hardcode","--configs_to_hardcode", default="params_to_inject.txt",
                    help="config params to inject")
parser$add_argument("-u","--instance-type-url","--instance_type_url", default="https://help.ica.illumina.com/project/p-flow/f-pipelines#compute-types",
                    help = "URL that contains ICA instance type table")
parser$add_argument("-a","--modules-config-file","--modules_config_file", default = NULL,
                    help = "configuration file for modules of a pipeline")
parser$add_argument("-t","--params-override-template","--params_override_template", default = "params_to_strip.txt",
                    help = "params to remove from nextflow.config")
parser$add_argument("-w","--config-template","--config_template", default = "template.modules.config",
                    help = "nextflow config_template file")
parser$add_argument("-e","--ica-config","--ica_config", default = "nextflow.ica.config",
                    help = "basename of ICA config file")
parser$add_argument("-f","--modules-config-relative-path","--modules_config_relative_path", default = "conf/modules.ica.config",
                    help = "nextflow modules_config_relative_path file")
# get command line options, if help option encountered print help and exit,
# otherwise if options not found on command line then set defaults, 
args <- parser$parse_args()
is_simple_config = args$is_simple_config
config_file = args$config_file
config_dat  = read.delim(config_file,quote="",header=F)
input_override = read.delim(args$input_files_override,header=F,quote="")
parameters_xml  = args$parameters_xml
configs_to_hardcode = args$configs_to_hardcode
params_override_template = args$params_override_template
ica_config = args$ica_config
generate_parameters_xml = args$generate_parameters_xml
modules_config_file = args$module_config_file
module_config_template = args$config_template
modules_config_relative_path = arg$modules_config_relative_path
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
##############################################

if(is_simple_config | is.null(modules_config_file)){
  # parse config file
  config_params = get_params_from_config(conf_file=config_file)
  config_lines = get_other_lines_from_config(conf_file=config_file)
  if(length(config_lines) < 1){
    config_lines = NULL
  }
  config_params_updated = strip_params(params_list = config_params,params_to_strip=params_override_template)
  config_params = inject_params(params_list = config_params_updated, params_to_inject=configs_to_hardcode)
  # generate ICA nextflow config file
  write_params(params_list = config_params,additional_lines = config_lines,output_file=ica_config)
  # copy ICA module template
  copy_modules_template(template_file=module_config_template,destination_dir=dirname(config_file))
  # add reference to your module config file
  add_module_reference(nextflow_config=ica_config,additional_config=modules_config_relative_path)
} else{
  # parse config file
  config_params = get_params_from_config(conf_file=config_file)
  config_lines = get_other_lines_from_config(conf_file=config_file)
  if(length(config_lines) < 1){
    config_lines = NULL
  }
  config_params_updated = strip_params(params_list = config_params,params_to_strip=params_override_template)
  config_params = inject_params(params_list = config_params_updated, params_to_inject=configs_to_hardcode)
  # generate ICA nextflow config file
  write_params(params_list = config_params,additional_lines = config_lines,output_file=ica_config)
  # add reference to your module config file
  add_module_reference(nextflow_config=ica_config,additional_config=modules_config_relative_path)
  # parse modules file
  module_configuration = loadModuleMetadata(c(modules_config_file))
  updated_module_configuration = override_module_config(module_list = module_configuration,ica_instance_table = ica_instance_table)
  write_modules(modules_list = updated_module_configuration,output_file=paste(dirname(config_file),modules_config_relative_path,sep="/"),template_file=module_config_template)
}