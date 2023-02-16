options(stringsAsFactors=FALSE)
library(argparse)
library(rlog)
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
##############################################

if(is_simple_config | is.null(base_config_files)){
  # parse config file
  config_params = get_params_from_config(conf_file=config_file)
  config_lines = get_other_lines_from_config(conf_file=config_file)
  if(length(config_lines) < 1){
    config_lines = NULL
  }
  config_params_updated = strip_params(params_list = config_params,params_to_strip=params_override_template)
  config_params = inject_params(params_list = config_params_updated, params_to_inject=configs_to_hardcode)
  # generate ICA nextflow config file
  write_params(params_list = config_params,additional_lines = config_lines,output_file=paste(dirname(config_file),ica_config,sep="/"))
  # copy ICA module template
  copy_modules_template(template_file=base_config_template,destination_dir=dirname(config_file))
  get_file_extension = strsplit(basename(base_config_template),"\\.")[[1]]
  base_config_relative_path = gsub(paste(".",get_file_extension[length(get_file_extension)],"$",sep="",collapse=""),".ica.config",base_config_file_path)
  
  # add reference to your module config file
  add_module_reference(nextflow_config=paste(dirname(config_file),ica_config,sep="/"),existing_module_file=NULL,additional_config=base_config_relative_path)
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
  write_params(params_list = config_params,additional_lines = config_lines,output_file=paste(dirname(config_file),ica_config,sep="/"))
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
    write_modules(modules_list = updated_base_configuration,output_file=paste(dirname(config_file),base_config_relative_path,sep="/"),template_file=base_config_template)
    add_module_reference(nextflow_config=paste(dirname(config_file),ica_config,sep="/"),existing_module_file=base_config_file_path,additional_config=base_config_relative_path)
    
  }
}
