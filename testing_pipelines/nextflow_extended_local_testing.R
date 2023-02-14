options(stringsAsFactors=FALSE)
suppressPackageStartupMessages(library("argparse"))
library(rlog)
library(stringr)
library(XML)
source('parameter_xml_utils.R')
# create parser object
parser <- ArgumentParser()
parser$add_argument("-x","--parameters-xml-file","--parameters_xml_file",
                    default=NULL, help = " parameters XML file output",required=TRUE)
parser$add_argument("-i","--input-file-format","--input_file_format",
                    default="FASTQ", help = "input file type",required=FALSE)
parser$add_argument("-s", "--nextflow-script","--nextflow_script", default = NULL, required = TRUE,
                    help="input nf-core pipeline script")
parser$add_argument("-d", "--docker-image","--docker_image", default ="nextflow/nextflow:20.10.0", required = FALSE,
                    help="docker image")
parser$add_argument("-t", "--testing-root-folder","--testing_root_folder", default = Sys.getenv("HOME") , required = FALSE,
                    help="folder to stage tests")
# step 1 : parse XML 
################
# test nextflow run (pipeline) prior to running on ICA ---- not just main script:
#   - parse XML for parameters
# get command line options, if help option encountered print help and exit,
# otherwise if options not found on command line then set defaults, 
args <- parser$parse_args()
parameters_xml_file = args$parameters_xml_file
input_file_format = args$input_file_format
nextflow_script = args$nextflow_script
docker_image = args$docker_image
test_root_folder = args$testing_root_folder
###################
## INPUT CHECK
if(!file.exists(parameters_xml_file)){
  stop(paste("Please define a valid path to th4e parameter XML file"))
}
#########
createDummyInputs <- function(my_input_list){
  my_dummy_inputs = list()
  for(i in 1:length(names(my_input_list))){
    rlog::log_info(paste("Param_name:",names(my_input_list)[i]))
    if(my_input_list[[names(my_input_list)[i]]][["type"]] != "DIRECTORY"){
      files_to_add = c()
      if(names(my_input_list)[i] == "input_files"){
        for(new_file in 1:10){
          files_to_add = c(files_to_add,paste(paste("dummy_file_",names(my_input_list)[i],"_",new_file,sep=""),sep="/"))
        }
      } else if(names(my_input_list)[i] == "input"){
        files_to_add = c(files_to_add,my_input_list[["input"]])
        
      } else{
        files_to_add = c(files_to_add,names(my_input_list)[i])
      }
      my_dummy_inputs[[names(my_input_list)[i]]] = files_to_add
    } else{
      dirs_to_add = c()
      for(new_dir in 1:10){
        dirs_to_add = c(dirs_to_add,paste(names(my_input_list)[i],paste("dummy_dir_",names(my_input_list)[i],"_",new_dir,sep=""),sep="/"))
      }
      my_dummy_inputs[[names(my_input_list)[i]]] = dirs_to_add
    }
  }
  return(my_dummy_inputs)
}
stageInputs <- function(my_dummy_inputs){
  for(i in 1:length(names(my_dummy_inputs))){
    file_paths_of_interest = my_dummy_inputs[[names(my_dummy_inputs)[i]]] 
    for(j in 1:length(file_paths_of_interest)){
      file_path_of_interest = file_paths_of_interest[j]
      dir_of_interest  = dirname(file_path_of_interest)
      if(!dir.exists(dir_of_interest)){
        rlog::log_info(paste("Creating directory:",dir_of_interest))
        #system(paste("mkdir",dir_of_interest))
      } else{
        rlog::log_info(paste("No need to create directory:",dir_of_interest))
      }
      if(!file.exists(file_path_of_interest)){
        rlog::log_info(paste("Creating file:",file_path_of_interest))
        #system(paste("touch",file_path_of_interest))
      } else{
        rlog::log_info(paste("No need to create file:",file_path_of_interest))
      }
    }
  }
}
generateFakeInputFiles <- function(my_dummy_inputs,number_of_samples_to_stage=10,format_type){
  if(format_type %in% c("fastq","FASTQ")){
    for(i in 1:number_of_samples_to_stage){
      file_name = paste("sample_",i,"_S",i,"_R1_001.fastq.gz",sep="")
      my_dummy_inputs[[file_name]] = file_name
      my_dummy_inputs[[file_name]][["type"]] = "FILE"
      file_name = paste("sample_",i,"_S",i,"_R2_001.fastq.gz",sep="")
      my_dummy_inputs[[file_name]] = file_name
      my_dummy_inputs[[file_name]][["type"]] = "FILE"
    }
  } else{
    for(i in 1:number_of_samples_to_stage){
      file_name = paste("sample_",i,".",tolower(format_type),sep="")
      my_dummy_inputs[[file_name]] = file_name
      my_dummy_inputs[[file_name]][["type"]] = "FILE"
    }
  }
  return(my_dummy_inputs)
}

createTestingDir <- function(xml_file){
  pipeline_name = strsplit(basename(xml_file),"\\.")[[1]][1]
  testing_dir = paste(pipeline_name,"testing",format(Sys.time(), "%d_%m_%y_%H_%M_%S"),sep="_")
  testing_dir = paste(test_root_folder,testing_dir,sep="/")
  if(!dir.exists(testing_dir)){
    rlog::log_info(paste("Creating Directory : " ,testing_dir))
    dir.create(testing_dir)
  }  else{
    rlog::log_info(paste(" Directory Exists: " ,testing_dir))
  }
  return(testing_dir)
}
# parse XML file
xml_doc =  xmlTreeParse(parameters_xml_file,addAttributeNamespaces = TRUE,useInternalNodes = TRUE)

rlog::log_info(paste("Looking for parameters in :",parameters_xml_file))
parameter_list = getParametersFromXML(xml_doc)
rlog::log_info(parameter_list)

data_type = NULL
input_template = NULL
if("input" %in% names(parameter_list)){
  rlog::log_info(paste("Found input in parameters list"))
  desc = parameter_list[["input"]][["description"]]
  rlog::log_info(paste("Description:", desc))
  data_type = "csv"
  example_expression = strsplit(desc,"\\s+")[[1]]
  example_expression = example_expression[example_expression != "" & !apply(t(example_expression),2,function(x) grepl("git",x)) ]
  if(sum(c("tab","tsv","TSV") %in% example_expression) > 0 ){
    data_type = "tsv"
  } else if(sum(apply(t(example_expression),2,function(x) grepl("/",x))) > 0 ){
    example_expression = example_expression[apply(t(example_expression),2,function(x) grepl("/",x))]
    if(length(example_expression) > 1){
      rlog::log_info(paste("MULTIPLE_EXPRESSIONS_FOUND:",example_expression))
      data_type = example_expression[1]
    } else if(length(example_expression) == 1){
       data_type = strsplit(example_expression,"/")[[1]][2]
    } else{
      rlog::log_info(paste("CONFUSED_ABOUT:",desc))
    }
  }
  rlog::log_info(paste("Expression:", data_type))
}


rlog::log_info(paste("Looking for dataInputs in :",parameters_xml_file))
data_input_list = getDataInputsFromXML(xml_doc)
rlog::log_info(data_input_list)


if(!is.null(data_type) && data_type != ""){
  rlog::log_info(paste("DATA format of input file is:",data_type))
  if(data_type %in% c('csv','tsv')){
    data_input_list[["input"]] = paste("input.",data_type,sep="")
    data_input_list[["input"]][["type"]] = "FILE"
    data_input_list[["input"]][["description"]] = "Dummy input file"
    data_input_list = generateFakeInputFiles(data_input_list,format_type=input_file_format)
  }
} else{
  data_input_list = generateFakeInputFiles(data_input_list,format_type=input_file_format)
}

# - create dummy files and stage them in current working directory
dummy_inputs = createDummyInputs(data_input_list)
### set working directory to Testing one
new_testing_dir = createTestingDir(parameters_xml_file)
setwd(new_testing_dir)
### stage test input data to kick off pipeline
stage_inputs = stageInputs(dummy_inputs)

# STEP 1: TEST run a nextflow command
# - craft nextflow run command
#####################
create_mount_string <- function(script_path){
  mount_string = paste("-v",paste(dirname(script_path),":",dirname(script_path),sep=""),"-w",dirname(script_path))
  return(mount_string)
}
craft_nextflow_params <- function(data_list, param_list){
  params_to_ignore = c("input_files","project_dir")
  cli_to_add = c()
  if(length(names(data_list)) > 0){
    for(i in 1:length(names(data_list))){
      param_name = names(data_list)[i]
      if(!param_name %in% params_to_ignore){
        params_to_ignore = c(params_to_ignore,param_name)
        if(! grepl("\\.",param_name)){
          str_to_add = paste(paste("--",param_name,sep=""),data_list[[param_name]])
          cli_to_add = c(cli_to_add,str_to_add)
        }
      }
    }
  }
  if(length(names(param_list)) > 0 ){
    for(i in 1:length(names(param_list))){
      param_name = names(param_list)[i]
      if(!param_name %in% params_to_ignore){
        params_to_ignore = c(params_to_ignore,param_name)
        str_to_add = paste(paste("--",param_name,sep=""),param_list[[param_name]][["default"]])
        cli_to_add = c(cli_to_add,str_to_add)
      }
    }
  }
  rlog::log_info(paste("ADDITIONAL_PARAMS:",paste(cli_to_add,collapse = " ")))
  return(paste(cli_to_add,collapse = " "))
}

syncNextflowScripts <- function(test_dir, nextflow_script){
  sync_cmd = paste("rsync -r",paste(dirname(nextflow_script),"/",sep=""),test_dir)
  rlog::log_info(paste("RUNNING:",sync_cmd))
  system(sync_cmd)
}
################
##rlog::log_info(paste("DATA_INPUT_PARAM:",names(dummy_inputs)))
overrides =  craft_nextflow_params(dummy_inputs,parameter_list)
### STAGE nextflow scripts
syncNextflowScripts(new_testing_dir,nextflow_script)

#### 
staged_nextflow_script = paste(new_testing_dir,basename(nextflow_script),sep="/")
###################################
docker_cmd = paste("docker run -it --rm",create_mount_string(staged_nextflow_script),docker_image,"nextflow run",basename(staged_nextflow_script),overrides)
rlog::log_info(paste("RUNNING CMD:",docker_cmd))
docker_result = system(docker_cmd,intern = TRUE)
staged_nextflow_script_log = paste(new_testing_dir,paste(basename(nextflow_script),".log",sep=""),sep="/")
rlog::log_info(paste("WRITING OUT CONTENTS TO :",staged_nextflow_script_log))
write.table(paste(docker_result,sep="\n",collapse="\n"),staged_nextflow_script_log)
#####################
docker_command_error <- function(cmd_out){
  error_in_output = FALSE
  eror_in_line = apply(t(cmd_out),2,function(x) grepl("error",x))
  if(sum(eror_in_line) > 0){
    error_in_output = TRUE
  }
  return(error_in_output)
}
################
error_check = docker_command_error(docker_result)
if(error_check){
  rlog::log_error(paste("SCRIPT:",staged_nextflow_script,"ERROR"))
  rlog::log_error(paste("OUTPUT:",docker_result))
} else{
  rlog::log_info(paste("SCRIPT:",staged_nextflow_script,"PASSED"))
}
# STEP 2: Double-check modules
  # - identify modules with:
  #   - missing pod annotation labels
  # - missing cpus and memory declarations
  # - missing maxForks declarations

### reset working directory
setwd(Sys.getenv("HOME"))



################
