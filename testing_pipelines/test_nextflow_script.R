options(stringsAsFactors=FALSE)
suppressPackageStartupMessages(library("argparse"))
library(rlog)
# create parser object
parser <- ArgumentParser()

# specify our desired options 
# by default ArgumentParser will add an help option 
parser$add_argument("-c", "--nextflow-config","--nextflow_config", default = NULL, required = TRUE,
                    help="main nf-core pipeline config")
parser$add_argument("-s", "--nextflow-script","--nextflow_script", default = NULL, required = TRUE,
                    help="input nf-core pipeline script")
parser$add_argument("-d", "--docker-image","--docker_image", default ="nextflow/nextflow:20.10.0", required = FALSE,
                    help="docker image")
parser$add_argument("-q","--in-docker","--in_docker",action="store_true",
                    default=FALSE, help = "flag to indicate if this script is run within a docker image/container")
########################################
args <- parser$parse_args()
nextflow_script = args$nextflow_script
docker_image = args$docker_image
main_config = args$nextflow_config
xml_file = paste(dirname(nextflow_script),"/",basename(dirname(nextflow_script)),".pipeline.xml",sep="")
#####################
create_mount_string <- function(script_path){
  mount_string = paste("-v",paste(dirname(script_path),":",dirname(script_path),sep=""),"-w",dirname(script_path))
  return(mount_string)
}
################
if(is.null(main_config)){
    main_config = paste(dirname(nextflow_script),"/nextflow.config",sep="")
}

if(!file.exists(main_config)){
    rlog::log_error(paste("Cannot find the config",main_config))
    stop()
} else if(!file.exists(nextflow_script)){
    rlog::log_error(paste("Cannot find the script",nextflow_script))
    stop()
}
docker_binary_check_bool = FALSE
if(!args$in_docker){
  docker_binary_check = system("which docker",intern = TRUE)
  if(length(docker_binary_check) > 0){
    docker_binary_check_bool = apply(t(docker_binary_check),2, function(x) grepl("docker",x))
  }
}
if(sum(docker_binary_check_bool) > 0 & !args$in_docker){
  nextflow_cmd = paste("docker run -it --rm",create_mount_string(nextflow_script),docker_image,"nextflow run",basename(nextflow_script),"-c",basename(main_config),"--input","input.csv")
} else if(args$in_docker){
  setwd(dirname(nextflow_script))
  nextflow_cmd = paste("nextflow run",basename(nextflow_script),"-c",basename(main_config),"--input","input.csv")
} else{
  setwd(dirname(nextflow_script))
  nextflow_cmd = paste("nextflow run",basename(nextflow_script),"-c",basename(main_config),"--input","input.csv")
  
}
rlog::log_info(paste("RUNNING CMD:",nextflow_cmd))
nextflow_test_result = system(nextflow_cmd,intern = TRUE)
#####################
nextflow_command_error <- function(cmd_out){
  error_in_output = FALSE
  error_in_line = apply(t(cmd_out),2,function(x) grepl("compile|error|groovy|fail",x,ignore.case = TRUE))
  please_in_line = apply(t(cmd_out),2,function(x) grepl("please",x,ignore.case = TRUE))
  if(sum(error_in_line & !please_in_line) > 0){
    error_in_output = TRUE
  }
  return(error_in_output)
}
################

error_check = nextflow_command_error(nextflow_test_result)
#print(nextflow_test_result)
lines_of_interest = apply(t(nextflow_test_result),2,function(x) grepl("Missing",x))
xml_pass = FALSE
lines_to_double_check = c()
if(sum(lines_of_interest) >0){
  lines_idxs = (1:length(lines_of_interest))[lines_of_interest]
  params_of_interest = c()
  for(loi in 1:length(lines_idxs)){
    line_of_interest = nextflow_test_result[lines_idxs[loi]]
    lines_to_double_check = c(lines_to_double_check,line_of_interest)
    rlog::log_info(paste("NEXTFLOW_OUTPUT_LINE_OF_INTEREST:",line_of_interest))
    line_of_interest_split = strsplit(line_of_interest,"\\s+")[[1]]
    for(lois in 1:length(line_of_interest_split)){
      if(grepl("^--",line_of_interest_split[lois]) & grepl("[a-z]",line_of_interest_split[lois])){
        params_of_interest = c(params_of_interest,line_of_interest_split[lois])
      }
    }
  }
  if(length(params_of_interest) >0){
    for(pi in 1:length(params_of_interest)){
      param_of_interest = params_of_interest[pi]
      param_of_interest = gsub("-","",param_of_interest)
      xml_lines = system(paste("grep",param_of_interest,xml_file),intern = T)
      if(length(xml_lines)==0){
        rlog::log_warn(paste("Could not find",param_of_interest,"in",xml_file))
        xml_pass = FALSE
        break
      } else{
        rlog::log_info(paste("Found",param_of_interest,"in",xml_file))
        xml_pass = TRUE
      }
    } 
  } else{
    rlog::log_warn(paste("No parameters found missing"))
    rlog::log_warn(paste(lines_to_double_check,collapse="\n"))
    xml_pass = TRUE
  }
} else{
  xml_pass = TRUE
}
if(error_check & !xml_pass){
  rlog::log_error(paste("SCRIPT:",nextflow_script,"ERROR"))
  rlog::log_error(paste("OUTPUT:",nextflow_test_result))
} else{
  rlog::log_info(paste("SCRIPT:",nextflow_script,"PASSED"))
}
