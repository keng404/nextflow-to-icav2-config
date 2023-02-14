options(stringsAsFactors=FALSE)
suppressPackageStartupMessages(library("argparse"))
library(rlog)
# create parser object
parser <- ArgumentParser()

# specify our desired options 
# by default ArgumentParser will add an help option 

parser$add_argument("-s", "--nextflow-script","--nextflow_script", default = NULL, required = TRUE,
                    help="input nf-core pipeline script")
parser$add_argument("-d", "--docker-image","--docker_image", default ="nextflow/nextflow:20.10.0", required = FALSE,
                    help="docker image")
########################################
args <- parser$parse_args()
nextflow_script = args$nextflow_script
docker_image = args$docker_image
#####################
create_mount_string <- function(script_path){
  mount_string = paste("-v",paste(dirname(script_path),":",dirname(script_path),sep=""),"-w",dirname(script_path))
  return(mount_string)
}
################
docker_cmd = paste("docker run -it --rm",create_mount_string(nextflow_script),docker_image,"nextflow run",basename(nextflow_script))
rlog::log_info(paste("RUNNING CMD:",docker_cmd))
docker_result = system(docker_cmd,intern = TRUE)
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
  rlog::log_error(paste("SCRIPT:",nextflow_script,"ERROR"))
  rlog::log_error(paste("OUTPUT:",docker_result))
} else{
  rlog::log_info(paste("SCRIPT:",nextflow_script,"PASSED"))
}