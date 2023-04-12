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
docker_cmd = paste("docker run -it --rm",create_mount_string(nextflow_script),docker_image,"nextflow run",basename(nextflow_script),"-c",basename(main_config),"--input","input.csv")
rlog::log_info(paste("RUNNING CMD:",docker_cmd))
docker_result = system(docker_cmd,intern = TRUE)
#####################
docker_command_error <- function(cmd_out){
  error_in_output = FALSE
  error_in_line = apply(t(cmd_out),2,function(x) grepl("error|groovy|fail",x,ignore.case = TRUE))
  please_in_line = apply(t(cmd_out),2,function(x) grepl("please",x,ignore.case = TRUE))
  if(sum(error_in_line) > 0 & sum(please_in_line) == 0){
    error_in_output = TRUE
  }
  return(error_in_output)
}
################

error_check = docker_command_error(docker_result)
#print(docker_result)
lines_of_interest = apply(t(docker_result),2,function(x) grepl("Missing",x))
#print(lines_of_interest)
xml_pass = FALSE
if(sum(lines_of_interest) >0){
  params_of_interest = apply(t(docker_result[lines_of_interest]),2,function(x){x1=strsplit(x,"\\s+")[[1]]; return(x1[length(x1)])})
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
  }
}
if(error_check & !xml_pass){
  rlog::log_error(paste("SCRIPT:",nextflow_script,"ERROR"))
  rlog::log_error(paste("OUTPUT:",docker_result))
} else{
  rlog::log_info(paste("SCRIPT:",nextflow_script,"PASSED"))
}
