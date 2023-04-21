
library(rlog)
options(stringsAsFactors=FALSE)
suppressPackageStartupMessages(library("argparse"))
# create parser object
parser <- ArgumentParser()

#nfcore_bundle_info = read.csv('/Users/keng/icav2.nfcore_bundle.manifest.20220624.csv')
#demo_data_manifest = read.delim('/Users/keng/DRAGEN.ICA_demo_data.manifest.tsv',header=FALSE)
#server_url = "stage.v2.stratus.illumina.com"
parser$add_argument("-n","--nfcore-bundle-file","--nfcore_bundle_file", required = TRUE,
                    default=NULL, help = "nf-core bundle metadata file")
parser$add_argument("-d","--demo-data-file","--demo_data_file", required = TRUE,
                    default=NULL, help = "demo data manifest ")
parser$add_argument("-k","--api-key-file","--api_key_file", required = TRUE,
                    default=NULL, help = "ICA API key file ")
parser$add_argument("-s","--pipeline-conversion-dir","--pipeline_conversion_dir", required = TRUE,
                    default=NULL, help = "ICA pipeline conversion dir")
parser$add_argument("-x","--parameters-xml","--parameters_xml",
                    default=NULL, help = "ICA parameter XML file")
parser$add_argument("-p","--ica-project-name","--ica_project_name",
                    default=NULL, help = "ICA project name")
parser$add_argument("-b","--base-ica-url","--base_ica_url",
                    default="ica.illumina.com", help = "ICA base URL")
parser$add_argument("-o","--output-file","--output_file",
                    default=NULL, help = "output file with CLI commands")
parser$add_argument("-t","--turn-off-testing","--turn_off_testing",action="store_true",
                    default=FALSE, help = "turn off testing condition (i.e. params.ica_smoke_test)")
args <- parser$parse_args()
########
nfcore_bundle_file = args$nfcore_bundle_file
nfcore_base_dir = args$nfcore_base_dir
demo_data_file = args$demo_data_file
ica_project_name = args$ica_project_name
ica_project_id = args$ica_project_id
api_key_file = args$api_key_file
server_url = args$base_ica_url
output_launch_script = args$output_file
pipeline_conversion_dir = args$pipeline_conversion_dir
turn_off_testing = args$turn_off_testing
####
current_timestamp = format(Sys.time(), "%d_%m_%y_%H_%M_%S")
current_datestamp = format(Sys.time(), "%Y%m%d")
if(is.null(output_launch_script)){
  output_launch_script = paste("smoke_test",".nf_core.",current_timestamp,".sh",sep = "")
}
###################
ica_auth_list = list()
if(!is.null(ica_project_id)){
  ica_auth_list[["--project-id"]] = ica_project_id
}
ica_auth_list[["--base-ica-url"]] = server_url
##########
creation_jsons = list.files(pipeline_conversion_dir,pattern="pipeline_creation.response.json",full.names=T,recursive=T)
cli_commands = c()
if(length(creation_jsons)==0){
  stop(paste("Could not find any files with the name:pipeline_creation.response.json in the directory path [",pipeline_conversion_dir,"]"))
} else{
  for (i in  1:length(creation_jsons)){
    pipeline_creation_json = creation_jsons[i]
    #infer pipeline name
    pipeline_name = basename(dirname(pipeline_creation_json))
    # infer XML file path
    xml_file_path = paste(dirname(pipeline_creation_json),paste(pipeline_name,".pipeline.xml",sep=""),sep="/")
    # auto-generate CLI stub output file
    cli_stub_file =  paste(dirname(pipeline_creation_json),paste(pipeline_name,".nf-core.smoke_test.",current_datestamp,".txt",sep=""),sep="/")
  
    command_line = c("Rscript smoketest.nf-core.R","--nfcore-bundle-file",nfcore_bundle_file,"--demo-data-file",demo_data_file,"--parameters-xml",xml_file_path,"--pipeline-creation-json",pipeline_creation_json,"--api-key-file",api_key_file,"--output-file",cli_stub_file)                         
    
    for(k in 1:length(names(ica_auth_list))){
      key = names(ica_auth_list)[k]
      command_line = c(command_line,key,ica_auth_list[[key]])
    }
    if(!"project_id" %in% names(ica_auth_list)){
      command_line = c(command_line,"--ica-project-name",ica_project_name)
    }
    if(turn_off_testing){
      command_line = c(command_line,"--turn-off-testing")
    }
    rlog::log_info(paste("RUNNING",paste(command_line,collapse = " ",sep = " ")))
    system(paste(command_line,collapse = " ",sep = " "))
    cli_commands = c(cli_commands,paste("bash",cli_stub_file))
    cli_commands = c(cli_commands,"sleep 5")
  }
}

# create batch script with all CLI commands
rlog::log_info(paste("Writing out smoke test batch script:",output_launch_script))
write.table(paste(cli_commands,collapse="\n"),file=output_launch_script,quote=F,row.names=F,col.names = F)

system(paste("bash",output_launch_script))
