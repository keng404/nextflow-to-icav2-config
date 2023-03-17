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
parser$add_argument("-c","--pipeline-generation-log","--pipeline_generation_log", required = TRUE,
                    default=NULL, help = "ICA pipeline creation log file ")
parser$add_argument("-k","--api-key-file","--api_key_file", required = TRUE,
                    default=NULL, help = "ICA API key file ")
parser$add_argument("-x","--nfcore-base-dir","--nfcore_base_dir",
                    default=NULL, help = "ICA nfcore pipeline base dir")
parser$add_argument("-p","--ica-project-name","--ica_project_name",
                    default=NULL, help = "ICA project name")
parser$add_argument("-s","--production-mode","--production_mode", default=FALSE,action="store_true",
                    required=FALSE, help = "production mode")
parser$add_argument("-i","--ica-project-id","--ica_project_id",
                    default=NULL, help = "ICA project id")
parser$add_argument("-b","--base-ica-url","--base_ica_url",
                    default="ica.illumina.com", help = "ICA base URL")
parser$add_argument("-o","--output-file","--output_file",
                    default="launch.nfcore.smoke_test.txt", help = "output file with CLI commands")
args <- parser$parse_args()
nfcore_bundle_file = args$nfcore_bundle_file
nfcore_bundle_info = read.csv(nfcore_bundle_file)
nfcore_base_dir = args$nfcore_base_dir
demo_data_file = args$demo_data_file
demo_data_manifest = read.delim(demo_data_file,sep="\t",header=T)
ica_project_name = args$ica_project_name
ica_project_id = args$ica_project_id
api_key_file = args$api_key_file
server_url = args$base_ica_url
pipeline_generation_log = args$pipeline_generation_log
api_key = paste("'",read.delim(api_key_file,header=F)[1,],"'",sep="")
production_mode = args$production_mode
output_launch_script = args$output_file
if(is.null(ica_project_id) && is.null(ica_project_name)){
  stop(paste("Please provide an ICA project ID or ICA project name.\nExciting"))
} else if(is.null(ica_project_id)){
  ##rlog::log_info(paste("RUNNING: icav2 projects list -o json ","-s ica.illumina.com","-k",paste("'",api_key,"'",sep=""),"> tmp.json"))
  system(paste("icav2 projects list -o json ","-s",server_url,"-k",api_key,"> tmp.json"))
  ica_project_lookup = rjson::fromJSON(file="tmp.json")
  if(length(ica_project_lookup$items) < 1){
    stop(paste("Take a look at tmp.json"))
  }
  ica_project_lookup_to_add = ica_project_lookup
  while(!is.null(ica_project_lookup_to_add$nextPageToken)){
    system(paste("icav2 projects list -o json","-s",server_url,"-k",api_key,"--page-token",ica_project_lookup_to_add$nextPageToken,"> tmp.json"))
    ica_project_lookup_to_add = rjson::fromJSON(file="tmp.json")
    ica_project_lookup$items = append(ica_project_lookup$items, ica_project_lookup_to_add$items)
  }
  # delete last line of terminal output
  ica_project_lookup_table_subset = list()
  current_index = 1
  for(i in 1:length(ica_project_lookup$items)){
    project_name = ica_project_lookup$items[[i]]$name
    lookup_query = project_name== ica_project_name || grepl(ica_project_name,project_name,ignore.case = T)
    if(sum(lookup_query) >0){
      ica_project_lookup_table_subset[[current_index]] = ica_project_lookup$items[[i]]
      current_index = current_index +  1
    }
  }
  if(length(ica_project_lookup_table_subset) >0){
    # return the 1st result of the query
    ica_project_id = ica_project_lookup_table_subset[[1]]$id
    # throw warning if more than 1 potential match exists
    if(length(ica_project_lookup_table_subset) >1 ){
      rlog::log_warn(paste("Found more than 1 result that matches the ICA project name",ica_project_name))
      print(ica_project_lookup_table_subset)
    } else {
      rlog::log_info(paste("Found project id for ",ica_project_name,"project id is:",ica_project_id))
    }
  } else{
    stop(paste("Please provide a valid project name [",ica_project_name,"]\nFound",paste(ica_project_lookup_table[,1],collapse=", ")))
  }
}
########
ica_auth_list = list()
ica_auth_list[["--project-id"]] = ica_project_id
ica_auth_list[["--server-url"]] = server_url
ica_auth_list[["--x-api-key"]] = api_key
#############

nfcore_bundle_info$demo_data_label %in% demo_data_manifest[1,]

fieldsOfInterest <- function(template_file,delimitter_character){
  template_dat = read.delim(template_file,sep=delimitter_character)  
  fields_of_interest = colnames(template_dat)
  fields_of_interest = fields_of_interest[apply(t(fields_of_interest),2, function(x) grepl("read",x,ignore.case = T)) | apply(t(fields_of_interest),2, function(x) grepl("fastq",x,ignore.case = T))]
  if(length(fields_of_interest) < 1 ){
    return(NULL)
  } else{
    return(fields_of_interest)
  }
}

createTemplateFile <- function(data_manifest,template_path){
  if(grepl(".csv$",template_path)){
    fields_of_interest = fieldsOfInterest(template_path,',')
  } else{
    fields_of_interest = fieldsOfInterest(template_path,'\t')
  }
  if(is.null(fields_of_interest)){
    stop(paste("ERROR - Can't find fields to replace for ",template_path))
  } else{
    rlog::log_info(paste("LOOKING to replace the fields",paste(fields_of_interest,collapse=",")))
  }
}
#createTemplateFile(demo_data_manifest,'/Users/keng/nf-core/cutandrun/assets/samplesheet.csv')
#3createTemplateFile(demo_data_manifest,'/Users/keng/nf-core/ampliseq/assets/samplesheet.tsv')

identifyPipelineNames <- function(log_file,api_key,server_url){
  rlog::log_info(paste("RUNNING:",paste("grep \"Pipeline Id\"",pipeline_generation_log)))
  log_lines = system(paste("grep \"Pipeline Id\"",pipeline_generation_log),intern=TRUE)
  pipeline_ids = apply(t(log_lines),2,function(x){ line_split = strsplit(x,"\\s+")[[1]]; return(line_split[length(line_split)])})
  rlog::log_info(paste("PIPELINE_IDS_OF_INTEREST:",pipeline_ids))
  system(paste('icav2 projectpipelines list -o json',paste('-k',api_key),'-s',server_url,'--project-id',ica_project_id,">","pipeline.json"))
  pipeline_table = rjson::fromJSON(file = 'pipeline.json')
  pipeline_metadata = list()
  if(length (pipeline_table$items) >0){
  for(i in 1:length(pipeline_table$items)){
    if('pipeline' %in% names(pipeline_table$items[[i]])){
      if(pipeline_table$items[[i]]$pipeline$id %in% pipeline_ids){
        pipeline_name = pipeline_table$items[[i]]$pipeline$code
        pipeline_name_split = strsplit(pipeline_name,"\\_")[[1]]
        workflow_alias = "UKNOWN"
        if(grepl("^v",pipeline_name_split[length(pipeline_name_split)])){
          if(gsub("[[:digit:]]+","",pipeline_name_split[length(pipeline_name_split)]) =="v"){
            if(pipeline_name_split[length(pipeline_name_split)-1] != "vken"){
              workflow_alias = pipeline_name_split[length(pipeline_name_split)-1]
            } else{
              workflow_alias = pipeline_name_split[length(pipeline_name_split)-2]
            }
          } else{
            workflow_alias = pipeline_name_split[length(pipeline_name_split)]
          }
        }
        pipeline_metadata[[workflow_alias]] = pipeline_name
      }
    } else{
      pipeline_metadata[[pipeline_name]] = pipeline_name
    }
  }
  } else{
    pipeline_metadata[[pipeline_name]] = pipeline_name
  }
  return(pipeline_metadata)
}
rlog::log_info("Grabbing pipeline names")
x = identifyPipelineNames(pipeline_generation_log,api_key,server_url)
if('vken' %in% names(x)){
  name_split = strsplit(names(x),"_")[[1]]
  new_name = name_split[length(name_split) - 2]
  x[[new_name]] = x[['vken']]
}
rlog::log_info(paste("Found these ICA pipelines"))
print(x)

path_or_dir <- function(path_of_interest){
  if(grepl(".tsv$",path_of_interest) || grepl(".csv$",path_of_interest)){
    return(TRUE)
  } else{
    return(FALSE)
  }
}

get_xml <- function(path_of_interest){
  path_split = strsplit(path_of_interest,"/")[[1]]
  if(path_or_dir(path_of_interest)){
    idx = length(path_split)-1
    if(sum(path_split %in% "assets") > 0){
      idx = (1:length(path_split))[path_split %in% "assets"][1] - 1
    }
    xml_files = list.files(paste(path_split[1:idx],collapse="/"),"*.xml",full.names=T)
    rlog::log_info(paste("FOUND",xml_files))
    xml_files = xml_files[!grepl("nfcore",xml_files)]
    xml_files = xml_files[!apply(t(xml_files),2,function(x) strsplit(basename(x),"\\.")[[1]][2] == "nf-core")]
    return(xml_files[1])
  } else{
    if(length(path_split) > 5){
      #path_split[length(path_split)] = script_basename
      xml_files = list.files(paste(path_split[1:length(path_split)],collapse="/"),"*.xml",full.names=T)
      rlog::log_info(paste("FOUND",xml_files))
      xml_files = xml_files[!grepl("nfcore",xml_files)]
      xml_files = xml_files[!apply(t(xml_files),2,function(x) strsplit(basename(x),"\\.")[[1]][2] == "nf-core")]
      return(xml_files[1])
    } else{
      xml_files = list.files(path_of_interest,"*.xml",full.names=T)
      rlog::log_info(paste("FOUND",xml_files))
      xml_files = xml_files[!grepl("nfcore",xml_files)]
      xml_files = xml_files[!apply(t(xml_files),2,function(x) strsplit(basename(x),"\\.")[[1]][2] == "nf-core")]
      return(xml_files[1])
    }
  }
}
#############3
xml_list = list()
xml_files = c()
if(!is.null(nfcore_base_dir)){
  xml_files = apply(t(list.dirs(path=nfcore_base_dir,recursive = FALSE)), 2, get_xml)
} else{
  xml_files = apply(t(nfcore_bundle_info[,1]),2,get_xml)
  
}
rlog::log_info(xml_files)
for(i in 1:length(xml_files)){
  key_name = basename(dirname(xml_files[i]))
  key_value = xml_files[i]
  xml_list[[key_name]] = key_value
}
### craft and run template command ....
template_cmds = c()
rlog::log_info(paste("Generating template CLI commands"))
for(i in 1:length(names(x))){
  nf_core_project = names(x)[i]
  pipeline_of_interest = x[[nf_core_project]]
  xml_file_of_interest = NULL
  if(nf_core_project %in% names(xml_list)){
    xml_file_of_interest = xml_list[[nf_core_project]]
  }
  workflow_run_test = paste("test",nf_core_project,format(Sys.time(), "%d_%m_%y_%H_%M_%S"),sep = "_")
  if(!is.null(xml_file_of_interest)){
    cli_mock = paste("Rscript launch_pipeline_mock_pipeline_cli.R --workflow-language nextflow --parameters-xml", xml_file_of_interest,"--pipeline-name", pipeline_of_interest,"--pipeline-run-name", workflow_run_test,collapse = " ",sep = " ")
    rlog::log_info(paste("CLI_MOCK:",cli_mock))
    cli_launch_command = system(cli_mock,intern = T)
    rlog::log_info(paste("LAUNCH COMMAND FOR:",nf_core_project))
    find_cmd = apply(t(cli_launch_command),2, function(z) grepl("start",z))
    if(sum(find_cmd) > 0){
      rlog::log_info(cli_launch_command[find_cmd])
      template_cmds = c(template_cmds,cli_launch_command[find_cmd])
    } else{
      rlog::log_info(paste("CANNOT FIND CMD FOR",cli_launch_command))
    }
  } else{
    rlog::log_info(paste("COULD not find XML file for",nf_core_project))
  }
}
#############################################3
splitCommandString <- function(cmd_str){
  cmd_str_split = strsplit(cmd_str,"\\[\\ OPTIONAL\\:")[[1]][1]
  return(cmd_str_split)
}
getDatas <- function(cmd_str_split,token,data_type,ica_auth_list){
  pipeline_name = NULL
  token_split = strsplit(token,"\\:")[[1]]
  tokens_abstracted = c("project_dir:DIRECTORY","input_files:FILE")
  file_extensions_keep = c('csv','tsv','json','gz')
  idx = 1
  while(idx < length(cmd_str_split)){
    if(cmd_str_split[idx + 1] == "nextflow" & cmd_str_split[idx] == "start"){
      pipeline_name = cmd_str_split[idx + 2]
      idx = length(cmd_str_split)
    }
    idx = idx + 1
  }
  if(data_type == "DIRECTORY"){
    data_type = "FOLDER"
  }
  cmd_base  = paste("icav2 projectdata list",paste("/",paste(pipeline_name,"_project_dir",sep=""),"/",sep = "") ,"-o json --parent-folder --data-type",data_type,sep = " ",collapse = " ")
  for(i in 1:length(names(ica_auth_list))){
    cmd_base = paste(cmd_base,names(ica_auth_list)[i],ica_auth_list[[names(ica_auth_list)[i]]],sep = " ",collapse = " ")
  }
  cmd_base  = paste( cmd_base, " > projectdata.json", collapse = " ", sep = " ")
  rlog::log_info(paste("GRABBING_DATA",cmd_base))
  system(cmd_base)
  #######
  dataz = c()
  data_table = rjson::fromJSON(file = 'projectdata.json')
  if(length(data_table$items) > 0 ){
    for(j in 1:length(data_table$items)){
      file_path = data_table$items[[j]]$details$path
      file_path_split = strsplit(basename(file_path),"\\.")[[1]]
      file_extension = file_path_split[length(file_path_split)]
      if(token %in% tokens_abstracted){
        if(token ==  "input_files:FILE" & file_extension %in% file_extensions_keep){
          dataz = c(dataz,data_table$items[[j]]$id)
        } else{
          rlog::log_info(paste("Adding the file",file_path,"to the command line"))
          dataz = c(dataz,data_table$items[[j]]$id)
        }
      } else{
        if(j == 1){
          if(file_extension %in% file_extensions_keep){
            dataz = c(dataz,data_table$items[[j]]$id)
          }
        }
      }
    }
  } else{
    rlog::log_warn(paste("project_id:",ica_auth_list[['--project-id']],paste("/",paste(pipeline_name,"_project_dir",sep=""),"/",sep = ""),"\n" ,"Could not find data listed in the path above"))
    return(NULL)
  }
  if(length(dataz) > 0){
    updated_token = paste(token_split[1],paste(dataz,sep=",",collapse=","),sep = ":",collapse = ":")
  } else{
    updated_token = token
  }
  return(updated_token)
}
#############
getFileId <- function(ica_path, ica_auth_list = ica_auth_list){
  upload_id = NULL
  keys_of_interest  = names(ica_auth_list)
  lookup_cmd = paste("icav2 projectdata get",paste("'",ica_path,"'",sep = ""),sep = " ", collpase = " ")
  for(key in 1:length(keys_of_interest)){
    key_of_interest = keys_of_interest[key]
    lookup_cmd = paste(lookup_cmd,key_of_interest,ica_auth_list[[key_of_interest]],sep = " ",collapse = " ")
  }
  lookup_cmd = paste(lookup_cmd, "-o json > datalookup.json",sep = " ", collapse = " ")
  rlog::log_info(paste('RUNNING_LOOKUP:',lookup_cmd))
  system(lookup_cmd)
  upload_out = rjson::fromJSON(file="datalookup.json")
  if('id' %in% names(upload_out)){
    upload_id = upload_out$id
  }
  return(upload_id)
}
uploadFile <- function(local_path = NULL,destination_path = NULL,ica_auth_list = ica_auth_list){
  upload_id = NULL
  if(is.null(local_path)){
    stop(paste("Please define local path to upload"))
  }
  if(is.null(destination_path)){
    destination_path = "/"
  }
  base_cmd = "icav2 projectdata upload"
  base_cmd = paste(base_cmd,local_path,destination_path, sep = " ", collapse = " ")
  keys_of_interest  = names(ica_auth_list)
  for(key in 1:length(keys_of_interest)){
    key_of_interest = keys_of_interest[key]
    base_cmd = paste(base_cmd,key_of_interest,ica_auth_list[[key_of_interest]],sep = " ",collapse = " ")
  }
  rlog::log_info(paste('RUNNING_UPLOAD:',base_cmd))
  system(base_cmd)
  Sys.sleep(15)
  file_ids = c()
  upload_id = getFileId(paste("/",basename(destination_path),sep=""),ica_auth_list = ica_auth_list)
  return(upload_id)
}
#####
findICAFilePath <- function(file_name,additional_path = NULL,ica_auth_list = ica_auth_list){
  ica_file_path = NULL
  base_cmd = "icav2 projectdata list --file-name"
  base_cmd = paste(base_cmd,file_name)
  keys_of_interest  = names(ica_auth_list)
  for(key in 1:length(keys_of_interest)){
    key_of_interest = keys_of_interest[key]
    base_cmd = paste(base_cmd,key_of_interest,ica_auth_list[[key_of_interest]],sep = " ",collapse = " ")
  }
  base_cmd = paste(base_cmd," --match-mode FUZZY -o json > findfile.json")
  rlog::log_info(paste('RUNNING_FIND_PATH:',base_cmd))
  system(base_cmd)
  findfile_out = rjson::fromJSON(file="findfile.json")
  size_iter = 1
  page_size = 1000
  while(!is.null(findfile_out$nextPageToken)){
    base_cmd = "icav2 projectdata list --file-name"
    base_cmd = paste(base_cmd,file_name)
    keys_of_interest  = names(ica_auth_list)
    for(key in 1:length(keys_of_interest)){
      key_of_interest = keys_of_interest[key]
      base_cmd = paste(base_cmd,key_of_interest,ica_auth_list[[key_of_interest]],sep = " ",collapse = " ")
    }
    page_offset = size_iter * page_size
    base_cmd = paste(base_cmd, '--page-offset',page_offset, sep = " ", collapse = " ")
    base_cmd = paste(base_cmd," --match-mode FUZZY -o json > findfile.json")
    system(base_cmd)
    findfile_out_tmp = rjson::fromJSON(file="findfile.json")
    findfile_out = rbind(findfile_out,findfile_out_tmp)
    size_iter = size_iter + 1
  }
  if(length(findfile_out$items) > 0){
    for(idx in 1:length(findfile_out$items)){
      if(!is.null(additional_path)){
        if(additional_path == findfile_out$items[[idx]]$details$path){
          ica_file_path = findfile_out$items[[idx]]$details$path
          break
        }
      } else{
        if(grepl(file_name,findfile_out$items[[idx]]$details$path)){
          ica_file_path = findfile_out$items[[idx]]$details$path
          break
        }
      }
    }
  }
 return(ica_file_path)
}
#### 
readTemplateFile <- function (template_path, ica_auth_list = ica_auth_list){
  if(grepl(".csv$",template_path)){
    delimitter_character = ','
  } else{
    delimitter_character = '\t'
  }
  template_dat = read.delim(template_path,sep=delimitter_character)  
  file_ids = c()
  for( i in 1:nrow(template_dat)){
    line_of_interest = template_dat[i,]
    line_of_interest = apply(t(line_of_interest),2, function(x) as.character(x))
    rlog::log_info(paste("LINE_OF_INTEREST:",paste(line_of_interest,collapse=" ")))
    for(fidx in 1:length(line_of_interest)){
      if(grepl(".gz$|.bam$|.vcf$",line_of_interest[fidx])){
        rlog::log_info(paste("FIELD_OF_INTEREST:",line_of_interest[fidx]))
        path_split = strsplit(line_of_interest[fidx],"/")[[1]]
        if(length(path_split) > 1){
          if(!grepl(":",path_split[1])){
            ica_file_path  = findICAFilePath(basename(line_of_interest[fidx]),additional_path = line_of_interest[fidx],ica_auth_list = ica_auth_list)
            if(!is.null(ica_file_path)){
              file_id = getFileId(ica_file_path,ica_auth_list = ica_auth_list)
              file_ids = c(file_ids,file_id)
              rlog::log_info(paste("FOUND the file",ica_file_path,"with id",file_id))
            } else{
              rlog::log_info(paste("COULD NOT FIND file ID for",line_of_interest[fidx]))
            }
          }
        } else{
          ica_file_path = findICAFilePath(line_of_interest[fidx],ica_auth_list = ica_auth_list)
          if(!is.null(ica_file_path)){
            file_id = getFileId(ica_file_path,ica_auth_list = ica_auth_list)
            rlog::log_info(paste("FOUND the file",ica_file_path,"with id",file_id))
            file_ids = c(file_ids,file_id)
          } else{
            rlog::log_info(paste("COULD NOT FIND file ID for",line_of_interest[fidx]))
          }
        }
      }
    }
  }  
  return(file_ids)
}
##############
findInputFileString <- function(cmd_str){
  cmd_split = strsplit(cmd_str,"\\s+")[[1]]
  input_str = NULL
  for(i in 1:length(cmd_split)){
    if(grepl("^input_files:",cmd_split[i])){
      input_str = strsplit(cmd_split[i],"input_files:")[[1]][2]
    }
  }
  return(input_str)
}
replaceInputString <- function(base_cmd,input_str){
  cmd_str_split = strsplit(base_cmd,"\\s+")[[1]]
  modified_cmd  = c()
  idx = 1
  while(idx < length(cmd_str_split)){
    if(grepl("^--",cmd_str_split[idx])){
      if(cmd_str_split[idx] == "--input" & grepl("^input_files:",cmd_str_split[idx+1])){
        modified_cmd = c(modified_cmd,cmd_str_split[idx])
        modified_cmd = c(modified_cmd,paste(paste("input_files:",input_str,sep="",collapse="")))
        idx = idx + 2
      } else{
        modified_cmd = c(modified_cmd,cmd_str_split[idx])
        modified_cmd = c(modified_cmd,cmd_str_split[idx + 1])
        idx = idx + 2
      }
    } else{
      modified_cmd = c(modified_cmd,cmd_str_split[idx])
      idx = idx + 1
    }
  }
  final_modified_command = paste(modified_cmd,sep = " ",collpase = " ")
  return(final_modified_command)
}
combineInputFileStrings <- function(cmd1,cmd2){
  input_str1 = findInputFileString(cmd1)
  input_str2 = findInputFileString(cmd2)
  combined_input_str = input_str1
  if(!is.null(input_str1)){
    if(!is.null(input_str2)){
      combined_input_str1 = paste(combined_input_str,input_str2,collapse = ",",sep=",")
      rlog::log_info(paste("INITIALCOMBINED_INPUT_STR:",combined_input_str1))
      combined_input_str = paste(unique(strsplit(combined_input_str1,"\\,")[[1]]),collapse = ",",sep=",")
    }
  } else{
    combined_input_str = input_str2
  }
  rlog::log_info(paste("COMBINED_INPUT_STR:",combined_input_str))
  final_modified_command = replaceInputString(cmd1,combined_input_str)
  rlog::log_info(paste("REGENERATED_CMD:",paste(final_modified_command,sep = " ", collapse = " ")))
  return(final_modified_command)
}
###############
generateInputString <- function(cmd_str_split,nf_core_manifest,ica_auth_list,demo_data_manifest=demo_data_manifest){
  additional_data_input = list()
  additional_data_input[["files_to_add"]] = c()
  additional_data_input[["input_string"]] = NULL
  input_string = "'*{1,2}*{fastq,fq}.gz'"
  #additional_data_input[["input_string"]] = paste("input",input_string,sep = ":")
  #input_string = paste("'","\\${workflow.launchDir}/",gsub("'","",input_string),"'",sep="")
  # either a string expression or filename ...
  ### lookup against the nfcore_bundle_info to find template file
  files_to_add = c()
  pipeline_name = cmd_str_split[5]
  pipeline_name_split = strsplit(pipeline_name,"_")[[1]]
  pipeline_name_split = pipeline_name_split[!pipeline_name_split %in% c('test','pipeline')]
  idx_of_interest = length(pipeline_name_split)-1
  rlog::log_info(paste("LOOKING_AT_TOKEN:",pipeline_name_split[idx_of_interest]))
  rlog::log_info(paste(pipeline_name_split, sep = " ", collapse = " "))
  path_split = apply(t(nf_core_manifest[,1]),2,function(x) strsplit(x,"/")[[1]])
  nfcore_query = apply(t(path_split),2,function(x) pipeline_name_split[idx_of_interest] %in% x[[1]])
  rlog::log_info(paste("QUERY:",paste(nfcore_query,sep=" ",collapse=" "),sep = " ",collapse = " "))
  #nfcore_query = grepl(pipeline_name_split[idx_of_interest],nf_core_manifest[,1])
  template_query = grepl(".tsv$|.csv$",nf_core_manifest[,1])
  if(sum(nfcore_query & template_query) > 0){
    final_query = nfcore_query & template_query
    rlog::log_info(paste("FINAL_QUERY:",paste(final_query,sep=" ",collapse=" "),sep = " ",collapse = " "))
  ### set input_string equal to that file
    template_of_interest = nf_core_manifest[final_query,1][1]
    if(file.exists(template_of_interest)){
      rlog::log_info(paste("TEMPLATE_EXISTS",template_of_interest))
      template_file_id  = uploadFile(local_path = template_of_interest,destination_path = paste("/",paste(pipeline_name,basename(template_of_interest),sep="."),sep=""), ica_auth_list = ica_auth_list)
      files_to_add = c(files_to_add,template_file_id)
      new_input = paste("'",paste(pipeline_name,basename(template_of_interest),sep="."),"'",sep="")
      #new_input = paste("'","\\${workflow.launchDir}/",paste(pipeline_name,basename(template_of_interest),sep="."),"'",sep="")
      additional_data_input[["input_string"]] = paste("input",new_input,sep=":")
      data_inputs = readTemplateFile(template_path = template_of_interest,ica_auth_list = ica_auth_list)
      if(length(data_inputs) > 0){
        rlog::log_info(paste("ADDING_FILES:",paste(data_inputs,sep=", ",collapse=", ")))
        files_to_add = c(files_to_add,data_inputs)
      }
      additional_data_input[["files_to_add"]] = files_to_add
      # modify input_files
      modified_cmd  = c()
      idx = 1
      while(idx < length(cmd_str_split)){
      #for(idx in 1:length(cmd_str_split)){
        if(grepl("^--",cmd_str_split[idx])){
          if(cmd_str_split[idx] == "--input" & grepl("^input_files:",cmd_str_split[idx+1])){
            #modified_cmd = c(modified_cmd,cmd_str_split[idx])
            modified_cmd = c(modified_cmd,paste(cmd_str_split[idx + 1],files_to_add,sep = ",",collapse = ","))
            idx = idx + 2
          } else{
            #modified_cmd = c(modified_cmd,cmd_str_split[idx])
            #modified_cmd = c(modified_cmd,cmd_str_split[idx + 1])
            idx = idx + 2
          }
        } else{
          #modified_cmd = c(modified_cmd,cmd_str_split[idx])
          idx = idx + 1
        }
      }
      final_modified_command = paste(modified_cmd,sep = " ",collpase = " ")
      additional_data_input[["final_modified_command"]] = paste("input_files:",gsub("input_files:","",final_modified_command),sep= "",collapse="")
    } else{
      rlog::log_info(paste("Cannot find template file of interest:", template_of_interest))
      additional_data_input[["input_string"]] = paste("input",input_string,sep = ":")
    }
  ### add on on this input string
  } else{
    if(sum(nfcore_query) > 0 ){
      ## read in datamanifest and link this up to the data demo_data_manifest
      dataset_alias = nfcore_bundle_info[nfcore_query,]$demo_data_label[1]
      find_dataset_alias = demo_data_manifest[,1] == dataset_alias 
      rlog::log_info(paste("DEMO_DATASET_FOUND FOR",dataset_alias))
      rlog::log_info(paste("DEMO_DATASET_FOUND --- no template:",paste(find_dataset_alias,sep = ",",collapse=",")))
      if(sum(find_dataset_alias) > 0 ){
        files_to_add = demo_data_manifest[find_dataset_alias,3]
        rlog::log_info(paste("ADDING_DEMO_DATA:",paste(files_to_add,sep = ",",collapse=",")))
        additional_data_input[["files_to_add"]] = files_to_add
        # modify input_files
        modified_cmd  = c()
        idx = 1
        while(idx < length(cmd_str_split)){
          #for(idx in 1:length(cmd_str_split)){
          if(grepl("^--",cmd_str_split[idx])){
            if(cmd_str_split[idx] == "--input" & grepl("^input_files:",cmd_str_split[idx+1])){
              #modified_cmd = c(modified_cmd,cmd_str_split[idx])
              modified_cmd = c(modified_cmd,paste(cmd_str_split[idx + 1],files_to_add,sep = ",",collapse = ","))
              idx = idx + 2
            } else{
              #modified_cmd = c(modified_cmd,cmd_str_split[idx])
              #modified_cmd = c(modified_cmd,cmd_str_split[idx + 1])
              idx = idx + 2
            }
          } else{
            #modified_cmd = c(modified_cmd,cmd_str_split[idx])
            idx = idx + 1
          }
        }
        final_modified_command = paste(modified_cmd,sep = " ",collpase = " ")
        additional_data_input[["final_modified_cmd"]] = paste("input_files:",gsub("input_files:","",final_modified_command),sep= "",collapse="")
        additional_data_input[["final_modified_command"]] = paste("input_files:",gsub("input_files:","",final_modified_command),sep= "",collapse="")
        additional_data_input[["input_string"]] = paste("input",input_string,sep = ":")
      } else{
        rlog::log_info(paste("No demo dataset found for :", pipeline_name))
        additional_data_input[["input_string"]] = paste("input",input_string,sep = ":")
      }
    } else{
      rlog::log_info(paste("No template file found for :", pipeline_name))
      additional_data_input[["input_string"]] = paste("input",input_string,sep = ":")
    }
  }
  return(additional_data_input)
}
parseCommandString <- function(cmd_str,ica_auth_list,demo_data_manifest){
  final_command_string = c()
  strs_to_replace = c("null","FILE","DIRECTORY")
  cmd_str_split = strsplit(cmd_str,"\\s+")[[1]]
  input_metadata = NULL
  str_idx = 1
  is_still_template = FALSE
  while(str_idx < length(cmd_str_split)){
    token_of_interest = cmd_str_split[str_idx]
    token_of_interest = trimws(token_of_interest)
    rlog::log_info(paste("TOKEN_OF_INTEREST:",token_of_interest))
    if(grepl("^--",token_of_interest)){
      if(token_of_interest == "--input"){
        if( cmd_str_split[str_idx + 1] == "project_dir:DIRECTORY"){
          rlog::log_info(paste("FOUND_DIRECTORY:",token_of_interest))
          rlog::log_info(paste("FOUND_DIRECTORY:",cmd_str_split[str_idx + 1]))
          data_retrieval = getDatas(cmd_str_split,cmd_str_split[str_idx + 1],"DIRECTORY",ica_auth_list)
          if(!is.null(data_retrieval)){
            cmd_str_split[str_idx + 1] = getDatas(cmd_str_split,cmd_str_split[str_idx + 1],"DIRECTORY",ica_auth_list)
          } else{
            if(! production_mode){
              is_still_template = TRUE
            }
          }
        } else if( cmd_str_split[str_idx + 1] == "input_files:FILE"){
            rlog::log_info(paste("FOUND_FILE",token_of_interest))
            rlog::log_info(paste("FOUND_FILE",cmd_str_split[str_idx + 1]))
            data_retrieval =  getDatas(cmd_str_split,cmd_str_split[str_idx + 1],"FILE",ica_auth_list)
            if(!is.null(data_retrieval)){
              cmd_str_split[str_idx + 1] = getDatas(cmd_str_split,cmd_str_split[str_idx + 1],"FILE",ica_auth_list)
            } else{
              if(! production_mode){
                is_still_template = TRUE
              }
            }
          } else{
          param_split = strsplit(cmd_str_split[str_idx + 1],"\\:")[[1]]
          if(param_split[2] %in% strs_to_replace){
            rlog::log_info(paste("FOUND_NEW_DATA_INPUT:",token_of_interest))
            data_retrieval = getDatas(cmd_str_split,cmd_str_split[str_idx + 1],param_split[2],ica_auth_list)
            if(!is.null(data_retrieval)){
              cmd_str_split[str_idx + 1] = data_retrieval
            } else{
              if(! production_mode){
                is_still_template = TRUE
              }
            }
          }
        }
      } else if (token_of_interest == "--parameters"){
        # | grepl("^input",cmd_str_split[str_idx + 1]) 
        if(cmd_str_split[str_idx + 1] == "input:null"){
          input_metadata =   generateInputString(cmd_str_split,nfcore_bundle_info,ica_auth_list,demo_data_manifest)
          if(! "final_modified_command" %in% names(input_metadata)){
            cmd_str_split[str_idx + 1] = input_metadata[["input_string"]]
          } else{
            if(!grepl("input_files:", input_metadata[["final_modified_command"]])){
              cmd_str_split[str_idx + 1] = input_metadata[["final_modified_command"]]
            } else{
              cmd_str_split[str_idx + 1] = input_metadata[["input_string"]]
            }
          }
        } else if(grepl("^input_string:",cmd_str_split[str_idx + 1]) & cmd_str_split[str_idx + 1] != "input_string:null"){
          input_str_split = strsplit(cmd_str_split[str_idx + 1],"input_string\\:")[[1]]
          input_str_split = input_str_split[ input_str_split != ""]
          input_str_split[1] = paste("'",input_str_split[1],"'",sep = "")
          cmd_str_split[str_idx + 1] = paste("input_string:",input_str_split,sep = "")
        } else if (cmd_str_split[str_idx + 1] == "outdir:null"){
          cmd_str_split[str_idx + 1]  = "outdir:out"
        } else if(cmd_str_split[str_idx + 1] == "email:null"){
          cmd_str_split[str_idx + 1]  = "email:foobar@gmail.com"
        } else if(cmd_str_split[str_idx + 1] == "genome:null"){
          cmd_str_split[str_idx + 1] = "genome:GRCh38"
        } else{
          param_split = strsplit(cmd_str_split[str_idx + 1],"\\:")[[1]]
          if(param_split[2] == "null"){
            cmd_str_split[str_idx + 1] = paste(param_split[1],"test",sep=":")
          }
        }
      }
      final_command_string = c(final_command_string,token_of_interest)
      final_command_string = c(final_command_string,cmd_str_split[str_idx + 1])
      str_idx = str_idx + 2
    } else{
      final_command_string = c(final_command_string,token_of_interest)
      str_idx = str_idx + 1
    }
  }
  for(idx in 1:length(names(ica_auth_list))){
    final_command_string = c(final_command_string,c(names(ica_auth_list)[idx],ica_auth_list[[names(ica_auth_list)[idx]]]))
  }
  if(!is.null(input_metadata)){
    if("final_modified_command" %in% names(input_metadata)){
      rlog::log_info(paste("CMD_MODIFIED_FINAL:",input_metadata[["final_modified_command"]]))
      final_command_string1 = combineInputFileStrings(paste(final_command_string,collapse= " ",sep = " "),input_metadata[["final_modified_command"]])
      rlog::log_info(paste("SERIOUSLY_FINAL_COMMAND:",paste(final_command_string1,collapse= " ",sep=" "),collpase= " ---- ",sep= " ---- "))
      final_command_string1 = final_command_string1[final_command_string1 != ""]
      final_command_string1 = paste(final_command_string1,sep= " ",collapse = " ")
      if(!is.null(final_command_string1)){
        if(final_command_string1 != ""){
          final_command_string = final_command_string1
        }
      }
    }
  }
  if(! is_still_template){
    return(paste(final_command_string,collapse = " ",sep= " "))
  } else{
    return(NULL)
  }
}
#############
remove_generics_from_stub <- function(cli_stub){
  idxs_to_remove = c()
  args_to_strip = c("DIRECTORY","FILE","null")
  cli_stub_split = strsplit(cli_stub,"\\s+")[[1]]
  for(i in 1:length(cli_stub_split)){
    remove_or_not = apply(t(args_to_strip),2, function(x) grepl(x,cli_stub_split[i]))
    if(sum(remove_or_not) > 0 ){
      idxs_to_remove = c(idxs_to_remove,i-1)
      idxs_to_remove = c(idxs_to_remove,i)
    }
  }
  if(length(idxs_to_remove) > 0){
    idxs_to_keep = !1:length(cli_stub_split) %in%  idxs_to_remove
    updated_stub = paste(cli_stub_split[idxs_to_keep],sep = " ",collapse = " ")
    rlog::log_info(paste("Generics stripped from stub:",cli_stub))
    return(updated_stub)
  } else{
    return(cli_stub)
  }
}
# split command .... fill in null, FILE.DIRECTORY
# project directories will contain all directories within projectDir
# input_files will contain all json files within projectDir
# throw warnings for files that are unknown
# input if there are no templates will be '*{1,2}*fq.gz' by default
# add in api_key, project_id, server_url
scale_tests = c('small','half','quarter')
launch_cmds = c()
for(idx in 1:length(template_cmds)){
  rlog::log_info(paste("MODIFYING TEMPLATE:",template_cmds[idx]))
  revised_command_string = splitCommandString(template_cmds[idx])
  rlog::log_info(paste("REVISED TEMPLATE:",revised_command_string))
  final_command = parseCommandString(revised_command_string,ica_auth_list,demo_data_manifest)
  if(!is.null(final_command)){
    final_command1 = remove_generics_from_stub(final_command)
    final_command = final_command1
    rlog::log_info(paste("FINAL COMMAND:",final_command))
    launch_cmds = c(launch_cmds,final_command)
  } else{
    rlog::log_warn(paste("ISSUE creating CLI launch command from this template\n",template_cmds[idx]))
  }
}
rlog::log_info(paste("WRITING out commands to:",output_launch_script))
launch_cmds = unique(launch_cmds)
write.table(launch_cmds,file=output_launch_script,sep="\n",quote=F,col.names = F,row.names = F)