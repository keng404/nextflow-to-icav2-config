
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
parser$add_argument("-x","--parameters-xml","--parameters_xml",
                    default=NULL, help = "ICA parameter XML file")
parser$add_argument("-j","--pipeline-creation-json","--pipeline_creation_json", default=NULL,
                    required=TRUE, help = "ICA pipeline creation JSON file")
parser$add_argument("-p","--ica-project-name","--ica_project_name",
                    default=NULL, help = "ICA project name")
parser$add_argument("-i","--ica-project-id","--ica_project_id",
                    default=NULL, help = "ICA project id")
parser$add_argument("-b","--base-ica-url","--base_ica_url",
                    default="ica.illumina.com", help = "ICA base URL")
parser$add_argument("-o","--output-file","--output_file",
                    default="launch.nfcore.smoke_test.txt", help = "output file with CLI commands")
args <- parser$parse_args()
pipeline_creation_json = args$pipeline_creation_json
pipeline_creation_jsons = c(pipeline_creation_json)

nfcore_bundle_file = args$nfcore_bundle_file
nfcore_bundle_info = read.csv(nfcore_bundle_file)
nfcore_base_dir = args$nfcore_base_dir
demo_data_file = args$demo_data_file
demo_data_manifest = read.delim(demo_data_file,header=FALSE)
ica_project_name = args$ica_project_name
ica_project_id = args$ica_project_id
api_key_file = args$api_key_file
server_url = args$base_ica_url
api_key = paste("'",read.delim(api_key_file,header=F)[1,],"'",sep="")
output_launch_script = args$output_file

# PRELIMINARY STEP: for each, ```pipeline_creation.response.json```
# identify the appropriate ```*.pipeline.xml``` for STEP 0:
if(length(pipeline_creation_jsons) == 0){
  stop(paste("No pipeline JSONs provided"))
}

xml_file = NULL
if(!is.null(args$parameters_xml)){
  xml_file = args$parameters_xml
}
####
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
parse_json <- function(json_file){
  out = tryCatch(
    {
      jsonlite::fromJSON(json_file)
    }, 
    error = function(cond){
      rlog::log_warn(cond)
      rlog::log_warn(paste("Could not properly read:",json_file))
      return(NULL)
    }
  )
  return(out)
}
###########
#####
getICAFilePath <- function(file_id,ica_auth_list = ica_auth_list){
  ica_file_path = NULL
  base_cmd = "icav2 projectdata get "
  base_cmd = paste(base_cmd,file_id)
  keys_of_interest  = names(ica_auth_list)
  for(key in 1:length(keys_of_interest)){
    key_of_interest = keys_of_interest[key]
    base_cmd = paste(base_cmd,key_of_interest,ica_auth_list[[key_of_interest]],sep = " ",collapse = " ")
  }
  base_cmd = paste(base_cmd," -o json > findfile.json")
  rlog::log_info(paste('RUNNING_FIND_PATH:',base_cmd))
  system(base_cmd)
  findfile_out = rjson::fromJSON(file="findfile.json")
  ica_file_path = findfile_out$details$path
  return(ica_file_path)
}
getDatas <- function(pipeline_code_name,data_type,ica_auth_list = ica_auth_list){
  pipeline_name = NULL
  data_types = c("FOLDER","FILE")
  file_extensions_keep = c('csv','tsv','json','gz')
  dataz = c()
  if(sum(data_types %in% data_type) ==0){
    rlog::log_error(paste("Invalid data_type provided:",data_type,paste(data_types,collapse=",",sep=",")))
    stop()
  }
  if(data_type == "DIRECTORY"){
    data_type = "FOLDER"
  }
  cmd_base  = paste("icav2 projectdata list",paste("/",paste(pipeline_code_name,"_project_dir",sep=""),"/",sep = "") ,"-o json --parent-folder --data-type",data_type,sep = " ",collapse = " ")
  for(i in 1:length(names(ica_auth_list))){
    cmd_base = paste(cmd_base,names(ica_auth_list)[i],ica_auth_list[[names(ica_auth_list)[i]]],sep = " ",collapse = " ")
  }
  cmd_base  = paste( cmd_base, " > projectdata.json", collapse = " ", sep = " ")
  rlog::log_info(paste("GRABBING_DATA",cmd_base))
  system(cmd_base)
  #######
  data_table = parse_json(json_file = 'projectdata.json')
  if(length(data_table$items) > 0 ){
    for(j in 1:length(data_table$items$details$path)){
      file_path = data_table$items$details$path[j]
      file_path_split = strsplit(basename(file_path),"\\.")[[1]]
      file_extension = file_path_split[length(file_path_split)]
      if(data_type == "FILE" & file_extension %in% file_extensions_keep){
        dataz = c(dataz,data_table$items$id[j])
      } else if(data_type == "FOLDER"){
        rlog::log_info(paste("Adding the",tolower(data_type),file_path,"to the command line"))
        dataz = c(dataz,data_table$items$id[j])
      } else{
        rlog::log_info(paste("Not adding",file_path))
      }
    }
  } else{
    rlog::log_warn(paste("project_id:",ica_auth_list[['--project-id']],paste("/",paste(pipeline_name,"_project_dir",sep=""),"/",sep = ""),"\n" ,"Could not find data listed in the path above"))
    return(NULL)
  }
  return(dataz)
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
  upload_out = parse_json(json_file="datalookup.json")
  if(!is.null(upload_out)){
    if('id' %in% names(upload_out)){
      upload_id = upload_out$id
    }
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
  upload_id = getFileId(destination_path,ica_auth_list = ica_auth_list)
  return(upload_id)
}

deleteFile <- function(ica_path = NULL,ica_auth_list = ica_auth_list){
  upload_id = NULL
  delete_occur = FALSE
  if(is.null(ica_path)){
    stop(paste("Please define local path to upload"))
  }
  base_cmd = "icav2 projectdata delete"
  base_cmd = paste(base_cmd,ica_path, sep = " ", collapse = " ")
  keys_of_interest  = names(ica_auth_list)
  for(key in 1:length(keys_of_interest)){
    key_of_interest = keys_of_interest[key]
    base_cmd = paste(base_cmd,key_of_interest,ica_auth_list[[key_of_interest]],sep = " ",collapse = " ")
  }
  base_cmd = paste(base_cmd,"-o json > deleteFile.json")
  rlog::log_info(paste('RUNNING_DELETION:',base_cmd))
  system(base_cmd)
  delete_confirm = parse_json(json_file="deleteFile.json")
  if(!is.null(delete_confirm)){
    if("status" %in% names(delete_confirm)){
      if(delete_confirm[["status"]] == 204){
        delete_occur = TRUE
      }
    }
  }
  return(delete_occur)
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
  findfile_out = parse_json(json_file="findfile.json")
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
##############
get_demo_dataset <- function(pipeline_alias,nfcore_manifest_bundle,demo_dataset_bundle,data_type=NULL){
  # get expected dataset type
  data_types = c('fastq','bam','vcf')
  dataset_label = NULL
  demo_dataset = c()
  demo_dataset_files = c()
  demo_dataset_list = list()
  if(is.null(data_type)){
    data_type = "fastq"
  } else{
    data_type = tolower(data_type)
  }
  if(!data_type %in% data_types){
    stop(paste("Don't know what to do with data_type:",data_type))
  }
  # identify dataset of interest
  if(sum(pipeline_alias %in% nfcore_manifest_bundle[,1]) > 0 ){
    dataset_label = nfcore_manifest_bundle[nfcore_manifest_bundle[,1] %in% pipeline_alias,]$data_label
  } else{
    rlog::log_warn(paste("Could not find",pipeline_alias,"in the nfcore manifest bundle"))
    if(grepl("rna",pipeline_alias,ignore.case = T) | !(grepl("viral",pipeline_alias,ignore.case = T))){
     dataset_label = "Illumina DRAGEN RNA Demo Data"
    } else{
      dataset_label = "covidseq_fastq_input"
    }
  }
  
  # check dataset against data_type
  if( sum(dataset_label %in% demo_dataset_bundle[,1]) > 0 ){
    files_of_interest = demo_dataset_bundle[demo_dataset_bundle[,1] %in% dataset_label,2]
    files_of_interest = apply(t(files_of_interest),2,trimws)
    extensions_to_ignore = c("gz","bgz","zip","tgz","bz2")
    file_extensions = apply(t(files_of_interest),2,function(x){x1=basename(x); x1_split = strsplit(x1,"\\.")[[1]]; x1_split = x1_split[!x1_split %in% extensions_to_ignore];return(x1_split[length(x1_split)])})
    dataset_file_type = unique(apply(t(file_extensions),2, tolower))
    if(sum(dataset_file_type %in% data_type) >0){
      rlog::log_info(paste("Found data type",data_type,"in the dataset",dataset_label))
      demo_dataset = demo_dataset_bundle[demo_dataset_bundle[,1] %in% dataset_label,3]
      demo_dataset_files = files_of_interest
    } else{
      rlog::log_error(paste("Found data type",dataset_file_type,"in the dataset",dataset_label))
      rlog::log_error(paste("Could not find the file extension in",data_type," the dataset in the bundle file:",dataset_label))
      dataset_label = NULL
    }
  } else{
    rlog::log_error(paste("Could not find the demo dataset in the bundle file:",dataset_label))
    dataset_label = NULL
  }
  demo_dataset_list[["data_ids"]] = demo_dataset
  demo_dataset_list[["filenames"]] = demo_dataset_files
  return(demo_dataset_list)
}
create_dummy_columns <- function(cols_to_add,field_metadata,spreadsheet_lines,full_spreadsheet_header){
  spreadsheet_lines = as.matrix(spreadsheet_lines)
  #adding_cols  = c()
  for(i in 1:length(cols_to_add)){
    col_to_add = cols_to_add[i]
    col_idx = (1:length(full_spreadsheet_header))[full_spreadsheet_header %in% col_to_add]
    if(sum("enum" %in% names(field_metadata[[col_to_add]]))){
      possible_enums = field_metadata[[col_to_add]][["enum"]]
      add_col = rep(possible_enums[length(possible_enums)],nrow(spreadsheet_lines))
    } else if(sum("anyOf" %in% names(field_metadata[[col_to_add]])) > 0 ){
      pattern_of_interest = field_metadata[[col_to_add]][["anyOf"]][["pattern"]]
      vals_of_interest = stringr::str_extract(pattern_of_interest,"\\([^()]+\\)")[[1]]
      vals_of_interest = gsub("\\(|\\)","",vals_of_interest)
      vals_of_interest_split = strsplit(vals_of_interest,"\\|")[[1]]
      add_col = rep(vals_of_interest_split[length(vals_of_interest_split)],nrow(spreadsheet_lines))
    } else{
      if(sum("type" %in% names(field_metadata[[col_to_add]])) > 0 ){
        if(field_metadata[[col_to_add]][["type"]] == "integer"){
          add_col = rep(1,nrow(spreadsheet_lines))
        } else{
          add_col = paste(col_to_add,1:nrow(spreadsheet_lines),sep="")
        }
      } else{
        add_col = paste(col_to_add,1:nrow(spreadsheet_lines),sep="")
      }
    }
    # shuffle columns according to header and column we intend to add
    if(col_idx > 1 & col_idx < length(full_spreadsheet_header)){
      rlog::log_info(paste("COL_IDX:",col_idx))
      rlog::log_info(dim(spreadsheet_lines))
      spreadsheet_lines = cbind(spreadsheet_lines[,1:(col_idx-1)],add_col,spreadsheet_lines[,col_idx:ncol(spreadsheet_lines)])
      #$full_spreadsheet_header = c(full_spreadsheet_header[1:(col_idx-1)],col_to_add,full_spreadsheet_header[col_idx:length(full_spreadsheet_header)])
    } else if(col_idx == 1){
      spreadsheet_lines = cbind(add_col,spreadsheet_lines)
      #full_spreadsheet_header = c(col_to_add,full_spreadsheet_header)
    } else if(col_idx == length(full_spreadsheet_header)){
      spreadsheet_lines = cbind(spreadsheet_lines,add_col)
      #ull_spreadsheet_header = c(full_spreadsheet_header,col_to_add)
    }
  }
  new_spreadsheet_lines = spreadsheet_lines
  new_spreadsheet_lines = as.data.frame(new_spreadsheet_lines)
  print(head(new_spreadsheet_lines))
  print(full_spreadsheet_header)
  colnames(new_spreadsheet_lines)  = full_spreadsheet_header
  return(new_spreadsheet_lines)
}
#demodataset_list = get_demo_dataset("atacseq",nfcore_manifest_bundle=nfcore_bundle_info,demo_dataset_bundle=demo_data_manifest)
create_dummy_spreadsheet <- function(pipeline_name,input_schema_json,demo_dataset){
  samplesheet_id = NULL
  # create spreadsheet
  spreadsheet_header = names(input_schema_json[["items"]][["properties"]])
  field_metadata = input_schema[["items"]][["properties"]]
  fields_assumed = c("sample","fastq_1","fastq_2")
  other_fields_of_interest = c("lane")
  data_types_assumed = c("fastq","bam","vcf")
  found_data_types_assumed = apply(t(data_types_assumed),2, function(x) sum(grepl(x,spreadsheet_header,ignore.case=T)) > 0)
  cols_to_add = c()
  if(sum(!spreadsheet_header %in% fields_assumed) > 0){
    cols_to_add = spreadsheet_header[!spreadsheet_header %in% fields_assumed]
    cols_to_add = cols_to_add[cols_to_add %in% input_schema_json[["items"]][["required"]] | cols_to_add %in% other_fields_of_interest]
  }
  if(sum(spreadsheet_header %in% fields_assumed) > 1 & sum(found_data_types_assumed) > 0 ) {
  spreadsheet_header = spreadsheet_header[(spreadsheet_header %in% fields_assumed | spreadsheet_header %in% cols_to_add)]
  spreadsheet_lines = c()
  # assume paired-end
  demo_files = apply(t(demo_dataset[["filenames"]]),2,basename)
  sample_ids = apply(t(demo_files),2,function(x){ x1=strsplit(x,"\\.")[[1]]; x2=strsplit(x1[1],"\\_")[[1]][1]; return(x2)})
  sample_ids_of_interest = unique(sample_ids)
  for(i in 1:length(sample_ids_of_interest)){
    sample_id_of_interest = sample_ids_of_interest[i]
    files_of_interest =  demo_files[sample_ids == sample_id_of_interest]
    rlog::log_info(paste("FILES_OF_INTEREST:",paste(files_of_interest,sep=", ",collapse=", ")))
    r1_group = apply(t(files_of_interest),2,function(x){ x1=strsplit(basename(x),"\\.")[[1]]; x2=strsplit(x1[1],"\\_")[[1]]; return(sum("r1" %in% x2)>0 | sum("R1" %in% x2)>0)})
    r2_group = apply(t(files_of_interest),2,function(x){ x1=strsplit(basename(x),"\\.")[[1]]; x2=strsplit(x1[1],"\\_")[[1]]; return(sum("r2" %in% x2)>0 | sum("R2" %in% x2)>0)})
    rlog::log_info(paste("r1_group:",paste(r1_group,sep=", ",collapse=", ")))
    rlog::log_info(paste("r2_group:",paste(r2_group,sep=", ",collapse=", ")))
    if(sum(r1_group) > 0 & sum(r2_group) > 0){
      r1_group_files = files_of_interest[r1_group]
      r1_group_files = apply(t(r1_group_files),2,basename)
      r2_group_files = files_of_interest[r2_group]
      r2_group_files = apply(t(r2_group_files),2,basename)
      rlog::log_info(paste("r1_group_files:",paste(r1_group_files,sep=", ",collapse=", ")))
      rlog::log_info(paste("r2_group_files:",paste(r2_group_files,sep=", ",collapse=", ")))
      for(j in 1:length(r1_group_files)){
        spreadsheet_lines = rbind(spreadsheet_lines,c(sample_id_of_interest,r1_group_files[j],r2_group_files[j]))
      }
    } else if(sum(r1_group) > 0 & sum(r2_group) == 0){
      r1_group_files = files_of_interest[r1_group]
      r1_group_files = apply(t(r1_group_files),2,basename)
      rlog::log_info(paste("r1_group_files:",paste(r1_group_files,sep=", ",collapse=", ")))
      for(j in 1:length(r1_group_files)){
        spreadsheet_lines = rbind(spreadsheet_lines,c(sample_id_of_interest,r1_group_files[j]))
      }
    } else{
      rlog::log_error(paste("Cannot find R1 or R2 groups for these files:",files_of_interest))
      files_of_interest = apply(t(files_of_interest),2,basename)
      for(j in 1:length(files_of_interest)){
        spreadsheet_lines = rbind(spreadsheet_lines,c(sample_id_of_interest,files_of_interest[j]))
      }
    }
  }
  print(cat(head(spreadsheet_lines)))
  if(length(cols_to_add) >0){
    print(cols_to_add)
    spreadsheet_lines = create_dummy_columns(cols_to_add,field_metadata,spreadsheet_lines,spreadsheet_header)
  } else{
    spreadsheet_lines = as.data.frame(spreadsheet_lines)
    colnames(spreadsheet_lines) = spreadsheet_header
  } 
  spreadsheet_lines = as.data.frame(spreadsheet_lines)
  #colnames(spreadsheet_lines) = spreadsheet_header
  ### write spreadsheet
  write.table(spreadsheet_lines,file=paste(pipeline_name,".input.csv",sep=""),row.names=F,sep=",",quote=F)
  rlog::log_info(paste("Generated input spreadsheet:",paste(pipeline_name,".input.csv",sep="")))
  # upload to ICA
  project_directory = paste("/",pipeline_name,"_project_dir/",sep="")
  destination_path = paste(project_directory,paste(pipeline_name,".input.csv",sep=""),sep="",collapse="")
  existing_file_id = getFileId(ica_path = destination_path, ica_auth_list = ica_auth_list)
  if(!is.null(existing_file_id)){
    delete_status = deleteFile(ica_path = destination_path,ica_auth_list = ica_auth_list)
    if(delete_status == FALSE){
      rlog::log_warn(paste("Not sure if we could delete existing file on ICA:",destination_path))
    }
  }
  num_retries = 2
  num_tries = 0
  while(num_tries < num_retries){
    samplesheet_id = uploadFile(local_path = paste(pipeline_name,".input.csv",sep=""),destination_path = destination_path,ica_auth_list = ica_auth_list)
    if(!is.null(samplesheet_id)){
      num_tries = num_retries
    }  else{
      num_tries = num_tries + 1
      Sys.sleep(num_tries * 10)
    }
  }
  } else{
    rlog::log_warn(paste("Currently only able to generate FASTQ-based samplesheets"))
    rlog::log_warn(paste("I see",paste(spreadsheet_header,sep=",",collapse=",")))
    rlog::log_warn(paste(data_types_assumed,sep=",",collapse=","))
    rlog::log_warn(paste(found_data_types_assumed,sep=",",collapse=","))
    samplesheet_id = NULL
  }
  return(samplesheet_id)
}

#demodataset_list = get_demo_dataset("atacseq",nfcore_manifest_bundle=nfcore_bundle_info,demo_dataset_bundle=demo_data_manifest)
create_dummy_spreadsheet_no_json <- function(pipeline_name,template_files,demo_dataset){
  samplesheet_id = NULL
  fields_assumed = c("sample","fastq_1","fastq_2")
  other_fields_of_interest = c("lane")
  data_types_assumed = c("fastq","bam","vcf")
  delimitter = ","
  template_file_ext = ".input.csv"
  if(grepl(".tsv$",template_files[1])){
    delimitter = "\t"
    template_file_ext = ".input.tsv"
  }
  # create spreadsheet
  spreadsheet_header = c()
  field_metadata = list()
  for(i in 1:length(template_files)){
    rlog::log_info(paste("READING in template_file :",template_files[i]))
    template_dat = as.matrix(read.delim(template_files[i],sep=delimitter))
    if(length(spreadsheet_header) >0){
      fields_to_add = colnames(template_dat)[!colnames(template_dat) %in% spreadsheet_header]
    } else{
      fields_to_add = colnames(template_dat)
    }
    if(length(fields_to_add) >0){
      fields_to_add = fields_to_add[!is.na(fields_to_add)]
      rlog::log_info(paste("ADDING cols to template:",paste(fields_to_add,collpase=", ",sep=", "),collapse=", "))
      for(j in 1:length(fields_to_add)){
        field_metadata[[fields_to_add[j]]] = list()
        field_type = "string"
        col_idx = (1:length(colnames(template_dat)))[colnames(template_dat) == fields_to_add[j]]
        is_integer_column = apply(t(template_dat[,col_idx]),2,function(x) strtoi(x))
        if(sum(is.na(is_integer_column)) == 0){
          field_type = "integer"
        }
        field_metadata[[fields_to_add[j]]][["type"]] = field_type
        unique_template_vals = unique(t(template_dat[,col_idx]))
        if(length(unique_template_vals) >= 2 & sum(fields_to_add[j] %in% c(fields_assumed,other_fields_of_interest)) == 0){
          field_metadata[[fields_to_add[j]]][["enum"]] = unique_template_vals
        }
      }
      spreadsheet_header = c(spreadsheet_header, fields_to_add)
    } else{
      rlog::log_info(paste("No additional columns to add"))
    }
  }
  found_data_types_assumed = apply(t(data_types_assumed),2, function(x) sum(grepl(x,spreadsheet_header,ignore.case=T)) > 0)
  cols_to_add = c()
  if(sum(!spreadsheet_header %in% fields_assumed) > 0){
    cols_to_add = spreadsheet_header[!spreadsheet_header %in% fields_assumed]
  }
  if(sum(spreadsheet_header %in% fields_assumed) > 1 | sum(found_data_types_assumed) > 0 ) {
    spreadsheet_header = spreadsheet_header[(spreadsheet_header %in% fields_assumed | spreadsheet_header %in% cols_to_add)]
    spreadsheet_lines = c()
    # assume paired-end
    demo_files = apply(t(demo_dataset[["filenames"]]),2,basename)
    sample_ids = apply(t(demo_files),2,function(x){ x1=strsplit(x,"\\.")[[1]]; x2=strsplit(x1[1],"\\_")[[1]][1]; return(x2)})
    sample_ids_of_interest = unique(sample_ids)
    for(i in 1:length(sample_ids_of_interest)){
      sample_id_of_interest = sample_ids_of_interest[i]
      files_of_interest =  demo_files[sample_ids == sample_id_of_interest]
      rlog::log_info(paste("FILES_OF_INTEREST:",paste(files_of_interest,sep=", ",collapse=", ")))
      r1_group = apply(t(files_of_interest),2,function(x){ x1=strsplit(basename(x),"\\.")[[1]]; x2=strsplit(x1[1],"\\_")[[1]]; return(sum("r1" %in% x2)>0 | sum("R1" %in% x2)>0)})
      r2_group = apply(t(files_of_interest),2,function(x){ x1=strsplit(basename(x),"\\.")[[1]]; x2=strsplit(x1[1],"\\_")[[1]]; return(sum("r2" %in% x2)>0 | sum("R2" %in% x2)>0)})
      rlog::log_info(paste("r1_group:",paste(r1_group,sep=", ",collapse=", ")))
      rlog::log_info(paste("r2_group:",paste(r2_group,sep=", ",collapse=", ")))
      if(sum(r1_group) > 0 & sum(r2_group) > 0){
        r1_group_files = files_of_interest[r1_group]
        r1_group_files = apply(t(r1_group_files),2,basename)
        r2_group_files = files_of_interest[r2_group]
        r2_group_files = apply(t(r2_group_files),2,basename)
        rlog::log_info(paste("r1_group_files:",paste(r1_group_files,sep=", ",collapse=", ")))
        rlog::log_info(paste("r2_group_files:",paste(r2_group_files,sep=", ",collapse=", ")))
        for(j in 1:length(r1_group_files)){
          spreadsheet_lines = rbind(spreadsheet_lines,c(sample_id_of_interest,r1_group_files[j],r2_group_files[j]))
        }
      } else if(sum(r1_group) > 0 & sum(r2_group) == 0){
        r1_group_files = files_of_interest[r1_group]
        r1_group_files = apply(t(r1_group_files),2,basename)
        rlog::log_info(paste("r1_group_files:",paste(r1_group_files,sep=", ",collapse=", ")))
        for(j in 1:length(r1_group_files)){
          spreadsheet_lines = rbind(spreadsheet_lines,c(sample_id_of_interest,r1_group_files[j]))
        }
      } else{
        rlog::log_error(paste("Cannot find R1 or R2 groups for these files:",files_of_interest))
        files_of_interest = apply(t(files_of_interest),2,basename)
        for(j in 1:length(files_of_interest)){
          spreadsheet_lines = rbind(spreadsheet_lines,c(sample_id_of_interest,files_of_interest[j]))
        }
      }
    }
    print(cat(head(spreadsheet_lines)))
    if(length(cols_to_add) >0){
      print(cols_to_add)
      spreadsheet_lines = create_dummy_columns(cols_to_add,field_metadata,spreadsheet_lines,spreadsheet_header)
    } else{
      spreadsheet_lines = as.data.frame(spreadsheet_lines)
      colnames(spreadsheet_lines) = spreadsheet_header
    } 
    spreadsheet_lines = as.data.frame(spreadsheet_lines)
    #colnames(spreadsheet_lines) = spreadsheet_header
    ### write spreadsheet
    write.table(spreadsheet_lines,file=paste(pipeline_name,template_file_ext,sep=""),row.names=F,sep=delimitter,quote=F)
    rlog::log_info(paste("Generated input spreadsheet:",paste(pipeline_name,template_file_ext,sep="")))
    # upload to ICA
    project_directory = paste("/",pipeline_name,"_project_dir/",sep="")
    destination_path = paste(project_directory,paste(pipeline_name,template_file_ext,sep=""),sep="",collapse="")
    existing_file_id = getFileId(ica_path = destination_path, ica_auth_list = ica_auth_list)
    if(!is.null(existing_file_id)){
      delete_status = deleteFile(ica_path = destination_path,ica_auth_list = ica_auth_list)
      if(delete_status == FALSE){
        rlog::log_warn(paste("Not sure if we could delete existing file on ICA:",destination_path))
      }
    }
    num_retries = 2
    num_tries = 0
    while(num_tries < num_retries){
      samplesheet_id = uploadFile(local_path =paste(pipeline_name,template_file_ext,sep=""),destination_path = destination_path,ica_auth_list = ica_auth_list)
      if(!is.null(samplesheet_id)){
        num_tries = num_retries
      }  else{
        num_tries = num_tries + 1
        Sys.sleep(num_tries * 10)
      }
      
    }
  } else{
    rlog::log_warn(paste("Currently only able to generate FASTQ-based samplesheets"))
    rlog::log_warn(paste("I see",paste(spreadsheet_header,sep=",",collapse=",")))
    rlog::log_warn(paste(data_types_assumed,sep=",",collapse=","))
    rlog::log_warn(paste(found_data_types_assumed,sep=",",collapse=","))
    samplesheet_id = NULL
  }
  return(samplesheet_id)
}

fill_json_template_with_demo_data <- function(pipeline_name,demo_dataset, json_template,input_spreadsheet=NULL,ica_auth_list = ica_auth_list){
  files_of_interest = getDatas(pipeline_code_name=pipeline_name,"FILE",ica_auth_list = ica_auth_list)
  folders_of_interest = getDatas(pipeline_code_name=pipeline_name,"FOLDER",ica_auth_list = ica_auth_list)
  # how to get initial files staged within project dir?
  json_template1 = json_template
  if(is.null(input_spreadsheet)){
    json_template1[["parameters"]][["input"]][["value"]] = "\"*{fastq,fq}.gz\""
    json_template1[["input"]][["input_files"]][["value"]] =  c(demo_dataset,files_of_interest)
    json_template1[["input"]][["project_dir"]][["value"]] =  folders_of_interest
    
  } else{
    spreadsheet_ica_path = getICAFilePath(file_id = input_spreadsheet,ica_auth_list = ica_auth_list)
    json_template1[["parameters"]][["input"]][["value"]] =  basename(spreadsheet_ica_path)
    json_template1[["input"]][["input_files"]][["value"]] =  c(demo_dataset,input_spreadsheet,files_of_interest)
    json_template1[["input"]][["project_dir"]][["value"]] =  folders_of_interest
  }
  return(json_template1)
}

set_dummy_parameter_setting <- function(pipeline_settings){
  params_to_check = names(pipeline_settings[["parameters"]])
  for(i in 1:length(params_to_check)){
    default_value = pipeline_settings[["parameters"]][[params_to_check[i]]][["value"]]
    param_type = pipeline_settings[["parameters"]][[params_to_check[i]]][["type"]]
    if(params_to_check[i] == "input" & (default_value == "null" | default_value == ""| grepl("\\*",default_value))){
      default_value = "\"*{fastq,fg}.gz\""
    } else if(is.null(default_value)){
      if(param_type == "stringType"){
        default_value = "test"
        if(grepl("email",params_to_check[i])){
          default_value = "foobar@gmail.com"
        }
      } else if(param_type == "integerType"){
        default_value = 1
      } else if(param_type == "booleanType"){
        default_value = "false"
      } else if(param_type == "optionsType"){
        rlog::log_warn(paste("Not sure how to set options_type type: Check XML for",params_to_check[i]))
      }
      pipeline_settings[["parameters"]][[params_to_check[i]]][["value"]] = default_value
    } else if(default_value == "" | default_value == "null"){
      if(param_type == "stringType"){
        default_value = "test"
        if(grepl("email",params_to_check[i])){
          default_value = "foobar@gmail.com"
        }
      } else if(param_type == "integerType"){
        default_value = 1
      } else if(param_type == "booleanType"){
        default_value = "false"
      } else if(param_type == "optionsType"){
        rlog::log_warn(paste("Not sure how to set options_type type: Check XML for",params_to_check[i]))
      }
      pipeline_settings[["parameters"]][[params_to_check[i]]][["value"]] = default_value
    } else if(grepl("email",params_to_check[i])){
      default_value = "foobar@gmail.com"
      pipeline_settings[["parameters"]][[params_to_check[i]]][["value"]] = default_value
    }
  }
  return(pipeline_settings)
}
##########
check_usage_doc <- function(usage_documentation){
  usage_list = list()
  usage_documentation_data = read.delim(usage_documentation,header=F)
  usage_lines_of_interest = apply(t(usage_documentation_data),2,function(x) grepl("samplesheet",x) | grepl("csv",x) | grepl("tsv",x))
  line_of_interest = apply(t(usage_documentation_data),2,function(x) grepl("--",x) & grepl("nextflow",x) & grepl("run",x) & !grepl("test",x))
  if(sum(line_of_interest) > 0){
    usage_list[["command_line"]] = usage_documentation_data[line_of_interest,][1]
    usage_list[["params_to_override"]] = list()
    usage_list[["other_params_of_interest"]] = list()
    usage_list[["samplesheet"]] = NULL
    usage_list[["usage_lines"]] = usage_documentation_data
    usage_list[["usage_lines_of_interest"]]  =usage_lines_of_interest
    cli_split = strsplit(usage_list[["command_line"]],"\\s+")[[1]]
    for(i in 4:length(cli_split)){
      if(grepl("--",cli_split[i]) | grepl("-",cli_split[i])){
        val_iof_interest = NULL
        if(length(cli_split) >= (i+1)){
          val_of_interest = cli_split[i+1]
          if(grepl("'",val_of_interest)){
            if(sum(cli_split[i] %in% c("--input","-input","input")) == 0 ){
             usage_list[["params_to_override"]][[cli_split[i]]] = val_of_interest 
            } else{
              usage_list[["other_params_of_interest"]][[cli_split[i]]] = val_of_interest 
            }
          } else if(grepl("tsv",val_of_interest) | grepl("csv",val_of_interest)){
            usage_list[["samplesheet"]] = val_of_interest
          } else{
            usage_list[["other_params_of_interest"]][[cli_split[i]]] = val_of_interest 
          }
        }
      }
    }
    return(usage_list)
  } else{
    return(NULL)
  }
}
################
create_template_data <- function(usage_lines_to_read){
  in_console_statement = FALSE
  template_lines = c()
  for(i in 1:length(usage_lines_to_read)){
    if(usage_lines_to_read[i] == "```console"){
      in_console_statement = TRUE
       next
    }
    if(usage_lines_to_read[i] == "```"){
      in_console_statement = FALSE
    }
    if(in_console_statement){
      template_lines = rbind(template_lines,usage_lines_to_read[i] )
    }
  }
  return(template_lines)
}
############
cli_commands = c()
for( i in 1:length(pipeline_creation_jsons)){
  pipeline_json = pipeline_creation_jsons[i]
  rlog::log_info(paste("Generating smoke test for:",pipeline_json))
  pipeline_dir = dirname(pipeline_creation_jsons[i])
  created_pipeline = jsonlite::fromJSON(pipeline_json)
  created_pipeline_metadata = created_pipeline[["pipeline"]]
  pipeline_name = created_pipeline_metadata[["code"]]
  pipeline_id = created_pipeline_metadata[["id"]]
  workflow_version = created_pipeline_metadata[["languageVersion"]][["name"]]
  pipeline_name_split = strsplit(pipeline_name,"\\_")[[1]]
  pipeline_alias_bool = !grepl("test|pipeline",pipeline_name_split,ignore.case=TRUE)
  if(sum(pipeline_alias_bool) == 0 ){
    stop(paste("Cannot find pipeline alias for:",pipeline_name))
  } else{
    pipeline_alias = pipeline_name_split[pipeline_alias_bool][1]
  }
  if(is.null(xml_file)){
    xml_file = paste(pipeline_dir,"/",pipeline_alias,".pipeline.xml",sep="")
  }
  if(!file.exists(xml_file)){
    stop(paste("Cannot find xml file:",xml_file))
  }
  
  #### create JSON template that helps generate ICA CLI stub
  template_json = sub(".xml$",".template.json",xml_file)
  cli_json = sub(".xml$",".cli_template.json",xml_file)
  
  # STEP 0: Run create_cli_templates_from_xml.R --- creates a dummy JSON file template that will inform the final ICA CLI command
  
  template_generation = paste("Rscript create_cli_templates_from_xml.R --parameters-xml",xml_file,"--output-json",template_json)
  system(template_generation)
  
  template_json_list = jsonlite::fromJSON(template_json)
  
  #STEP 1 : Check if ```schema_input.json``` exists , if not assume staged input files will be specified by ``` --input '*{fastq,fq}.gz' ```
  #parse schema_input.json, understanding if fields are required or not and if there are restricted values (enums) for each field
  #```R
  json_of_interest = list.files(pipeline_dir,pattern="schema_input.json",full.names=TRUE,recursive=TRUE)
  tsvs_of_interest = list.files(pipeline_dir,pattern="*tsv$",full.names=TRUE,recursive=TRUE)
  csvs_of_interest = list.files(pipeline_dir,pattern="*csv$",full.names=TRUE,recursive=TRUE)
  template_files_of_interest = c(tsvs_of_interest,csvs_of_interest)
  usage_docs = list.files(pipeline_dir,pattern="usage.md",full.names=TRUE,recursive=TRUE)
  if(length(usage_docs) > 0){
    test_list = check_usage_doc(usage_docs[1])
  } else{
    test_list = NULL
  }
  demodataset_list = get_demo_dataset(pipeline_alias,nfcore_manifest_bundle=nfcore_bundle_info,demo_dataset_bundle=demo_data_manifest)
  if(length(json_of_interest) > 0){
    json_of_interest = json_of_interest[1]
    input_schema = jsonlite::fromJSON(json_of_interest)
    fields_in_spreadsheet = names(input_schema[["items"]][["properties"]])
    required_fields =  input_schema[["items"]][["required"]]
    field_metadata = input_schema[["items"]][["properties"]]
    #STEP 2 : parse nf-core manifest bundle (ties pipeline to demo dataset) and demo data bundle (ties demo dataset to data_ids)
    #   
    samplesheet = create_dummy_spreadsheet(pipeline_name,input_schema,demodataset_list)
    updated_template_json_list = fill_json_template_with_demo_data(pipeline_name=pipeline_name,demo_dataset=demodataset_list[["data_ids"]], json_template=template_json_list,input_spreadsheet=samplesheet,ica_auth_list = ica_auth_list)
  } else if(length(json_of_interest) == 0 & length(template_files_of_interest)>0){
    samplesheet = create_dummy_spreadsheet_no_json(pipeline_name,template_files_of_interest,demodataset_list)
    updated_template_json_list = fill_json_template_with_demo_data(pipeline_name=pipeline_name,demo_dataset=demodataset_list[["data_ids"]], json_template=template_json_list,input_spreadsheet=samplesheet,ica_auth_list = ica_auth_list)
    
  } else{
    rlog::log_info(paste("No input samplesheet for:",basename(pipeline_dir)))
    if(length(usage_docs) == 0) {
      updated_template_json_list = fill_json_template_with_demo_data(pipeline_name=pipeline_name,demo_dataset=demodataset_list[["data_ids"]], json_template=template_json_list,ica_auth_list = ica_auth_list)
    } else{
      test_list = check_usage_doc(usage_docs[1])
      line_idxs = (1:length(test_list$usage_lines_of_interest))[test_list$usage_lines_of_interest]
      if(!is.null(test_list[["samplesheet"]])){
        rlog::log_info(paste("Trying to create template for:",pipeline_name,"based off of documentation\n",usage_docs[1]))
        delimitter = ","
        new_template = paste(pipeline_name,".",test_list[["samplesheet"]],sep="")
        if(grepl("tsv",new_template)){
          delimitter = "\t"
        }
        template_data = create_template_data(test_list[["usage_lines"]][line_idxs[3]:line_idxs[4],])
        if(nrow(template_data) > 0){
          write.table(template_data,file=new_template,sep=delimitter)
          template_files_of_interest = c(new_template)
          samplesheet = create_dummy_spreadsheet_no_json(pipeline_name,template_files_of_interest,demodataset_list)
          updated_template_json_list = fill_json_template_with_demo_data(pipeline_name=pipeline_name,demo_dataset=demodataset_list[["data_ids"]], json_template=template_json_list,input_spreadsheet=samplesheet,ica_auth_list = ica_auth_list)
        } else{
          rlog::log_error(paste("Could not create temoplate for:",pipeline_name,"based of documentation\n",usage_docs[1]))
          stop()
        }
      } else{
        updated_template_json_list = fill_json_template_with_demo_data(pipeline_name=pipeline_name,demo_dataset=demodataset_list[["data_ids"]], json_template=template_json_list,ica_auth_list = ica_auth_list)
      }
    }
  }
  revised_template_json_list = set_dummy_parameter_setting(updated_template_json_list)
  ### last second --- add overrides based on documentation
  #print(jsonlite::toJSON(updated_template_json_list,pretty=TRUE))
  #####
  print(test_list)
  if(!is.null(test_list)){
    if(length(test_list[["params_to_override"]]) > 0){
      add_to_json = test_list[["params_to_override"]]
      original_keys_to_add = names(add_to_json)
      keys_to_add = apply(t(original_keys_to_add),2,function(x) gsub("-","",x))
      for(atj in 1:length(keys_to_add)){
        value_to_add = add_to_json[[original_keys_to_add[atj]]]
        rlog::log_info(paste("updating parameter setting in CLI:",keys_to_add[atj],"->",value_to_add))
        if(!sum(keys_to_add[atj] %in% names(revised_template_json_list[["parameters"]])) > 0) {
          revised_template_json_list[["parameters"]][[keys_to_add[atj]]] = list()
          revised_template_json_list[["parameters"]][[keys_to_add[atj]]][["minValue"]] = "1"
          revised_template_json_list[["parameters"]][[keys_to_add[atj]]][["maxValue"]] = "1"
          if(sum(value_to_add %in% c("true","false",TRUE,FALSE))>0){
            revised_template_json_list[["parameters"]][[keys_to_add[atj]]][["type"]] = "booleanType"
          } else{
            revised_template_json_list[["parameters"]][[keys_to_add[atj]]][["type"]] = "stringType"
          }
        }
        revised_template_json_list[["parameters"]][[keys_to_add[atj]]][["value"]] = value_to_add
      }
    }
  }
  template_JSON = jsonlite::toJSON(revised_template_json_list,pretty=TRUE)
  rlog::log_info(paste("Writing template to",cli_json))
  write(template_JSON,file=cli_json)
  
  #STEP 4: get CLI template based on updated JSON
  #```R
  # 	Rscript create_cli_templates_from_xml.R --template-json {JSON_TEMPLATE} --parameters-xml {XML_FILE} 
  #```
  create_cli_template_cmd = paste("Rscript create_cli_templates_from_xml.R --template-json",cli_json,"--parameters-xml",xml_file )
  rlog::log_info(paste("RUNNING:",create_cli_template_cmd))
  create_cli_template_output = system(create_cli_template_cmd,intern = TRUE)
  create_cli_template_output_split = strsplit(create_cli_template_output[length(create_cli_template_output)],"\\s+")[[1]]
  first_idx = (1:length(create_cli_template_output_split))[create_cli_template_output_split %in% "icav2"][1]
  create_cli_template_output_split = create_cli_template_output_split[first_idx:length(create_cli_template_output_split)]
  if(sum(create_cli_template_output_split %in% "PROJECT_ID" ) > 0){
    create_cli_template_output_split[create_cli_template_output_split %in% "PROJECT_ID"] = ica_auth_list[["--project-id"]]
  }
  if(sum(create_cli_template_output_split %in%  "PIPELINE_NAME" ) > 0){
    create_cli_template_output_split[create_cli_template_output_split %in% "PIPELINE_NAME"] = pipeline_name
  }
  if(sum("--x-api-key" ==  create_cli_template_output_split) == 0 & sum("-x" == create_cli_template_output_split) == 0 ){
    create_cli_template_output_split = c(create_cli_template_output_split,"--x-api-key",ica_auth_list[["--x-api-key"]])
  }
  ### adding auto generated pipeline run identifier
  workflow_run_test = paste("test",pipeline_name,format(Sys.time(), "%d_%m_%y_%H_%M_%S"),sep = "_")
  keys_to_add = c("--user-reference","--technical-tag","--user-tag","--reference-tag")
  for(k in 1:length(keys_to_add)){
    create_cli_template_output_split = c(create_cli_template_output_split,keys_to_add[k],workflow_run_test)
  }
  if(sum(create_cli_template_output_split %in%  "STRING" ) > 0){
    create_cli_template_output_split[create_cli_template_output_split %in% "STRING"] = "TEST"
  }
  #### adding base ICA url
  create_cli_template_output_split = c(create_cli_template_output_split,"--server-url",args$base_ica_url)
  cli_commands = c(cli_commands,paste(create_cli_template_output_split,collapse=" ",sep=" "))
}
rlog::log_info(paste("Writing out CLI commands to:",output_launch_script))
write.table(paste(cli_commands,sep="\n"),file=output_launch_script,row.names=F,col.names = F,quote=F)




