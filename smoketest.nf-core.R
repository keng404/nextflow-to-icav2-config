
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

###########
getDatas <- function(pipeline_name,data_type,ica_auth_list = ica_auth_list){
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
  cmd_base  = paste("icav2 projectdata list",paste("/",paste(pipeline_name,"_project_dir",sep=""),"/",sep = "") ,"-o json --parent-folder --data-type",data_type,sep = " ",collapse = " ")
  for(i in 1:length(names(ica_auth_list))){
    cmd_base = paste(cmd_base,names(ica_auth_list)[i],ica_auth_list[[names(ica_auth_list)[i]]],sep = " ",collapse = " ")
  }
  cmd_base  = paste( cmd_base, " > projectdata.json", collapse = " ", sep = " ")
  rlog::log_info(paste("GRABBING_DATA",cmd_base))
  system(cmd_base)
  #######
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
    dataset_label = "Illumina DRAGEN RNA Demo Data"
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
create_dummy_columns <- function(cols_to_add,field_metadata,spreadsheet_lines){
  spreadsheet_lines = as.matrix(spreadsheet_lines)
  adding_cols  = c()
  for(i in 1:length(cols_to_add)){
    col_to_add = cols_to_add[i]
    if(sum("enum" %in% names(field_metadata[[col_to_add]]))){
      add_col = rep(field_metadata[[col_to_add]][["enum"]][1],nrow(spreadsheet_lines))
    } else{
      if(field_metadata[[col_to_add]][["type"]] == "integer"){
        add_col = 1:nrow(spreadsheet_lines)
      } else{
        add_col = paste("group",1:nrow(spreadsheet_lines),sep="")
      }
    }
    adding_cols = cbind(adding_cols,add_col)
  }
  new_spreadsheet_lines = cbind(spreadsheet_lines,adding_cols)
  return(new_spreadsheet_lines)
}
#demodataset_list = get_demo_dataset("atacseq",nfcore_manifest_bundle=nfcore_bundle_info,demo_dataset_bundle=demo_data_manifest)
create_dummy_spreadsheet <- function(pipeline_name,input_schema_json,demo_dataset){
  samplesheet_id = NULL
  # create spreadsheet
  spreadsheet_header = names(input_schema_json[["items"]][["properties"]])
  field_metadata = input_schema[["items"]][["properties"]]
  fields_assumed = c("sample","fastq_1","fastq_2")
  cols_to_add = c()
  if(sum(!spreadsheet_header %in% fields_assumed) > 0){
    cols_to_add = spreadsheet_header[!spreadsheet_header %in% fields_assumed]
    cols_to_add = cols_to_add[cols_to_add %in% input_schema_json[["items"]][["required"]]]
  }
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
      r2_group_files = files_of_interest[r2_group]
      rlog::log_info(paste("r1_group_files:",paste(r1_group_files,sep=", ",collapse=", ")))
      rlog::log_info(paste("r2_group_files:",paste(r2_group_files,sep=", ",collapse=", ")))
      for(j in 1:length(r1_group_files)){
        spreadsheet_lines = rbind(spreadsheet_lines,c(sample_id_of_interest,r1_group_files[j],r2_group_files[j]))
      }
    } else if(sum(r1_group) > 0 & sum(r2_group) == 0){
      r1_group_files = files_of_interest[r1_group]
      rlog::log_info(paste("r1_group_files:",paste(r1_group_files,sep=", ",collapse=", ")))
      for(j in 1:length(r1_group_files)){
        spreadsheet_lines = rbind(spreadsheet_lines,c(sample_id_of_interest,r1_group_files[j]))
      }
    } else{
      rlog::log_error(paste("Cannot find R1 or R2 groups for these files:",files_of_interest))
      for(j in 1:length(files_of_interest)){
        spreadsheet_lines = rbind(spreadsheet_lines,c(sample_id_of_interest,files_of_interest[j]))
      }
    }
  }
  print(cat(head(spreadsheet_lines)))
  if(length(cols_to_add) >0){
    print(cols_to_add)
    spreadsheet_lines = create_dummy_columns(cols_to_add,field_metadata,spreadsheet_lines)
  } 
  spreadsheet_lines = as.data.frame(spreadsheet_lines)
  colnames(spreadsheet_lines) = spreadsheet_header
  ### write spreadsheet
  write.table(spreadsheet_lines,file=paste(pipeline_name,".input.csv",sep=""),row.names=F,sep=",")
  rlog::log_info(paste("Generated input spreadsheet:",paste(pipeline_name,".input.csv",sep="")))
  # upload to ICA
  project_directory = paste("/",pipeline_name,"_project_dir/",sep="")
  destination_path = paste(project_directory,"/",paste(pipeline_name,".input.csv",sep=""),sep="",collapse="")
  samplesheet_id = uploadFile(local_path = paste(pipeline_name,".input.csv",sep=""),destination_path = destination_path,ica_auth_list = ica_auth_list)
  return(samplesheet_id)
}

fill_json_template_with_demo_data <- function(pipeline_name,demo_dataset, json_template,input_spreadsheet=NULL,ica_auth_list = ica_auth_list){
  files_of_interest = getDatas(pipeline_name,"FILE",ica_auth_list = ica_auth_list)
  folders_of_interest = getDatas(pipeline_name,"FOLDER",ica_auth_list = ica_auth_list)
  # how to get initial files staged within project dir?
  json_template1 = json_template
  if(is.null(input_spreadsheet)){
    json_template1[["parameters"]][["input"]][["value"]] = "*{fastq,fg}.gz"
    json_template1[["input"]][["input_files"]][["value"]] =  c(demo_dataset,files_of_interest)
    json_template1[["input"]][["project_dir"]][["value"]] =  folders_of_interest
    
  } else{
    json_template1[["parameters"]][["input"]][["value"]] = input_spreadsheet
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
    if(is.null(default_value)){
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
    }
  }
  return(pipeline_settings)
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
  pipeline_alias_bool = !grepl("test|^v|pipeline",pipeline_name_split,ignore.case=TRUE)
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
    updated_template_json_list = fill_json_template_with_demo_data(pipeline_name=pipeline_name,demo_dataset=demodataset_list, json_template=template_json_list,input_spreadsheet=samplesheet,ica_auth_list = ica_auth_list)
    revised_template_json_list = set_dummy_parameter_setting(updated_template_json_list)
  } else if(length(json_of_interest) == 0){
    rlog::log_info(paste("No schema_input.json for:",basename(pipeline_dir)))
    updated_template_json_list = fill_json_template_with_demo_data(pipeline_name=pipeline_name,demo_datasett=demodataset_list, json_template=template_json_list,ica_auth_list = ica_auth_list)
  }
  revised_template_json_list = set_dummy_parameter_setting(updated_template_json_list)
  template_JSON = jsonlite::toJSON(template_list,pretty=TRUE)
  rlog::log_info(paste("Writing template to",cli_json))
  write(template_JSON,file=cli_json)
  
  #STEP 4: get CLI template based on updated JSON
  #```R
  # 	Rscript create_cli_templates_from_xml.R --template-json {JSON_TEMPLATE} --parameters-xml {XML_FILE} 
  #```
  create_cli_template_cmd = paste("Rscript create_cli_templates_from_xml.R --template-json",cli_json,"--parameters-xml",xml_file )
  create_cli_template_output = system(create_cli_template_cmd,intern = TRUE)
  create_cli_template_output_split = strsplit(create_cli_template_output[length(create_cli_template_output)],"\\s+")[[1]]
  cli_commands = c(cli_commands,paste(create_cli_template_output_split,collapse=" ",sep=" "))
}
rlog::log_info(paste("Writing out CLI commands to:",output_launch_script))
write.table(paste(cli_commands,sep="\n"),file=output_launch_script,row.names=F,col.names = F,quote=F)




