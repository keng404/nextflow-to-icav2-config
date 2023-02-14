options(stringsAsFactors=FALSE)
suppressPackageStartupMessages(library("argparse"))
source('parameter_xml_utils.R')
library(rlog)
library(rjson)
library(stringr)
library(httr)
# create parser object
parser <- ArgumentParser()

# specify our desired options 
# by default ArgumentParser will add an help option 

parser$add_argument("-s", "--nextflow-script","--nextflow_script", default=NULL,
                    help="Main nf script for a pipeline")
parser$add_argument("-l", "--cwl-script","--cwl_script", default=NULL,
                     help="Main CWL  script for a pipeline")
parser$add_argument("-z","--storage-size","--storage_size", default="Small",
                    help = "default storage size to run analyses with this pipeline. [Small => 1.2 TB, Medium => 2.4 TB, and Large => 7.2 TB] are storage sizes")
parser$add_argument("-w","--workflow-language","--workflow_language", default="nextflow",
                    required=TRUE, help = "workflow language of pipeline. Currently supported workflow languages are cwl and nextflow")
parser$add_argument("-x","--parameters-xml","--parameters_xml", default=NULL,
                    required=TRUE, help = "parameters XML file")
parser$add_argument("-o","--parameters-xml-override","--parameters_xml_override", default=FALSE,action="store_true",
                    required=FALSE, help = "parameters XML file")
parser$add_argument("-v","--pipeline-name","--pipeline_name",required = TRUE,
                    default=NULL, help = "pipeline name")
parser$add_argument("-g","--code-project-directory","--code-project_directory",
                    default=NULL, help = "directory with other files of interest")
parser$add_argument("-f","--nextflow-version","--nextflow_version",
                    default="20.10.0", help = "nextflow_version")
parser$add_argument("-p","--ica-project-name","--ica_project_name",
                    default=NULL, help = "ICA project name")
parser$add_argument("-i","--ica-project-id","--ica_project_id",
                    default=NULL, help = "ICA project id")
parser$add_argument("-b","--base-ica-url","--base_ica_url",
                    default="ica.illumina.com", help = "ICA base URL")
parser$add_argument("-k","--api-key-file","--api_key_file", required = TRUE,
                    default=NULL, help = "ICA API key file i")
parser$add_argument("-m","--simple-mode","--simple_mode",action="store_true",
                    default=FALSE, help = "flag to indicate the creation of a simple pipeline => One workflow script + XML file")
parser$add_argument("-n","--nf-core-mode","--nf_core_mode",action="store_true",
                    default=FALSE, help = "flag to indicate nf-core pipeline")
parser$add_argument("--debug",action="store_true", default=FALSE, help = "flag for debug")
parser$add_argument("-d","--description", default= NULL, help = "pipeline description")
parser$add_argument("-c","--comments", default= NULL, help = "pipeline comments")
# get command line options, if help option encountered print help and exit,
# otherwise if options not found on command line then set defaults, 
allowed_storage_sizes = c("Small","Medium","Large")
allowed_workflow_languages = c("cwl","nextflow")
pipeline_creation_request = list()
args <- parser$parse_args()
## API key file
api_key_file = args$api_key_file
api_key = read.delim(api_key_file,quote="",header=F)[,1]
nextflow_version = args$nextflow_version
## main script
## xml
xml_file = args$parameters_xml

storage_size = args$storage_size
if(!storage_size %in% allowed_storage_sizes){
  error_message = paste("Could not find",storage_size,"in allowable storage sizes")
  if(grepl(storage_size,allowed_storage_sizes,ignore.case = T)){
    potential_matches = allowed_storage_sizes[grepl(allowed_storage_sizes,storage_size,ignore.case = T)]
    error_message = paste(error_message,"\nDid you mean",paste(potential_matches,collapse="or "),"\n")
  }
  stop(error_message)
}
workflow_language = args$workflow_language
if(!workflow_language %in% allowed_workflow_languages){
  error_message = paste("Could not find",workflow_language,"in allowable workflow languages")
  if(grepl(workflow_language,allowed_workflow_languages,ignore.case = T)){
    potential_matches = allowed_workflow_languages[grepl(allowed_workflow_languages,workflow_language,ignore.case = T)]
    error_message = paste(error_message,"\nDid you mean",paste(potential_matches,collapse="or "),"\n")
  }
  stop(error_message)
}

if(workflow_language == "cwl"){
  main_script = args$cwl_script
} else if(workflow_language == "nextflow"){
  main_script = args$nextflow_script
}
ica_pipeline_launch_cmd = c("icav2 projectpipelines",workflow_language,"start",args$pipeline_name)
ica_pipeline_launch_cmd_global_flags = c("-k",paste("'",api_key,"'",sep=""))
#######################
additional_files = args$project_directory
files_to_add = list()
if(!is.null(additional_files)){
  file_list  = list.files(additional_files,full.names = T,recursive=T)
  file_list = file_list[file_list != main_script && file_list != xml_file]
  if(length(file_list) > 0) {
    file_list = file_list[!apply(t(file_list),2,function(x) x == file.path(dirname(main_script),"main.nf")) & !apply(t(file_list),2,function(x) grepl(".config$",x)) & !apply(t(file_list),2,function(x) grepl(".md$",x)) & !apply(t(file_list),2,function(x) grepl(".png$",x))]
  }
  dir_list = sort(unique(apply(t(file_list),2, function(x) dirname(x))))
### folders
  dir_list = dir_list[!apply(t(dir_list),2,function(x) x == file.path(dirname(main_script),"docs"))]
  files_to_add[["folders"]] = dir_list
### files 
  file_list = file_list[!apply(t(file_list),2,function(x) grepl(file.path(dirname(main_script),"docs"),x))]
  files_to_add[["files"]] = file_list
} else{
  if(!args$simple_mode){
    rlog::log_info(paste("By default, LOOKING for additonal files to add here:",dirname(main_script)))
    file_list  = list.files(dirname(main_script),full.names = T,recursive=T)
    file_list = file_list[file_list != main_script & file_list != xml_file]
    if(length(file_list) > 0) {
      file_list = file_list[!apply(t(file_list),2,function(x) x == file.path(dirname(main_script),"main.nf")) & !apply(t(file_list),2,function(x) grepl(".config$",x)) & !apply(t(file_list),2,function(x) grepl(".md$",x)) & !apply(t(file_list),2,function(x) grepl(".png$",x))]
    }
    dir_list = sort(unique(apply(t(file_list),2, function(x) dirname(x))))
    dir_list = dir_list[!apply(t(dir_list),2,function(x) x == file.path(dirname(main_script),"docs"))]
    ### folders
    files_to_add[["folders"]] = dir_list
    ### files 
    file_list = file_list[!apply(t(file_list),2,function(x) grepl(file.path(dirname(main_script),"docs"),x))]
    files_to_add[["files"]] = file_list
  } else{
    rlog::log_info(paste("Not adding additional files to pipeline"))
  }
}


comments = args$comments
description = args$description
is_nf_core = args$nf_core_mode

ica_project_name = args$ica_project_name
ica_project_id = args$ica_project_id
pipeline_name  = args$pipeline_name
# envelop pipeline name with double-quotes to prevent inadvertent parsing
if(length(strsplit(pipeline_name,"\\s+")[[1]]) >1){
  pipeline_name = paste("\"",pipeline_name,"\"", sep="")
}

### auto config ICA CLI ---- for Docker use
if(!file.exists(paste(Sys.getenv('HOME'),"/.icav2/config.yaml",sep=""))){
  #rlog::log_info(paste("CONFIGURING ICA CLI:\tRunning: ",paste("sed -e s","'/MY_API_KEY/",api_key,"/'",sep=""),"script.exp","> icav2_cli.auto_config.exp"))
  system(paste(paste("sed -e s","'/MY_API_KEY/",api_key,"/'",sep=""),"script.exp","> icav2_cli.auto_config.exp"))
  system("expect icav2_cli.auto_config.exp")
}
#"-server-url ica.illumina.com")

if(is.null(ica_project_id) && is.null(ica_project_name)){
  stop(paste("Please provide an ICA project ID or ICA project name.\nExciting"))
} else if(is.null(ica_project_id)){
  ##rlog::log_info(paste("RUNNING: icav2 projects list -o json ","-s ica.illumina.com","-k",paste("'",api_key,"'",sep=""),"> tmp.json"))
  system(paste("icav2 projects list -o json ","-s",args$base_ica_url,"-k",paste("'",api_key,"'",sep=""),"> tmp.json"))
  ica_project_lookup = rjson::fromJSON(file="tmp.json")
  if(length(ica_project_lookup$items) < 1){
    stop(paste("Take a look at tmp.json"))
  }
  ica_project_lookup_to_add = ica_project_lookup
  while(!is.null(ica_project_lookup_to_add$nextPageToken)){
    system(paste("icav2 projects list -o json","-s",args$base_ica_url,"-k",paste("'",api_key,"'",sep=""),"--page-token",ica_project_lookup_to_add$nextPageToken,"> tmp.json"))
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
  ica_pipeline_launch_cmd_global_flags = c(ica_pipeline_launch_cmd_global_flags,"--project-id",ica_project_id)
}

if(is.null(comments) && is_nf_core){
  comments = paste("nf-core pipeline",basename(dirname(main_script)))
  #comments = paste(strsplit(comments,"[::punc::]+")[[1]],collapse = " ")
  comments = gsub("[\\(\\)\\[\\]]","  ",comments,perl=T)
  if(!grepl("\"",comments)){
    comments = paste("\"",comments,"\"")
  }
} 


if(is_nf_core && is.null(pipeline_name)){
  pipeline_name = paste("nf-core pipeline",basename(dirname(main_script)),collapse="_")
}
###############################
get_all_pipelines <- function(api_key){
  all_pipeline_names = c()
  get_pipelines_command = paste("icav2 pipelines list -o json","-k",paste("'",api_key,"'",sep=""),"-s",args$base_ica_url, ">","pipelines.json")
  #rlog::log_info(paste("RUNNING CMD:",get_pipelines_command))
  system(get_pipelines_command)
  my_dat = rjson::fromJSON(file="pipelines.json")
  pipelines = my_dat$items
  if(length(pipelines)>0){
    for(i in 1:length(pipelines)){
      all_pipeline_names = c(all_pipeline_names,pipelines[[i]]$code)
    }
  }
  my_dat = rjson::fromJSON(file="pipelines.json")
  while(!is.null(my_dat$nextPageToken)){
    get_pipelines_command = paste("icav2 pipelines list -o json","-k",paste("'",api_key,"'",sep=""),"-s",args$base_ica_url,"--page-token",my_dat$nextPageToken, ">","pipelines.json")
    system(get_pipelines_command)
    pipelines = my_dat$items
    if(length(pipelines)>0){
      for(i in 1:length(pipelines)){
        all_pipeline_names = c(all_pipeline_names,pipelines[[i]]$code)
      }
    }
  }
  return(all_pipeline_names)
}
find_relevant_pipeline_names <- function(query,api_key){
  name_list = get_all_pipelines(api_key)
  if(sum(name_list %in% query) > 0){
    results = name_list[name_list %in% query]
    other_matches = apply(t(name_list),2,function(x) grepl(query,x))
    if(sum(other_matches) > 0){
      results = unique(c(results,name_list[other_matches]))
    }
    return(sort(results))
  } else{
    return(NULL)
  }
}
rename_pipeline_name <- function(pipeline_name,api_key){
  new_pipeline_name = find_relevant_pipeline_names(pipeline_name,api_key)
  rlog::log_info(paste("RELEVANT_PIPELINE_NAMES:",new_pipeline_name))
  created_good_name = FALSE
  while(! created_good_name){
    new_pipeline_name = find_relevant_pipeline_names(pipeline_name,api_key)
    if(!is.null(new_pipeline_name)){
      rlog::log_info(paste("RENAMING_PIPELINE_NAME:",pipeline_name,"TO",new_pipeline_name))
      pipeline_name = new_pipeline_name[length(new_pipeline_name)]
    } else if(is.null(new_pipeline_name) & !is.null(pipeline_name)){
      created_good_name = TRUE
      return(pipeline_name)
    }
    pipeline_name_split = strsplit(pipeline_name,"_")[[1]]
    if(grepl("^v",pipeline_name_split[length(pipeline_name_split)])){
      version_number = gsub("v","",pipeline_name_split[length(pipeline_name_split)])
      if(!is.na(strtoi(version_number)) ) {
        version_number = strtoi(version_number) + 1
        pipeline_name_split[length(pipeline_name_split)] = paste("v",version_number,sep="")
      } else{
        pipeline_name_split = c(pipeline_name_split,"v1")
      }
    } else{
      pipeline_name_split = c(pipeline_name_split,"v1")
    }
    new_name = paste(pipeline_name_split,sep="_",collapse="_")
    rlog::log_info(paste("TRYING_PIPELINE_NAME:",pipeline_name,"TO",new_name))
    pipeline_name = new_name
  }
  return(new_name)
}
###########################3
pipeline_name = rename_pipeline_name(pipeline_name,api_key)
pipeline_creation_request[["code"]] = pipeline_name
pipeline_creation_request[["parametersXmlFile"]] = xml_file
if(workflow_language == "nextflow"){
  pipeline_creation_request[["mainNextflowFile"]] = main_script
} else if(workflow_language == "cwl"){
  pipeline_creation_request[["workflowCwlFile"]] = main_script
}
base_ica_command = "icav2 projectpipelines create -s ica.illumina.com"
full_ica_command = paste(base_ica_command,"-k",paste("'",api_key,"'",sep=""),workflow_language,pipeline_name,"--project-id",ica_project_id,"--main",main_script,"--parameter",xml_file,"--storage-size",storage_size)

if(!is.null(comments)){
  full_ica_command = paste(full_ica_command,"--comment",paste("\"",comments,"\""))
  pipeline_creation_request[["versionComment"]] = comments
}
## add comments for pipeline if provided or give canned comment if this is an nf-core pipeline

### add documentation if README.md is found in same directory as the main NF script
### this is typical convention for most local instances GitHub repos
documentation = list.files(dirname(main_script),pattern="README.md",full.names = T)
if(!is.na(documentation)){
    
    description = read.delim(documentation,quote="",header=F)
    description_content = NULL
    lines_to_add = c()
    for(i in 1:nrow(description)){
     description[i,]  = gsub("\\*","",description[i,])
     description[i,]  = gsub("\"","",description[i,])
     description[i,]  = gsub("#","",description[i,])
     description[i,]  = gsub("`","",description[i,])
     description[i,]  = gsub("'","",description[i,])
      if(!grepl("\\!\\[",description[i,]) && !grepl("docker",description[i,],ignore.case=T) && (grepl("description",description[i,],ignore.case=T) || grepl("doc",description[i,],ignore.case=T))){
        lines_to_add = c(lines_to_add,description[i,])
      }
    }
    if(length(lines_to_add) >0){
      #description_content = paste("See",paste("https://github.com/nf-core/",pipeline_name,sep=""))
      description_content = description_content[description_content != ""]
      description_content = paste(lines_to_add,collapse = "\\n")
      description_content = gsub("[\\(\\)\\[\\]]"," ",description_content,perl=T)
      #description_content = paste(strsplit(description_content,"[::punc::]+")[[1]],collapse=" ")
      description_content = paste("\"",description_content,"\"")
    }
    if(is.null(description_content)){
      description = paste("\"","Please refer to documentation found here:",documentation,"\"")
    } else{
      description = description_content
    }
}
if(!is.null(description)){
  full_ica_command = paste(full_ica_command,"--description",description)
  pipeline_creation_request[["description"]] = description
}

### add additional files for pipeline if there are additional files found in main NF script
### This has to be done in ICA for now
files_add_string = c()
file_list = list()
if(workflow_language == "nextflow"){
  if(length(names(files_to_add))>0){
    pipeline_creation_request[["otherNextflowFiles"]] = files_to_add[["files"]]
  }
} else if(workflow_language == "cwl"){
  if(length(names(files_to_add))>0){
    pipeline_creation_request[["toolCwlFiles"]] = files_to_add[["files"]]
  }
}

## if simple_mode == TRUE , then modify XML file and upload the rest of the files to the project
if(args$simple_mode){
  file_list  = list.files(dirname(main_script),full.names = T,recursive=T)
  file_list = file_list[file_list != main_script & file_list != xml_file]
  if(length(file_list) > 0) {
    file_list = file_list[!apply(t(file_list),2,function(x) x == file.path(dirname(main_script),"main.nf")) & !apply(t(file_list),2,function(x) grepl(".config$",x)) & !apply(t(file_list),2,function(x) grepl(".pipeline.xml$",x))]
  }
  library(XML)
  library(rlog)
  dummy_xml = xml_file
  if(args$parameters_xml_override){
    rlog::log_info(paste("UPDATING parameters XML file:",dummy_xml))
    doc = xmlTreeParse(dummy_xml,useInternalNodes = TRUE)
    root = xmlRoot(doc)
    dataInputsNode = root[["dataInputs"]]
    new_input_node_attributes = c(code = "project_dir",format = "UNKNOWN",type = "DIRECTORY",required = "true",multiValue = "true")  
    
    node_object = newXMLNode("dataInput",attrs=new_input_node_attributes,parent = dataInputsNode)
    newXMLNode("label", "project_dir", parent=node_object)
    newXMLNode("description", "directory with additional files/input to run pipeline --- other files in your github project", parent=node_object)
    
    new_input_node_attributes = c(code = "input_files",format = "UNKNOWN",type = "FILE",required = "true",multiValue = "true")  
    
    node_object = newXMLNode("dataInput",attrs=new_input_node_attributes,parent = dataInputsNode)
    newXMLNode("label", "input_files", parent=node_object)
    newXMLNode("description", "additional files/input to run pipeline --- other files in your github project", parent=node_object)
    
    outputPath = gsub(".xml$",".updated.xml",dummy_xml)
    rlog::log_info(paste("Updating parameters XML here:",outputPath))
    #prefix='<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n'
    prefix.xml <- "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n"
    saveXML(doc , file=outputPath,encoding="utf-8")
    xml_file = outputPath
    rlog::log_info(paste("Resetting parameters XML to:",outputPath))
  }
  pipeline_creation_request[["parametersXmlFile"]] = xml_file
  #####################################################
  files_to_stage = file_list
  for(fts in 1:length(files_to_stage)){
    base_path = paste(dirname(main_script),"/",sep="")
    local_path = files_to_stage[fts]
    ica_path = paste("/",pipeline_name,"_project_dir/",gsub(base_path,"",local_path),sep="")
    upload_cmd = paste("icav2 projectdata upload", local_path, ica_path,'--project-id',ica_project_id,"-k",paste("'",api_key,"'",sep=""),"-s",args$base_ica_url)
    #rlog::log_info(paste("RUNNING UPLOAD_CMD:",upload_cmd))
    system(upload_cmd)
  }
}
# return result to check if we're good
pipeline_creation_url = paste(paste("https://",args$base_ica_url,"/ica/rest/api/projects/",sep=""),ica_project_id,"/pipelines:createNextflowPipeline",sep="")
if(workflow_language == "cwl"){
  pipeline_creation_url = paste(paste("https://",args$base_ica_url,"/ica/rest/api/projects/",sep=""),ica_project_id,"/pipelines:createCwlPipeline",sep="")
}

system(paste("icav2 analysisstorages list -o json","-s",args$base_ica_url,"-k",paste("'",api_key,"'",sep=""),"> storages.json"))
storages_json = rjson::fromJSON(file="storages.json")
storage_id = NULL
for(i in 1:length(storages_json$items)){
  if(storages_json$items[[i]]$name == storage_size){
    storage_id = storages_json$items[[i]]$id
  }
}

if(is.null(storage_id)){
  stop(paste("Could not find the storage id associated with the storage size",storage_size))
}
#full_ica_command = paste(full_ica_command, "-o json"," > pipeline_create.json")
#rlog::log_info(paste("RUNNING_COMMAND:",full_ica_command))
#system(full_ica_command)
#json_response = rjson::fromJSON(file="pipeline_create.json")
#if(json_response$status_code != 201){
#  stop(paste("Check pipeline_create.json for error messages"))
#} else{
#  rlog::log_info(paste("Pipeline Created successfully in project [",ica_project_id,"]\nPipeline id is:",json_response$id))
#}


#### API implementation
# example POST request
#curl -X 'POST' \
#'https://ica.illumina.com/ica/rest/api/projects/adsfadsfas/pipelines:createNextflowPipeline' \
#-H 'accept: application/vnd.illumina.v3+json' \
#-H 'Content-Type: multipart/form-data' \
#-F 'otherNextflowFiles=@make_snapshot.sh;type=text/x-sh' \
#-F 'otherNextflowFiles=@nfcore_external_java_deps.jar;type=application/java-archive' \
#-F 'versionComment=FirstVersion' \
#-F 'parametersXmlFile=@sarek.nf-core.pipeline.xml;type=text/xml' \
#-F 'code=asdfdasfasdf' \
#-F 'htmlDocumentation=' \
#-F 'mainNextflowFile=@main.ica.dev.nf' \
#-F 'metadataModelFile=' \
#-F 'links=' \
#-F 'analysisStorageId=qassss' \
#-F 'categories=' \
#-F 'description=adsfassadf'
#############
parsePipelineDescription <- function(pipeline_description,api_key){
  known_versions = c('20.10.0','22.04.3')  
  final_version_id = NULL
  final_version = NULL
  pipeline_language_version_url =paste("https://",args$base_ica_url,"/ica/rest/api/pipelineLanguages/nextflow/versions",sep="")
  curl_command = paste("curl --verbose -vL -X 'GET'",pipeline_language_version_url)
  curl_command = paste(curl_command,"-H 'accept: application/vnd.illumina.v3+json'")
  curl_command = paste(curl_command,"-H 'X-API-Key:",paste(api_key,"'",sep=""))
  curl_command = paste(curl_command,"-H 'Content-Type: application/vnd.illumina.v3+json'")
  curl_response = rjson::fromJSON(json_str=system(curl_command,intern=T))
  nextflow_version_metadata = curl_response$items 
  nextflow_version_metadata_list = list()
  if(length(nextflow_version_metadata) > 0){
    print(nextflow_version_metadata)
    for(i in 1:length(nextflow_version_metadata)){
      nextflow_version_metadata_list[[nextflow_version_metadata[[i]]$name]] = nextflow_version_metadata[[i]]$id
    }
  } else{
    stop(paste("Did not get response from ",pipeline_language_version_url))
  }
  pipeline_description_split = strsplit(pipeline_description,">=")[[1]]
  if(length(pipeline_description_split) <  2){
    return(NULL)
  } else{
    tokenize_pipeline_description_split = strsplit(pipeline_description_split[2],"\\s+")[[1]]
    tokenize_pipeline_description_split = tokenize_pipeline_description_split[tokenize_pipeline_description_split!=""]
    numeric_version_conversion = strtoi(gsub("\\.","",tokenize_pipeline_description_split))
    pick_closest_version = apply(t(known_versions),2, function(zed) strtoi(gsub("\\.","",zed)) - numeric_version_conversion )
    pick_closest_version = pick_closest_version[!is.na(pick_closest_version)]
    if(length(pick_closest_version) > 0){
      final_version = known_versions[order(pick_closest_version,decreasing =  TRUE)][1]
      final_version_id = nextflow_version_metadata_list[[final_version]]
      rlog::log_info(paste("Using the Nextflow Version",final_version,"associated with the UUID of",final_version_id))
      return(final_version_id)
    } else{
      return(NULL)
    }
  }
}
#########################

pipeline_creation_request[["analysisStorageId"]] = storage_id
pipeline_creation_request[["links"]] = "" 
pipeline_creation_request[["categories"]] = ""
pipeline_creation_request[["htmlDocumentation"]] = ""
pipeline_creation_request[["metadataModelFile"]] = "" 
###################
final_nextflow_version = parsePipelineDescription(pipeline_creation_request[["description"]],api_key)
if(!is.null(final_nextflow_version)){
  rlog::log_info(paste("Requesting nextflow version",final_nextflow_version))
  pipeline_creation_request[["pipelineLanguageVersionId"]] = final_nextflow_version
} else{
  rlog::log_info(paste("No nextflow version specified in documentation, defaulting to nextflow version",args$nextflow_version))
}
###### ATTEMPT ____ MANUALLY CREATE ACTUAL CURL COMMAND TO ICA API rest server to  create pipeline
create_curl_command <- function(url,request){
  curl_command = paste("curl --verbose -vL -X 'POST'",url)
  files_sections = c("otherNextflowFiles","toolCwlFiles","mainNextflowFile","parametersXmlFile","workflowCwlFile")
  #adding headers
  curl_command = paste(curl_command,"-H 'accept: application/vnd.illumina.v3+json'")
  curl_command = paste(curl_command,"-H 'X-API-Key:",paste(api_key,"'",sep=""))
  curl_command = paste(curl_command,"-H 'Content-Type: multipart/form-data'")
  for(i in 1:length(names(request))){
    if(!names(request)[i] %in% files_sections){
      string_to_add = paste("-F",paste("'",names(request)[i],"=",request[[names(request)[i]]],"'",sep=""))
      curl_command = paste(curl_command,string_to_add)
    } else{
      if(names(request)[i] == "otherNextflowFiles" || names(request)[i] == "toolCwlFiles"){
        base_path = paste(dirname(main_script),"/",sep="")
        other_files = c()
        if(workflow_language == "nextflow"){
          other_files = request[["otherNextflowFiles"]]
        } else if(workflow_language == "cwl"){
          other_files =  request[["toolCwlFiles"]]
        }
        if(length(other_files) > 0){
          rlog::log_info(paste("OTHER_FILES_TO_ADD",paste(other_files,collapse=", ")))
          for(fidx in 1:length(other_files)){
            current_file = other_files[fidx]
            #type_str = paste(";type=",mime::guess_type(current_file),sep="")
            filepath_str = paste(";filename=",gsub(base_path,"",current_file),sep="")
            string_to_add = paste("-F",paste("'",names(pipeline_creation_request)[i],"=@",current_file,filepath_str,"'",sep=""))
            #string_to_add = paste("-F",paste("'",names(pipeline_creation_request)[i],"=@",current_file,filepath_str,type_str,"'",sep=""))
            curl_command = paste(curl_command,string_to_add)
          }
        }
      } else if(names(request)[i] == "parametersXmlFile"){
        string_to_add = paste("-F",paste("'",names(request)[i],"=@",request[[names(request)[i]]],";type=text/xml'",sep=""))
        curl_command = paste(curl_command,string_to_add)
      } else if(names(request)[i] == "mainNextflowFile" || names(request)[i] == "workflowCwlFile"){
        string_to_add = paste("-F",paste("'",names(request)[i],"=@",request[[names(request)[i]]],"'",sep=""))
        curl_command = paste(curl_command,string_to_add)
      } else{
        rlog::log_warn(paste("NOT SURE what to do with",names(request)[i],":",paste(request[[names(request)[i]]],collapse=", ")))
      }
    }
  }
  return(curl_command)
}
################################################
curl_command = create_curl_command(pipeline_creation_url,pipeline_creation_request)
rlog::log_info(paste("RUNNING:",curl_command))
###############
######################
num_retries = 0
max_retries = 4
if(!args$debug){
  pipeline_creation_response = rjson::fromJSON(json_str=system(curl_command,intern=T))
  #pipeline_creation_response = httr::POST(pipeline_creation_url,config=httr::add_headers("X-API-Key"=api_key), httr::accept("application/vnd.illumina.v3+json"),httr::content_type("multipart/form-data"),body = pipeline_creation_request,encode="multipart", verbose())
  #pipeline_creation_response_list = str(content(pipeline_creation_response, "parsed"))
  if(!"pipeline" %in% names(pipeline_creation_response)){
    rlog::log_warn(paste("Could not create pipeline for:",main_script))
    rlog::log_warn(paste("Retrying pipeline creation for:",main_script))
    while(num_retries < max_retries){
      ## change the name of the pipeline
      pipeline_creation_request[["code"]] = rename_pipeline_name(pipeline_name,api_key)
      pipeline_creation_request[["description"]] = paste("See",paste("https://github.com/nf-core/",dirname(main_script),sep=""))
      curl_command = create_curl_command(pipeline_creation_url,pipeline_creation_request)
      #rlog::log_info(paste("RUNNING:",curl_command))
      pipeline_creation_response = rjson::fromJSON(json_str=system(curl_command,intern=T))
      num_retries = num_retries + 1
    }
    #print(pipeline_creation_response_list)
    if(!"pipeline" %in% names(pipeline_creation_response)){
      rlog::log_error(paste("Could not create pipeline for",main_script))
      print(pipeline_creation_response)
    } else{
      rlog::log_info(paste("Pipeline successfully created for project",ica_project_id,"\nPipeline Id is:",pipeline_creation_response$pipeline$id))
    }
  } else{
    rlog::log_info(paste("Pipeline successfully created for project",ica_project_id,"\nPipeline Id is:",pipeline_creation_response$pipeline$id))
  }
}