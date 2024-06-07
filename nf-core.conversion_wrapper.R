options(stringsAsFactors=FALSE)
suppressPackageStartupMessages(library("argparse"))
library(stringr)
library(rjson)
library(rlog)
library(jsonlite)
# create parser object
parser <- ArgumentParser()

# specify our desired options 
# by default ArgumentParser will add an help option 

parser$add_argument("-i", "--input", default = NULL, required = FALSE,
                    help="input nf-core pipeline JSON")
parser$add_argument("-s","--staging-directory", "--staging_directory", default=NULL, 
                    required = TRUE, help="staging_directory to stage nf-core pipelines")
parser$add_argument("-r", "--run-scripts","--run_scripts", default=NULL,
                    help="run_script directory for this script")
parser$add_argument("-y", "--strict-xml-mode","--strict_xml_mode", action="store_true",default=FALSE,
                    help="Strictly define dataInputs and parameters in XML")
parser$add_argument("-d", "--dsl2-check","--dsl2_check", action="store_true",default=FALSE,
                    help="run_script directory for this pipeline")
parser$add_argument("-v", "--develop-mode","--develop_mode", action="store_true",default=FALSE,
                    help="develop a pipeline to run on ICA")
parser$add_argument("-u", "--update-xml","--update_xml", action="store_true",default=FALSE,
                    help="pre-flight update of XML file before creating the pipeline in ICA")
#parser$add_argument("-x","--generate-xml","--generate_xml", default=FALSE
#                    ,action="store_true", help = " auto-generate parameters XML file output -- independent from a nextflow_schema.json")
parser$add_argument("-c", "--create-pipeline-in-ica","--create_pipeline_in_ica", action="store_true",default=FALSE,
                    help="Create pipeline in ICA")
parser$add_argument("-a", "--api-key-file","--api_key_file", default = NULL,
                    help="API key file for ICA")
parser$add_argument("-p", "--ica-project-name","--ica_project_name", default = NULL,
                    help="ICA project name")
parser$add_argument("-n", "--pipeline-name-prefix","--pipeline_name_prefix", default = "test_pipeline_",
                    help="ICA pipeline name prefix")
parser$add_argument("-m","--nf-core-mode","--nf_core_mode",action="store_true",
                    default=FALSE, help = "flag to indicate nf-core pipeline")
parser$add_argument("-q","--in-docker","--in_docker",action="store_true",
                    default=FALSE, help = "flag to indicate if this script is run within a docker image/container")
parser$add_argument("-g","--git-repos","--git_repos",default=c(""),nargs="?",
                    help = "git repositories to convert")
parser$add_argument("-l","--pipeline-dirs","--pipeline_dirs", default=c(""),nargs="?",
                    action="append", help = "local directories to convert")
parser$add_argument("-t","--intermediate-copy-template","--intermediate_copy_template", default = NULL,
                    help = "default NF script to copy intermediate and report files from ICA")
parser$add_argument("-b","--base-ica-url","--base_ica_url",
                    default="ica.illumina.com", help = "ICA base URL")
parser$add_argument("-z","--time-from-last-update","--time_from_last_update",default=120,
                    help = "Identify tagged releases of pipeline that are 180 days or older")
args <- parser$parse_args()

input_json = NULL
git_repos = args$git_repos
git_repos = git_repos[git_repos != ""]
pipeline_dirs = args$pipeline_dirs
pipeline_dirs = pipeline_dirs[pipeline_dirs != ""]
time_from_last_update = args$time_from_last_update
#generate_xml = args$generate_xml
if(!is.null(args$input)){
  input_json = args$input
} #else{
#  stop(paste("Please define an input nf-core JSON of pipelines"))
#}
if(!is.null(args$staging_directory)){
  staging_directory = args$staging_directory
  if(!dir.exists(staging_directory)){
    rlog::log_info(paste("Creating a  staging directory for nf-core\n"))
    dir.create(staging_directory)
  }
} else{
  stop(paste("Please define a staging directory for nf-core pipelines"))
}
if(!is.null(args$run_scripts)){
  run_scripts = args$run_scripts
} else{
  stop(paste("Please define a scripts directory"))
}

if(args$create_pipeline_in_ica){
  if(is.null(args$api_key_file)){
    stop(paste("Please define an API key file for ICA"))
  } else{
    api_key_file = args$api_key_file
  }
  if(is.null(args$ica_project_name)){
    stop(paste("Please define an ICA project to add the pipelines"))
  } else{
    ica_project_name = args$ica_project_name
  }
  
} else{
  api_key_file = args$api_key_file
  ica_project_name = args$ica_project_name
}
#########
timestamp_diff_good <- function(pipeline_object,timestamp_cutoff=time_from_last_update){
  releases_good = c()
  rlog::log_info(paste("Checking for tagged release that are >= ",time_from_last_update,"days old"))
  for(i in 1:length(pipeline_object)){
    diff_in_s = as.integer(as.POSIXct( Sys.time() )) -  pipeline_object[[i]]$published_at_timestamp
    diff_in_days = diff_in_s/(60*60*24)
    if(diff_in_days >= timestamp_cutoff){
      releases_good = c(releases_good,TRUE)
    } else{
      releases_good = c(releases_good,FALSE)
    }
  }
  return(releases_good)
}
##################
nf_pipelines_metadata = list()
if(!is.null(input_json)){
  pipeline_metadata = rjson::fromJSON(file=input_json)
  
  ### grab metadata for NF pipelines
  number_of_pipelines = length(pipeline_metadata$remote_workflows)
  for(i in 1:number_of_pipelines){
    pipeline_releases = pipeline_metadata$remote_workflows[[i]][["releases"]]
    if(length(pipeline_metadata$remote_workflows[[i]][["releases"]]) > 0 ){
      pipeline_name = pipeline_metadata$remote_workflows[[i]][["name"]]
      pipeline_github_link = pipeline_metadata$remote_workflows[[i]][["full_name"]]
      ##################################
      recent_release = timestamp_diff_good(pipeline_object = pipeline_metadata$remote_workflows[[i]][["releases"]])
      if(sum(recent_release) > 0){
        number_of_releases = (1:length(recent_release))[recent_release == TRUE]
        number_of_releases = number_of_releases[length(number_of_releases)]
          # sorted by release date . the larger the number, the more recent the release
        if(number_of_releases > 1){
          release_number = number_of_releases
        } else{
          # or pick oldest
          release_number = 1
        }
      } else{
        # if I can't determine most recent release, pick earliest created
        release_number = 1
      }
      ########################
      pipeline_branch = pipeline_metadata$remote_workflows[[i]][["releases"]][[release_number]]$tag_name
      pipeline_description =  pipeline_metadata$remote_workflows[[i]][["description"]]
      pipeline_tags  =  pipeline_metadata$remote_workflows[[i]][["topics"]]
      nf_pipeline_metadata = list()
      nf_pipeline_metadata[["name"]] = pipeline_name
      nf_pipeline_metadata[["github_link"]] = pipeline_github_link
      nf_pipeline_metadata[["release_branch"]] = pipeline_branch
      nf_pipeline_metadata[["description"]] = pipeline_description
      nf_pipeline_metadata[["tags"]] = pipeline_tags
      nf_pipelines_metadata[[pipeline_name]] = nf_pipeline_metadata
    }
  }
  
  ### pull code from github
  rlog::log_info(paste("GRABBING nf-core pipelines from GitHub"))
  for(j in 1:length(names(nf_pipelines_metadata))){
    nf_pipeline = nf_pipelines_metadata[[names(nf_pipelines_metadata)[j]]]
    setwd(staging_directory)
    clone_cmd = paste("git clone",paste("https://github.com/",nf_pipeline[["github_link"]],".git",sep=""))
    rlog::log_info(paste("Step1: git clone",nf_pipeline[["github_link"]]))
    system(clone_cmd)
    new_dir = paste(staging_directory,nf_pipeline[["name"]],sep="/")
    setwd(new_dir)
    rlog::log_info(paste("Step2: checking out tag",nf_pipeline[["release_branch"]]))
    checkout_cmd = paste("git checkout",paste(nf_pipeline[["release_branch"]]))
    system(checkout_cmd)
  }
} else{
  ###
  if(length(git_repos) > 0){
    rlog::log_info(paste("GRABBING nextflow pipelines from GitHub"))
    for(j in 1:length(git_repos)){
      git_repo_of_interest = git_repos[j]
      tagname_split = strsplit(basename(git_repo_of_interest),"\\:")[[1]]
      git_branches_to_try = NULL
      if(length(tagname_split) > 1){
        git_branches_to_try = tagname_split[2]
        pipeline_name = tagname_split[1]
        git_repo_base_url = paste(dirname(git_repo_of_interest),pipeline_name,sep="/")
      } else{
        pipeline_name = tagname_split[1]
        git_repo_base_url = paste(dirname(git_repo_of_interest),pipeline_name,sep="/")
      }
      setwd(staging_directory)
      clone_cmd = paste("git clone",paste(git_repo_base_url,".git",sep=""))
      rlog::log_info(paste("Step1: git clone",pipeline_name))
      system(clone_cmd)
      new_dir = paste(staging_directory,pipeline_name,sep="/")
      setwd(new_dir)
      if(!is.null(git_branches_to_try)){
        rlog::log_info(paste("Step2: checking out tag",git_branches_to_try))
        checkout_cmd = paste("git checkout",paste(git_branches_to_try))
        system(checkout_cmd)
      }
      nf_pipeline_metadata = list()
      nf_pipeline_metadata[["name"]] = pipeline_name
      nf_pipeline_metadata[["github_link"]] = git_repo_base_url
      if(!is.null(git_branches_to_try)){
         nf_pipeline_metadata[["release_branch"]] = git_branches_to_try
      } else{
        nf_pipeline_metadata[["release_branch"]] = "main"
      }
      nf_pipelines_metadata[[pipeline_name]] = nf_pipeline_metadata
    }
  } else if(length(pipeline_dirs) >0){
    rlog::log_info(paste("CONVERTING  pipelines from local sources"))
    for(j in 1:length(pipeline_dirs)){
      pipeline_dir_of_interest = pipeline_dirs[j]
      setwd(staging_directory)
      copy_cmd = paste("cp -r",pipeline_dir_of_interest,".")
      rlog::log_info(paste("Migrate directory to staging directory"))
      system(copy_cmd)
    }
  } else{
    stop(paste("Provide  nextflow repos to convert"))
  }  
}
### generate parameter XML files for each pipeline in nf-core_pipelines metadata
rlog::log_info(paste("Generate parameter XML files"))
dirs_of_interest = list.dirs(staging_directory,recursive = FALSE) 
schema_jsons = list.files(staging_directory,pattern="nextflow_schema.json",full.names=T,recursive = T)
max_depth = min(apply(t(dirs_of_interest),2,function(x) length(strsplit(x,"/")[[1]])))
apply(t(schema_jsons),2,function(x) length(strsplit(x,"/")[[1]]) - max_depth)
valid_json_paths = apply(t(schema_jsons),2,function(x) (length(strsplit(x,"/")[[1]]) - max_depth ) == 1)
if(sum(valid_json_paths) == 0){
  stop(paste("Could not find valid nextflow_schema.json paths in ",staging_directory,sep = " ", collapse = " "))
} else{
  schema_jsons = schema_jsons[valid_json_paths]
}
for(k in 1:length(schema_jsons)){
  setwd(run_scripts)
  run_cmd = paste("Rscript create_xml/nf-core.json_to_params_xml.R --json",schema_jsons[k])
  if(args$nf_core_mode){
    run_cmd = paste(run_cmd,"--nf-core-mode")
  }
  if(args$strict_xml_mode){
    run_cmd = paste(run_cmd,"--strict-mode")
  }
  rlog::log_info(paste("Running",run_cmd))
  system(run_cmd)
} 
### generate updated NF files for each pipeline in nf-core_pipelines_metadata
### no dsl2 support currently
dsl2_enabled = function(nf_script){
  is_dsl2 = FALSE
  nf_script_dat = read.delim(nf_script,quote="",header=FALSE)
  for(i in 1:nrow(nf_script_dat)){
    skip_line = FALSE # --- line is a comment
    line_split = strsplit(nf_script_dat[i,],"\\s+")[[1]]
    clean_line = line_split
    for(t in 1:length(line_split)){
      sanitized_token = trimws(line_split[t])
      clean_line[t] = sanitized_token
    }
    clean_line = clean_line[clean_line!=""]
    if(grepl("/",clean_line[1])){
      skip_line = TRUE
    } else {
      if(length(clean_line) > 2){
        if(clean_line[1] == "nextflow.enable.dsl" && clean_line[3] == "2"){
          is_dsl2 = TRUE
          break
        }
      }
    }
  }
  if(is_dsl2){
    rlog::log_info(paste("SCRIPT",nf_script, "is DSL2 enabled"))
  }
  return(is_dsl2)
}
#################################
nextflow_scripts = list()
nextflow_configs = list()
dsl2_nextflow_scripts = list()
dsl2_nextflow_configs = list()
configs_to_ignore_list = list()
base_configs_list = list()


if(length(schema_jsons) >0 ){
  for(k in 1:length(schema_jsons)){
   rlog::log_info(paste("LOOKING IN DIRECTORY:",dirname(schema_jsons[k])))
   nextflow_script = list.files(dirname(schema_jsons[k]),pattern="main.nf$",full.names=T)
   nextflow_script = nextflow_script[!is.na(nextflow_script)]
   nextflow_script_bool = apply(t(nextflow_script),2, function(x) basename(x) == "main.nf")
   nextflow_script = nextflow_script[nextflow_script_bool]
   rlog::log_info(paste(schema_jsons[k],"Nextflow script",nextflow_script))
   main_config = list.files(dirname(schema_jsons[k]),pattern="nextflow.config",full.names=T)
   main_config = main_config[!is.na(main_config)]
   main_config_bool = apply(t(main_config),2, function(x) basename(x) == "nextflow.config")
   main_config = main_config[main_config_bool]
   rlog::log_info(paste(schema_jsons[k],"Nextflow Config",main_config))
   configs_to_ignore = list.files(dirname(schema_jsons[k]),pattern="*config",full.names=T,recursive=T)
   if(length(configs_to_ignore) > 0){
     configs_ignore_bool = apply(t(configs_to_ignore),2,function(x) basename(x) == "genomes.config")
     #####
     base_configs_bool = apply(t(configs_to_ignore),2,function(x) grepl("base\\.", basename(x)))
     base_configs = configs_to_ignore[base_configs_bool]
     base_configs = base_configs[!is.na(base_configs)][1]
     #################
     base_configs_list[[schema_jsons[k]]] = base_configs
     if(sum(configs_ignore_bool) > 0 ){
       configs_to_ignore = configs_to_ignore[configs_ignore_bool]
       configs_to_ignore = configs_to_ignore[!is.na(configs_to_ignore)]
       rlog::log_info(paste(schema_jsons[k],"Nextflow Configs to Ignore",paste(configs_to_ignore,collapse=",")))
       configs_to_ignore_list[[schema_jsons[k]]] = configs_to_ignore
     } else{
       configs_to_ignore = NULL
     }
     } else{
       configs_to_ignore = NULL
   }
   if(length(nextflow_script) > 0 && length(main_config) > 0){
     if(args$dsl2_check){
       if(!dsl2_enabled(nextflow_script)){
         nextflow_scripts[[schema_jsons[k]]] = nextflow_script
         nextflow_configs[[schema_jsons[k]]] = main_config
         base_configs_list[[schema_jsons[k]]] = base_configs
       } else{
         dsl2_nextflow_scripts[[schema_jsons[k]]] = nextflow_script
         dsl2_nextflow_configs[[schema_jsons[k]]] = main_config
         base_configs_list[[schema_jsons[k]]] = base_configs
       }
     } else{
       nextflow_scripts[[schema_jsons[k]]] = nextflow_script
       nextflow_configs[[schema_jsons[k]]] = main_config
       base_configs_list[[schema_jsons[k]]] = base_configs
     }
   } else{
     rlog::log_warn(paste("Could not find main script and config for",basename(dirname(schema_jsons))))
   }
  }
  
  if(length(names(nextflow_scripts)) > 0 ){
    all_nf_scripts = names(nextflow_scripts)
    scripts_to_create = all_nf_scripts[all_nf_scripts %in% names(nextflow_configs)]
    scripts_skipped = all_nf_scripts[!all_nf_scripts %in% scripts_to_create]
    ####################
    dsl2_scripts_to_update = names(dsl2_nextflow_scripts)
    scripts_to_create = c(scripts_to_create,dsl2_scripts_to_update)
    scripts_skipped =scripts_skipped[!scripts_skipped %in% dsl2_scripts_to_update]
    if(length(scripts_skipped) > 0){
      rlog::log_warn(paste("Skipping updates for",paste(scripts_skipped,collapse=", ")))
    }
    if(length(scripts_to_create) > 0 ){
      rlog::log_info(paste("Generating updates to NF scripts",paste(scripts_to_create,collapse=", ")))
    }
    for(l in 1:length(scripts_to_create)){
      setwd(run_scripts)
      if(!is.null(args$intermediate_copy_template)){
        rlog::log_info(paste("ADDING dummy process to copy intermediate files from",args$intermediate_copy_template))
        if(scripts_to_create[l] %in% names(nextflow_scripts)){
          if(scripts_to_create[l] %in% names(base_configs_list)){
            run_cmd = paste("Rscript ica_nextflow_config.test.R --config-file",nextflow_configs[[scripts_to_create[l]]],"--base-config-files" ,base_configs_list[[scripts_to_create[l]]] )
          } else{
            run_cmd = paste("Rscript ica_nextflow_config.test.R --config-file",nextflow_configs[[scripts_to_create[l]]])
          }
        } else{
          if(scripts_to_create[l] %in% names(base_configs_list)){
            run_cmd = paste("Rscript ica_nextflow_config.test.R --config-file",dsl2_nextflow_configs[[scripts_to_create[l]]],"--base-config-files" ,base_configs_list[[scripts_to_create[l]]] )
          } else{
            run_cmd = paste("Rscript ica_nextflow_config.test.R --config-file",dsl2_nextflow_configs[[scripts_to_create[l]]])
          }
        }
      } else{
        if(scripts_to_create[l] %in% names(nextflow_scripts)){
          if(scripts_to_create[l] %in% names(base_configs_list)){
            run_cmd = paste("Rscript ica_nextflow_config.test.R --config-file",nextflow_configs[[scripts_to_create[l]]],"--base-config-files" ,base_configs_list[[scripts_to_create[l]]] )
          } else{
            run_cmd = paste("Rscript ica_nextflow_config.test.R --config-file",nextflow_configs[[scripts_to_create[l]]])
          }
        } else{
          if(scripts_to_create[l] %in% names(base_configs_list)){
            run_cmd = paste("Rscript ica_nextflow_config.test.R --config-file",dsl2_nextflow_configs[[scripts_to_create[l]]],"--base-config-files" ,base_configs_list[[scripts_to_create[l]]] )
          } else{
            run_cmd = paste("Rscript ica_nextflow_config.test.R --config-file",dsl2_nextflow_configs[[scripts_to_create[l]]])
          }
        }
      }
      rlog::log_info(paste("Running",run_cmd))
      system(run_cmd)
    }
    } else{
      rlog::log_info(paste("No NF scripts found\nCheck",paste(all_nf_scripts,collapse = ", ")))
    }
}
####
### add in helpful 
if(length(names(nextflow_scripts)) > 0 ){
  all_nf_scripts = names(nextflow_scripts)
  scripts_to_create = all_nf_scripts[all_nf_scripts %in% names(nextflow_configs)]
  scripts_skipped = all_nf_scripts[!all_nf_scripts %in% scripts_to_create]
  ####################
  dsl2_scripts_to_update = names(dsl2_nextflow_scripts)
  scripts_to_create = c(scripts_to_create,dsl2_scripts_to_update)
  scripts_skipped =scripts_skipped[!scripts_skipped %in% dsl2_scripts_to_update]
  for(l in 1:length(scripts_to_create)){
    rlog::log_info(paste("key:",scripts_to_create[l]))
    rlog::log_info(paste("value:",nextflow_scripts[[scripts_to_create[l]]]))
    setwd(run_scripts)
    workflow_scripts = c()
    workflow_dir = paste(dirname(nextflow_scripts[[scripts_to_create[l]]]),"workflows",sep="/")
    if(dir.exists(workflow_dir)){
      rlog::log_info(paste("Looking for scripts here:",workflow_dir,collapse = " "))
      workflow_scripts_to_add = list.files(workflow_dir,pattern="*nf$",full.names=T,recursive = T)
      workflow_scripts = c(workflow_scripts,workflow_scripts_to_add)
    } else{
      rlog::log_info(paste("This dir does not exist",workflow_dir))
    }
    subworkflow_dir = paste(dirname(nextflow_scripts[[scripts_to_create[l]]]),"subworkflows",sep="/")
    if(dir.exists(subworkflow_dir)){        
      subworkflow_scripts_to_add = list.files(subworkflow_dir,pattern="*nf$",full.names=T,recursive = T)
      workflow_scripts = c(workflow_scripts,subworkflow_scripts_to_add)
    } else{
      rlog::log_info(paste("This dir does not exist",subworkflow_dir))
    }
    if(length(workflow_scripts) > 0){
      is_development_script = apply(t(workflow_scripts),2, function(x) grepl(".dev.nf$|.ica.nf$",basename(x)))
      workflow_scripts = workflow_scripts[!is_development_script]
    }
    if(scripts_to_create[l] %in% names(nextflow_scripts)){
      xml_files = list.files(dirname(nextflow_scripts[[scripts_to_create[l]]]),"*.pipeline.xml",full.names=T)
      xml_files = xml_files[!grepl("nfcore",xml_files)]
      xml_files = xml_files[!apply(t(xml_files),2,function(x) strsplit(basename(x),"\\.")[[1]][2] == "nf-core")]
      if(length(xml_files)>0){
        pipeline_name = paste(args$pipeline_name_prefix,strsplit(basename(xml_files[1]),"\\.")[[1]][1],sep="")
        run_cmd = paste("Rscript develop_mode.downstream.R  --config-file", nextflow_configs[[scripts_to_create[l]]], "--nf-script", nextflow_scripts[[scripts_to_create[l]]])
        #if(generate_xml){
         # rlog::log_info("AUTO_UPDATE_XML for:",xml_files[1])
        #  run_cmd = paste(run_cmd,"--generate-parameters-xml --parameters-xml", xml_files[1])
        #}
        if(length(workflow_scripts) > 0){
          for(wsi in 1:length(workflow_scripts)){
            run_cmd = paste(run_cmd,"--other-workflow-scripts", workflow_scripts[wsi])
          }
        }
        rlog::log_info(paste("NON_DSL2_PIPELINE: Adding helper-debug code",run_cmd))
        system(run_cmd)
        ##### adding genome options to XML where appropriate
        additional_xml_update_cmd = paste("Rscript update_xml_based_on_additional_configs.R --config-file",nextflow_configs[[scripts_to_create[l]]],"--parameters-xml",xml_files[1])
        rlog::log_info(paste("NON_DSL2_PIPELINE: Updating XML files",additional_xml_update_cmd))
        system(additional_xml_update_cmd)
      } else{
        rlog::log_warn(paste("CANNOT find xml for:",gsub(".nf$",".ica.dev.nf",nextflow_scripts[[scripts_to_create[l]]])))
      }
    }
    else{
        xml_files = list.files(dirname(dsl2_nextflow_scripts[[scripts_to_create[l]]]),"*.pipeline.xml",full.names=T)
        xml_files = xml_files[!grepl("nfcore",xml_files)]
        xml_files = xml_files[!apply(t(xml_files),2,function(x) strsplit(basename(x),"\\.")[[1]][2] == "nf-core")]
      if(length(xml_files)>0){
        pipeline_name = paste(args$pipeline_name_prefix,strsplit(basename(xml_files[1]),"\\.")[[1]][1],sep="")
        run_cmd = paste("Rscript develop_mode.downstream.R  --config-file", dsl2_nextflow_configs[[scripts_to_create[l]]], "--nf-script", dsl2_nextflow_scripts[[scripts_to_create[l]]])
        #if(generate_xml){
        #  rlog::log_info("AUTO_UPDATE_XML for:",xml_files[1])
        #  run_cmd = paste(run_cmd,"--generate-parameters-xml --parameters-xml", xml_files[1])
        #}
        if(length(workflow_scripts) > 0){
          for(wsi in 1:length(workflow_scripts)){
            run_cmd = paste(run_cmd,"--other-workflow-scripts", workflow_scripts[wsi])
          }
        }
        rlog::log_info(paste("DSL2_BASED_PIPELINE: Adding helper-debug code",run_cmd))
        system(run_cmd)
        ##### adding genome options to XML where appropriate
        additional_xml_update_cmd = paste("Rscript update_xml_based_on_additional_configs.R --config-file",dsl2_nextflow_configs[[scripts_to_create[l]]],"--parameters-xml",xml_files[1])
        rlog::log_info(paste("DSL2_BASED_PIPELINE: Updating XML files",additional_xml_update_cmd))
        system(additional_xml_update_cmd)
      } else{
        rlog::log_warn(paste("CANNOT find xml for:",gsub(".nf$",".ica.dev.nf",dsl2_nextflow_scripts[[scripts_to_create[l]]])))
      }
    }
  }
}
### Create our pipelines in ICA.
if(args$create_pipeline_in_ica){
  if(length(names(nextflow_scripts)) > 0 ){
    
    if(!grepl("'",ica_project_name)){
      ica_project_name = paste("'",ica_project_name,"'",sep="")
    }
    all_nf_scripts = names(nextflow_scripts)
    scripts_to_create = all_nf_scripts[all_nf_scripts %in% names(nextflow_configs)]
    scripts_skipped = all_nf_scripts[!all_nf_scripts %in% scripts_to_create]
    ####################
    dsl2_scripts_to_update = names(dsl2_nextflow_scripts)
    scripts_to_create = c(scripts_to_create,dsl2_scripts_to_update)
    scripts_skipped =scripts_skipped[!scripts_skipped %in% dsl2_scripts_to_update]
    for(l in 1:length(scripts_to_create)){
      #### TODOs : Add in check to  'test' nextflow code with no inputs prior to creating pipeline
      setwd(run_scripts)
      if(scripts_to_create[l] %in% names(nextflow_scripts)){
      sanity_check = paste("Rscript testing_pipelines/test_nextflow_script.R --nextflow-script", nextflow_scripts[[scripts_to_create[l]]],"--docker-image nextflow/nextflow:22.04.3","--nextflow-config",gsub(".config$",".ica.config",nextflow_configs[[scripts_to_create[l]]]))   
      rlog::log_info(paste("RUNNING SANITY_CHECK:",sanity_check))
      sanity_check_out = system(sanity_check,intern = T)
      passed_sanity_check = grepl("PASSED",sanity_check_out[length(sanity_check_out)])
      if(length(passed_sanity_check)==0){
        passed_sanity_check = FALSE
      }
      rlog::log_info(paste(nextflow_scripts[[scripts_to_create[l]]],"passed sanity check:",passed_sanity_check))
      if(!passed_sanity_check){
        rlog::log_error(paste(nextflow_scripts[[scripts_to_create[l]]],"passed sanity check:",passed_sanity_check))
      }
      xml_files = list.files(dirname(nextflow_scripts[[scripts_to_create[l]]]),"*.pipeline.xml",full.names=T)
      xml_files = xml_files[!grepl("nfcore",xml_files)]
      xml_files = xml_files[!apply(t(xml_files),2,function(x) strsplit(basename(x),"\\.")[[1]][2] == "nf-core")]
      if(length(xml_files)>0 & passed_sanity_check){
        pipeline_name = paste(args$pipeline_name_prefix,strsplit(basename(xml_files[1]),"\\.")[[1]][1],sep="")
          if(!is.null(ica_project_name)){
            run_cmd  = paste("Rscript nf-core.create_ica_pipeline.R --nextflow-script",nextflow_scripts[[scripts_to_create[l]]],"--workflow-language nextflow")
            run_cmd  = paste(run_cmd,"--nextflow-config",gsub(".config$",".ica.config",nextflow_configs[[scripts_to_create[l]]]))
            run_cmd  = paste(run_cmd,paste("--parameters-xml",xml_files[1],"--nf-core-mode --ica-project-name",ica_project_name,"--pipeline-name", pipeline_name,"--api-key-file", api_key_file))
            run_cmd  = paste(run_cmd,"--base-ica-url",args$base_ica_url)
            if(args$develop_mode){
              run_cmd = paste(run_cmd,"--developer-mode")
            }
            if(args$update_xml){
              run_cmd = paste(run_cmd,"--parameters-xml-override")
            }
            rlog::log_info(paste("Running",run_cmd))
            system(run_cmd)
          }
        } else{
        rlog::log_warn(paste("CANNOT find xml for:",gsub(".nf$",".nf",nextflow_scripts[[scripts_to_create[l]]])))
        }
      } else{
        xml_files = list.files(dirname(dsl2_nextflow_scripts[[scripts_to_create[l]]]),"*.pipeline.xml",full.names=T)
        xml_files = xml_files[!grepl("nfcore",xml_files)]
        xml_files = xml_files[!apply(t(xml_files),2,function(x) strsplit(basename(x),"\\.")[[1]][2] == "nf-core")]
        sanity_check = paste("Rscript testing_pipelines/test_nextflow_script.R --nextflow-script", dsl2_nextflow_scripts[[scripts_to_create[l]]],"--docker-image nextflow/nextflow:22.04.3","--nextflow-config",gsub(".config$",".ica.config",nextflow_configs[[scripts_to_create[l]]]))   
        # run sanity check if running in Docker image/container
        if(!args$in_docker){
          rlog::log_info(paste("RUNNING SANITY_CHECK:",sanity_check))
          sanity_check_out = system(sanity_check,intern = T)
          passed_sanity_check = grepl("PASSED",sanity_check_out[length(sanity_check_out)])
          if(length(passed_sanity_check)==0){
            passed_sanity_check = FALSE
          }
          rlog::log_info(paste(dsl2_nextflow_scripts[[scripts_to_create[l]]],"passed sanity check:",passed_sanity_check))
          if(!passed_sanity_check){
            rlog::log_error(paste(nextflow_scripts[[scripts_to_create[l]]],"passed sanity check:",passed_sanity_check))
          }
        }
        if(length(xml_files)>0 & passed_sanity_check){
          pipeline_name = paste(args$pipeline_name_prefix,strsplit(basename(xml_files[1]),"\\.")[[1]][1],sep="")
          if(!is.null(ica_project_name)){
            run_cmd  = paste("Rscript nf-core.create_ica_pipeline.R --nextflow-script",dsl2_nextflow_scripts[[scripts_to_create[l]]],"--workflow-language nextflow")
            run_cmd  = paste(run_cmd,"--nextflow-config",gsub(".config$",".ica.config",nextflow_configs[[scripts_to_create[l]]]))
            run_cmd  = paste(run_cmd,paste("--parameters-xml",xml_files[1],"--nf-core-mode --ica-project-name",ica_project_name,"--pipeline-name", pipeline_name,"--api-key-file", api_key_file))
            run_cmd  = paste(run_cmd,"--base-ica-url",args$base_ica_url)
            if(args$develop_mode){
              run_cmd = paste(run_cmd,"--developer-mode")
            }
            if(args$update_xml){
              run_cmd = paste(run_cmd,"--parameters-xml-override")
            }
            rlog::log_info(paste("Running DSL2-enabled pipeline creation",run_cmd))
            system(run_cmd)
          } 
        } else{
          rlog::log_warn(paste("CANNOT find xml for:",gsub(".nf$",".nf",dsl2_nextflow_scripts[[scripts_to_create[l]]])))
        }
      }
    }
  }
}
#######################
if(!is.null(staging_directory)){
  file_output = paste(staging_directory,"nf-core.ica_conversion.metadata.json",sep="/")
} else{
  file_output = "nf-core.ica_conversion.metadata.json"
}
nf_pipelines_metadata_json = jsonlite::toJSON(nf_pipelines_metadata,pretty=TRUE)
rlog::log_info(paste("Writing out nf-core pipeline metadata out to:",nf_pipelines_metadata_json))
write(nf_pipelines_metadata_json,file=file_output)