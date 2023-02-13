options(stringsAsFactors=FALSE)
suppressPackageStartupMessages(library("argparse"))
library(stringr)
library(rjson)
library(rlog)
# create parser object
parser <- ArgumentParser()

# specify our desired options 
# by default ArgumentParser will add an help option 

parser$add_argument("-i", "--input", default = NULL, required = TRUE,
                    help="input nf-core pipeline JSON")
parser$add_argument("-s","--staging-directory", "--staging_directory", default=NULL, 
                    required = TRUE, help="staging_directory to stage nf-core pipelines")
parser$add_argument("-r", "--run-scripts","--run_scripts", default=NULL,
                    help="run_script directory for this pipeline")
parser$add_argument("-d", "--no-dsl2-check","--no_dsl2_check", action="store_true",default=FALSE,
                    help="run_script directory for this pipeline")
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
parser$add_argument("-t","--intermediate-copy-template","--intermediate_copy_template", default = NULL,
                    help = "default NF script to copy intermediate and report files from ICA")
parser$add_argument("-b","--base-ica-url","--base_ica_url",
                    default="ica.illumina.com", help = "ICA base URL")
args <- parser$parse_args()

if(!is.null(args$input)){
  input_json = args$input
} else{
  stop(paste("Please define an input nf-core JSON of pipelines"))
}
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

pipeline_metadata = rjson::fromJSON(file=input_json)

### grab metadata for NF pipelines
number_of_pipelines = length(pipeline_metadata$remote_workflows)
nf_pipelines_metadata = list()
for(i in 1:number_of_pipelines){
  if(length(pipeline_metadata$remote_workflows[[i]][["releases"]]) > 0 ){
    pipeline_name = pipeline_metadata$remote_workflows[[i]][["name"]]
    pipeline_github_link = pipeline_metadata$remote_workflows[[i]][["full_name"]]
    pipeline_branch = pipeline_metadata$remote_workflows[[i]][["releases"]][[1]]$tag_name
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
library(rlog)
rlog::log_info(paste("GRABBING nf-core pipelines from GitHub"))
for(j in 1:length(names(nf_pipelines_metadata))){
  nf_pipeline = nf_pipelines_metadata[[names(nf_pipelines_metadata)[j]]]
  setwd(staging_directory)
  clone_cmd = paste("git clone",paste("https://github.com/",nf_pipeline[["github_link"]],".git",sep=""))
  rlog::log_info(paste("Step1: git clone",nf_pipeline[["github_link"]]))
  system(clone_cmd)
  new_dir = paste(staging_directory,nf_pipeline[["name"]],sep="/")
  #setwd(new_dir)
  #rlog::log_info(paste("Step2: checking out tag",nf_pipeline[["release_branch"]]))
  #checkout_cmd = paste("git checkout",paste(nf_pipeline[["release_branch"]]))
  #system(checkout_cmd)
}
### generate parameter XML files for each pipeline in nf-core_pipelines metadata
rlog::log_info(paste("Generate parameter XML files"))
dirs_of_interest = list.dirs(staging_directory,recursive = FALSE) 
schema_jsons = list.files(staging_directory,pattern="nextflow_schema.json",full.names=T,recursive = T)
additional_repos = dirs_of_interest[!apply(t(dirs_of_interest),2,function(x) sum(grepl(x,schema_jsons)) > 0 )]
for(k in 1:length(schema_jsons)){
  setwd(run_scripts)
  run_cmd = paste("Rscript nf-core.json_to_params_xml.R --json",schema_jsons[k])
  if(args$nf_core_mode){
    run_cmd = paste(run_cmd,"--nf-core-mode")
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


## running the full conversion loop for  scripts and non-standard-repos
for(ig in 1:length(additional_repos)){
  nextflow_script = paste(additional_repos[ig],"main.nf",sep="/")
  main_config  = paste(additional_repos[ig],"nextflow.config",sep="/")
  configs_to_ignore = list.files(dirname(nextflow_script),pattern="*config",full.names=T,recursive=T)
  if(length(configs_to_ignore) > 0 ){
    configs_ignore_bool = apply(t(configs_to_ignore),2,function(x) basename(x) == "genomes.config")
    configs_to_ignore = configs_to_ignore[configs_ignore_bool]
    configs_to_ignore = configs_to_ignore[!is.na(configs_to_ignore)]
    rlog::log_info(paste(nextflow_script,"Nextflow Configs to Ignore",paste(configs_to_ignore,collapse=",")))
  } else{
    configs_to_ignore = NULL
  }
  if(file.exists(nextflow_script) && file.exists(main_config)){
    run_cmd = paste("Rscript nf-core.ica_mod_nf_script.R","--nf-script",nextflow_script,"--config_file",main_config,"--intermediate-copy-template",args$intermediate_copy_template,"--generate-parameters-xml")
    if(length(configs_to_ignore) > 0){
      run_cmd = run_cmd
      for(p in 1:length(configs_to_ignore)){
        run_cmd = paste(run_cmd,"--configs-to-ignore",configs_to_ignore[p])
      }
    }
    rlog::log_info(paste("Running Non-Canonical script modification",run_cmd))
    system(run_cmd)
    
    xml_files = list.files(dirname(nextflow_script),"*.pipeline.xml",full.names=T)
    xml_files = xml_files[!grepl("nfcore",xml_files)]
    xml_files = xml_files[!apply(t(xml_files),2,function(x) strsplit(basename(x),"\\.")[[1]][2] == "nf-core")]
    pipeline_name = paste(args$pipeline_name_prefix,strsplit(basename(xml_files[1]),"\\.")[[1]][1],sep="")
    run_cmd  = paste("Rscript nf-core.create_ica_pipeline.R --nextflow-script",gsub(".nf$",".ica.dev.nf",nextflow_script,"--workflow-language nextflow"))
    run_cmd  = paste(run_cmd,paste("--parameters-xml",xml_files[1],"--nf-core-mode --ica-project-name",ica_project_name,"--pipeline-name", pipeline_name,"--api-key-file", api_key_file))
    run_cmd  = paste(run_cmd,"--simple-mode")
    rlog::log_info(paste("Running Non-Canonical pipeline creation",run_cmd))
    system(run_cmd)
  } else{
    rlog::log_warn(paste("SKIPPING CONVERSION for",dirname(nextflow_script)))
  }

}

if(length(schema_jsons) >0 ){
  for(k in 1:length(schema_jsons)){
   rlog::log_info(paste("LOOKING IN DIRECTORY:",dirname(schema_jsons[k])))
   nextflow_script = list.files(dirname(schema_jsons[k]),pattern="main.nf",full.names=T)
   nextflow_script = nextflow_script[!is.na(nextflow_script)]
   rlog::log_info(paste(schema_jsons[k],"Nextflow script",nextflow_script))
   main_config = list.files(dirname(schema_jsons[k]),pattern="nextflow.config",full.names=T)
   main_config = main_config[!is.na(main_config)]
   rlog::log_info(paste(schema_jsons[k],"Nextflow Config",main_config))
   configs_to_ignore = list.files(dirname(schema_jsons[k]),pattern="*config",full.names=T,recursive=T)
   if(length(configs_to_ignore) > 0){
     configs_ignore_bool = apply(t(configs_to_ignore),2,function(x) basename(x) == "genomes.config")
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
     if(!args$no_dsl2_check){
       if(!dsl2_enabled(nextflow_script)){
         nextflow_scripts[[schema_jsons[k]]] = nextflow_script
         nextflow_configs[[schema_jsons[k]]] = main_config
       } else{
         dsl2_nextflow_scripts[[schema_jsons[k]]] = nextflow_script
         dsl2_nextflow_configs[[schema_jsons[k]]] = main_config
       }
     } else{
       nextflow_scripts[[schema_jsons[k]]] = nextflow_script
       nextflow_configs[[schema_jsons[k]]] = main_config
     }
   } else{
     rlog::log_warn(paste("Could not find main script and config for",basename(dirname(schema_jsons))))
   }
  }
  
  if(length(names(nextflow_scripts)) > 0 ){
    all_nf_scripts = names(nextflow_scripts)
    scripts_to_update = all_nf_scripts[all_nf_scripts %in% names(nextflow_configs)]
    scripts_skipped = all_nf_scripts[!all_nf_scripts %in% scripts_to_update]
    ####################
    dsl2_scripts_to_update = names(dsl2_nextflow_scripts)
    scripts_to_update = c(scripts_to_update,dsl2_scripts_to_update)
    scripts_skipped =scripts_skipped[!scripts_skipped %in% dsl2_scripts_to_update]
    if(length(scripts_skipped) > 0){
      rlog::log_warn(paste("Skipping updates for",paste(scripts_skipped,collapse=", ")))
    }
    if(length(scripts_to_update) > 0 ){
      rlog::log_info(paste("Generating updates to NF scripts",paste(scripts_to_update,collapse=", ")))
    }
    for(l in 1:length(scripts_to_update)){
      setwd(run_scripts)
      if(!is.null(args$intermediate_copy_template)){
        rlog::log_info(paste("ADDING dummy process to copy intermediate files from",args$intermediate_copy_template))
        if(scripts_to_update[l] %in% names(nextflow_scripts)){
          run_cmd = paste("Rscript nf-core.ica_mod_nf_script.R","--nf-script",nextflow_scripts[[scripts_to_update[l]]],"--config_file",nextflow_configs[[scripts_to_update[l]]],"--intermediate-copy-template",args$intermediate_copy_template)
        } else{
          run_cmd = paste("Rscript nf-core.ica_mod_nf_script.R","--nf-script",dsl2_nextflow_scripts[[scripts_to_update[l]]],"--config_file",dsl2_nextflow_configs[[scripts_to_update[l]]],"--intermediate-copy-template",args$intermediate_copy_template)
          
        }
      } else{
        if(scripts_to_update[l] %in% names(nextflow_scripts)){
          run_cmd = paste("Rscript nf-core.ica_mod_nf_script.R","--nf-script",nextflow_scripts[[scripts_to_update[l]]],"--config_file",nextflow_configs[[scripts_to_update[l]]])
        } else{
          run_cmd = paste("Rscript nf-core.ica_mod_nf_script.R","--nf-script",dsl2_nextflow_scripts[[scripts_to_update[l]]],"--config_file",dsl2_nextflow_configs[[scripts_to_update[l]]])
        }
      }
      if(scripts_to_update[l] %in% names(configs_to_ignore_list)){
        configs_to_ignore = configs_to_ignore_list[[scripts_to_update[l]]]
        if(length(configs_to_ignore) > 0){
          run_cmd = run_cmd
            for(p in 1:length(configs_to_ignore)){
              run_cmd = paste(run_cmd,"--configs-to-ignore",configs_to_ignore[p])
            }
        }
      }
      if(scripts_to_update[l] %in% dsl2_scripts_to_update){
        run_cmd = paste(run_cmd,"--dsl2-enabled")
      }
      rlog::log_info(paste("Running",run_cmd))
      system(run_cmd)
    }
    } else{
      rlog::log_info(paste("No NF scripts found\nCheck",paste(all_nf_scripts,collapse = ", ")))
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
      setwd(run_scripts)
      if(scripts_to_create[l] %in% names(nextflow_scripts)){
      xml_files = list.files(dirname(nextflow_scripts[[scripts_to_create[l]]]),"*.pipeline.xml",full.names=T)
      xml_files = xml_files[!grepl("nfcore",xml_files)]
      xml_files = xml_files[!apply(t(xml_files),2,function(x) strsplit(basename(x),"\\.")[[1]][2] == "nf-core")]
      if(length(xml_files)>0){
        pipeline_name = paste(args$pipeline_name_prefix,strsplit(basename(xml_files[1]),"\\.")[[1]][1],sep="")
          if(!is.null(ica_project_name)){
            run_cmd  = paste("Rscript nf-core.create_ica_pipeline.R --nextflow-script",gsub(".nf$",".ica.dev.nf",nextflow_scripts[[scripts_to_create[l]]]),"--workflow-language nextflow")
            run_cmd  = paste(run_cmd,paste("--parameters-xml",xml_files[1],"--nf-core-mode --ica-project-name",ica_project_name,"--pipeline-name", pipeline_name,"--api-key-file", api_key_file))
            run_cmd  = paste(run_cmd,"--simple-mode","--base-ica-url",args$base_ica_url)
            rlog::log_info(paste("Running",run_cmd))
            system(run_cmd)
          }
        } else{
        rlog::low_warn(paste("CANNOT find xml for:",gsub(".nf$",".ica.dev.nf",nextflow_scripts[[scripts_to_create[l]]])))
        }
      } else{
        xml_files = list.files(dirname(dsl2_nextflow_scripts[[scripts_to_create[l]]]),"*.pipeline.xml",full.names=T)
        xml_files = xml_files[!grepl("nfcore",xml_files)]
        xml_files = xml_files[!apply(t(xml_files),2,function(x) strsplit(basename(x),"\\.")[[1]][2] == "nf-core")]
        if(length(xml_files)>0){
          pipeline_name = paste(args$pipeline_name_prefix,strsplit(basename(xml_files[1]),"\\.")[[1]][1],sep="")
          if(!is.null(ica_project_name)){
            run_cmd  = paste("Rscript nf-core.create_ica_pipeline.R --nextflow-script",gsub(".nf$",".ica.dev.nf",dsl2_nextflow_scripts[[scripts_to_create[l]]]),"--workflow-language nextflow")
            run_cmd  = paste(run_cmd,paste("--parameters-xml",xml_files[1],"--nf-core-mode --ica-project-name",ica_project_name,"--pipeline-name", pipeline_name,"--api-key-file", api_key_file))
            run_cmd  = paste(run_cmd,"--simple-mode","--base-ica-url",args$base_ica_url)
            rlog::log_info(paste("Running DSL2-enabled pipeline creation",run_cmd))
            system(run_cmd)
          } 
        } else{
          rlog::low_warn(paste("CANNOT find xml for:",gsub(".nf$",".ica.dev.nf",dsl2_nextflow_scripts[[scripts_to_create[l]]])))
        }
      }
    }
  }
}