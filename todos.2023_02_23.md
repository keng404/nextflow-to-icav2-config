# prereqs
1) python module nf-core installed
-- easier to do within Docker container
nextflow pipeline of interest that you are developing or a pipeline that is nf-core like
2) To run these scripts, you'll need to be in the same directory where you've downloaded them to (i.e. run a git clone command)
3) Git hub creds ???
4) A nextflow pipeline that is inspired by pipelines written by the nf-core community


# DOCUMENTATION TOPICS

## Why does this code exists
	 - programmatic update and creation of nf-core pipelines and nf-core 'like' pipelines into ICAv2
## HOW to run this code
	- Docker
	- Local
	- ICAv2 CLI with API Key generated

## ICA pipeline orientation
### How to create a pipeline in ICA
- XML file
	- do you have nextflow_schema.json
	- what if you don't?
- pipeline code (nextflow/CWL)
- Configuration files
	- how input is staged in ICA relative to ```workflow.launchDir``` and ```workflow.workDir```
	- troubleshooting runs + pulling it back into ICA
- Create pipeline in ICA
### Advanced topics
- Get mock CLI command to run pipeline
- Create Smoke tests for your pipeline

STEPS performed in this code:
 - STEP 1 : modify existing configs or copy template configs
	- STEP 2 : generate XML from ```nextflow_schema.json``` or generate XML from ```main.nf``` and config files
	- STEP 3 : make final edits to XML and create pipeline in ICA
	- STEP 4 : create CLI stub to launch pipeline
	- STEP 5 : create smoke tests for pipeline
#############################################
### features to revise/code to add
- [X]  Params to add
Change order params are added so that the params injected comes before params stripped
-  [X] Module names with ‘*’ 
```R
	if(grepl("\\*"),module_name){
		module_name = paste("'",module_name,"'",sep="")
	}
```
- Bugs with smoke test (Might have figured these out)
	- TBD
- Turn repo into R library 
- Documentation update  -- SEE ABOVE
- [ ] find ways to consume configurations hosted on GitHub and not locally --- so that parameters are set in XML
- [X] find paths to expand so that ICA can find them when running the pipeline --- add to ```develop_mode.downstream.R```
``` R
scripts_to_absolute_path = list()
binary_dir = paste(dirname(main_script),"bin",sep="")
scripts_to_absolute_path[[binary_dir]] = list()
assets_dir = paste(dirname(main_script),"assets",sep="")
scripts_to_absolute_path[[assets_dir]] = list()
if(dir.exists(binary_dir)){
	setwd(dirname(main_script))
	files_of_interest = file.list(binary_dir,recursive=T)
	basename_files_of_interest = apply(t(files_of_interest),2,basename)
	for(f in 1:length(files_of_interest)){
		scripts_to_absolute_path[[files_of_interest[f]]] = basename_files_of_interest[f]
	}
}
if(dir.exists(assets_dir)){
	setwd(dirname(main_script))
	files_of_interest = file.list(assets_dir,recursive=TRUE)
	basename_files_of_interest = apply(t(files_of_interest),2,basename)
	for(f in 1:length(files_of_interest)){
		scripts_to_absolute_path[[files_of_interest[f]]] = basename_files_of_interest[f]
	}
}

## function to update module based on these conditions
absolute_path_update_module <- function(module_file){
	module_file_data = read.delim(module_file,quote="",header=F)
	module_file_data = t(module_file_data)
	updated_lines = module_file_data
	paths_of_interest = names(scripts_to_absolute_path)
	found_updates = FALSE
	for(idx in 1:length(module_file_data)){
		found_update = FALSE
		module_line = module_file_data[idx]
		module_line_split = strsplit(module_line,"\\s+")[[1]]
		for(poi in 1:length(paths_of_interest)){
			relative_path_lookup = paths_of_interest[poi]
			basename_path_lookup = scripts_to_absolute_path[[paths_of_interest[poi]]]
			if(relative_path_lookup %in% module_line_split){
				replacement_value = paste("'$baseDir/",relative_path_lookup,"'",sep="")
				found_update = TRUE
				found_updates = TRUE
				module_line_split[module_line_split %in% relative_path_lookup] = replacement_value
			} else if(basename_path_lookup %in% module_line_split){
				replacement_value = paste("'$baseDir/",relative_path_lookup,"'",sep="")
				found_update = TRUE
				found_updates = TRUE
				module_line_split[module_line_split %in% basename_path_lookup] = replacement_value
			}
		}
		if(update_found){
			new_line = paste(module_line_split,collapse = " ",sep = " ")
			rlog::log_info(paste("UPDATING PATH in this line:",new_line))
			updated_lines[idx] = new_line
		} 
	}
	if(found_updates){
		output_file = paste(module_file,".tmp",sep="")
		write.table(update_lines,output_file,header=F,quote="")
		rlog::log_info(paste("UPDATING:",module_file))
		system(paste("mv",output_file,module_file))
	}
}

#### apply function above to all nextflow module files
module_dir = paste(dirname(main_script),"modules",sep="") ### make this configurable at runtime for one-off executions
module_files = file.list(module_dir,full.names=TRUE,recursive=TRUE)
if(length(module_files) > 0 ){
	for( m in 1:length(module_files)){
		rlog::log_info(paste("SCANNING for path updates in:",module_files[m]))
		absolute_path_update_module(module_file = module_files[m])
	}
}
```
