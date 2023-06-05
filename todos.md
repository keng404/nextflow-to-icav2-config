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
	1) Convert to API call to have more features for pipeline submission
		- will add more documentation + details  
- STEP 5 : create smoke tests for pipeline

#############################################
### features to revise/code to add
- [X] 1) Params to add to ```nextflow.config```
Change order params are added so that the params injected comes before params stripped
- [X] 2) Module names with ‘*’ 
- 3) Bugs with smoke test (Might have figured these out)
	- TBD
- [ ] 4) Turn repo into R library 
- [X] 5) Documentation update  -- SEE ABOVE
- [X] 6) find ways to consume configurations hosted on GitHub and not locally --- so that parameters are set in XML
	- Usually has to do with genome/reference configuration. For example [this](https://github.com/keng404/ica_nextflow_demos/blob/master/rnaseq/nextflow.config#L130). I have code to identify and interpret these expresssions, but I need to consume and figure out a creative way to  add this to the XML.  	
- [X] 7) find paths to expand so that ICA can find them when running the pipeline --- add to ```develop_mode.downstream.R```
- [ ] 8) Add a diff between the converted pipeline and the original soure --- generate PDF of differences
	- try using [diff2html](https://github.com/rtfpessoa/diff2html)
	- with addition Rmarkdown -> PDF report
- [ ] 9) helper function to 'update' pipeline
	- upload workflow, module,subworkflow, assets, and bin directories to ICA via the CLI
	- avoid behavior of creating new versions of pipelines each time
	- add tag to pipeline metadata?
		- versionComment in request body for [this ICA API endpoint](https://ica.illumina.com/ica/api/swagger/index.html#/Project%20Pipeline/createNextflowPipeline)
- [ ] 10) Add some comments about docker scope as implemented in nextflow ([DockerBuilder.groovy](https://github.com/nextflow-io/nextflow/blob/master/modules/nextflow/src/main/groovy/nextflow/container/DockerBuilder.groovy))
- [ ] 11) a html table that lists what nf-core pipelines have been lifted over
- [ ] 12) Troubleshooting guide --- what if you run into error
	- error message is likely related to the following:
		- 1) Check XML file to see if it's malformed or have the parameter/dataInput needed for your pipeline
		- 2) Check config (```nextflow.ica.config``` or ```conf/base.ica.config``` ) files for syntax errors
		- 3) Look at module, workflow,subworkflow, or main.nf files for any additional syntax errors
- [ ] 13) A guide with the scripts in this repository on how to set up pipeline runs on ICA via the CLI
