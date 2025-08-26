# nextflow-to-icav2-config
This is an unofficial developer tool to help develop Nextflow pipelines that will run successfully on ICA. There are some syntax bugs that may get introduced in your Nextflow code. One suggestion is to run the steps as described below and then open  these files in VisualStudio Code with the Nextflow plugin installed. You may also need to run smoke tests on your code to identify syntax errors you might not catch upon first glance. 

This is not an official Illumina product, but is intended to make your Nextflow experience in ICA more fruitful.

[![Ask DeepWiki](https://deepwiki.com/badge.svg)](https://deepwiki.com/keng404/nextflow-to-icav2-config)

Some examples of Nextflow pipelines that have been lifted over with this repo can be found [here](https://github.com/keng404/ica_nextflow_demos_v2).

Some additional examples of ICA-ported Nextflow pipelines are [here](https://github.com/keng404/ica_nextflow_demos). 

Some additional repos that can help with your ICA experience can be found below:
  - Relaunch pipeline analysis [here](https://github.com/keng404/bssh_parallel_transfer/blob/master/relaunch_pipeline.py) and [here](https://github.com/keng404/bssh_parallel_transfer/blob/master/requeue.md). An interactive application that can help you create CLI and API templates to submit pipeline runs can be found [here](https://keneng87.pyscriptapps.com/ica-analysis-requeue/latest/)
  - Monitor your analysis run in ICA and troubleshoot [here](https://github.com/keng404/monitor_ica_analysis_run)
  - Wrap a WDL-based workflow in a [CWL wrapper](https://github.com/keng404/wdl_test)
  - Wrap a Nextflow-based workflow in a [CWL wrapper](https://github.com/keng404/nextflow_test)


## Local testing your Nextflow pipeline after using these scripts
This [naive wrapper](https://github.com/keng404/nextflow-to-icav2-config/blob/main/testing_pipelines/test_nextflow_script.R) will allow you to test your main.nf script. If you have a Nextflow pipeline that is more nf-core like (i.e. where you may have several subworkflow and module files), this [script](https://github.com/keng404/nextflow-to-icav2-config/blob/main/testing_pipelines/nextflow_extended_local_testing.R) may be more appropriate. Any and all comments are welcome.



# What do these scripts do?

What these scripts do:
1) Parse configuration files and the Nextflow scripts (main.nf, workflows, subworkflows, modules) of a pipeline and update the configuration of the pipeline with pod directives to tell ICA what compute instance to run
  - Strips out parameters that ICA utilizes for workflow orchestration
  - Migrates manifest closure to ```conf/base.ica.config``` file
  - Ensures that docker is enabled
2) Adds ```workflow.onError``` (main.nf, workflows, subworkflows, modules) to aid troubleshooting
3) Modifies the processes that reference scripts and tools in the ```bin/``` directory of a pipeline's ```projectDir```, so that when ICA orchestrates your Nextflow pipeline, it can find and properly execute your pipeline process
4) Generates parameter XML file based on ```nextflow_schema.json, nextflow.config, conf/```
`- Take a look at [this](https://github.com/keng404/nextflow-to-icav2-config/blob/main/XML.md) to understand a bit more of what's done with the XML, as you may want to make further edits to this file for better usability
5) Additional edits to ensure your pipeline runs more smoothly on ICA

# ICA concepts to better understand ICA liftover of Nextflow pipelines

- Nextflow workflows on ICA are orchestrated by kubernetes and require a parameters XML file containing data inputs (i.e. files + folders) and other string-based options for all configurable parameters to properly be passed from ICA to your Nextflow workflows
- Nextflow processes will need to contain a reference to a container --- a Docker image that will run that specific process
- Nextflow processes will need a ```pod annotation``` specified for ICA to know what instance type to run the process.
  - A table of instance types and the associated CPU + Memory specs can be found [here](https://help.ica.illumina.com/project/p-flow/f-pipelines#definition) under a table named `Compute Types`

These scripts have been made to be compatible with [nf-core](https://github.com/nf-core) workflows, so you may find the concepts from the documentation here a better starting point.

# Using these scripts

The scripts mentioned below can be run in a docker image ```keng404/nextflow-to-icav2-config:0.0.7```

This has:
  - nf-core installed
  - All Rscripts in this repo with relevant R libraries installed
  - The ICA CLI installed, to allow for pipeline creation and CLI templates to request pipeline runs after the pipeline is created in ICA
  
You'll likely need to run the image with a docker command like this for you to be able to run git commands within the container:
```bash
docker run -itv `pwd`:`pwd` --platform=linux/amd64  keng404/nextflow-to-icav2-config:0.0.7 /bin/bash
```

where ````pwd```` is your directory where your Nextflow pipeline code is stored. As you are running this image as ```root``` you may want to change ownership of files generated by the commands below back to the ```id``` and ``gid``` of your user on your desktop/server.

# Prerequitsites

## STEP 0 Github credentials

## STEP 1 [OPTIONAL] : create JSON of nf-core pipeline metadata or specify pipeline of interest

If you have a specific pipeline from Github, you can skip this statement below.

You'll first need to download the python module from nf-core via a ```pip install nf-core``` command.
Then you can use nf-core list --json to return a JSON metadata file containing current pipelines in the nf-core repository. 

You can choose which pipelines to  ```git clone```, but as a convenience, the wrapper ```nf-core.conversion_wrapper.R ``` will perform a git pull, parse nextflow_schema.json files and generate parameter XML files, and then read configuration and Nextflow scripts and make some initial modifications for ICA development. Lastly, these pipelines are created in an ICA project of your choosing, so you will need to generate and download an API key from the ICA domain of your choosing.


## STEP 2: Obtain API key file

Next, you'll need an API key file for ICA that can be generated using the instructions [here](https://help.ica.illumina.com/account-management/am-iam#api-keys).

## STEP 3: Create a project in ICA

Finally, you'll need to create a project in ICA. You can do this via the CLI and API, but you should be able to follow these [instructions](https://help.ica.illumina.com/home/h-projects#create-new-project) to create a project via the ICA GUI.

## STEP 4: Download and configure the ICA CLI (see STEP 2):

Install ICA CLI by following these [installation instructions](https://help.ica.illumina.com/command-line-interface/cli-installation).

A table of all CLI releases for mac, linux, and windows can be found [here](https://help.ica.illumina.com/command-line-interface/cli-releasehistory).

The Project view should be the default view after logging into your private domain (https://my_domain.login.illumina.com) and clicking on your ICA 'card' ( This will redirect you to https://illumina.ica.com/ica).

## Let's do some liftovers
```bash
Rscript nf-core.conversion_wrapper.R --input {PIPELINE_JSON_FILE} --staging_directory {DIRECTORY_WHERE_NF_CORE_PIPELINES_ARE_LOCATED} --run-scripts {DIRECTORY_WHERE_THESE_R_SCRIPTS_ARE_LOCATED}  --intermediate-copy-template {DIRECTORY_WHERE_THESE_R_SCRIPTS_ARE_LOCATED}/dummy_template.txt --create-pipeline-in-ica --api-key-file {API_KEY_FILE} --ica-project-name {ICA_PROJECT_NAME} --nf-core-mode 

[OPTIONAL PARAMETER]
--git-repos {GIT_HUB_URL}
--pipeline-dirs {LOCAL_DIRECTORY_WITH_NEXTFLOW_PIPELINE}

[OPTIONAL FLAG]
--in-docker {identify if script is run within the docker image/container}
```

```GIT_HUB_URL``` can be specified to grab pipeline code from github. If you intend to liftover anything in the master branch, your ```GIT_HUB_URL``` might look like ```https://github.com/keng404/my_pipeline```. 
If there is a specific release tag you intend to use, you can use the convention ```https://github.com/keng404/my_pipeline:my_tag```.


# *** IF you are running the script within a Docker image
If you are running these scripts in a Docker image, ```--in-docker``` is a flag we recommend you do ***add*** to your command line to ensure that the sanity check is skipped. Sanity check is run in nextflow Docker images (docker within docker is not supported.

Alternatively, if you have a local copy/version of a Nextflow pipeline you'd like to convert and use in ICA, you can use the ```--pipeline-dirs``` argument to specify this.

In summary, you will need the following prerequisites, either to run the wrapper referenced above or to carry out individual steps below.
- 1) ```git clone``` nf-core pipelines of interest
- 2) Install the python module ```nf-core``` and create a JSON file using the command line ```nf-core list --json > {PIPELINE_JSON_FILE}```


# A detailed step-by-step breakdown of what ```nf-core.conversion_wrapper.R``` does for each Nextflow pipeline

## Step 1: Generate an XML file from nf-core pipeline (your pipeline has a [nextflow_schema.json](https://nf-co.re/pipeline_schema_builder))
```bash
Rscript create_xml/nf-core.json_to_params_xml.R --json {PATH_TO_SCHEMA_JSON}
```
- A Nextflow schema JSON is generated by nf-core's python library nf-core
- nf-core can be installed via a ```pip install nf-core``` command
```bash
nf-core schema build -d {PATH_NF-CORE_DIR}
```


## Step 2: Create a ```nextflow.config``` and a ```base config``` file so that it is compatible with ICA.
```bash
Rscript ica_nextflow_config.test.R --config-file {DEFAULT_NF_CONFIG} [OPTIONAL: --base-config-files  {BASE_CONFIG}] [--is-simple-config]
```
This script will update your configuration files so that it integrates better with ICA. The flag ```--is-simple-config``` will create a base config file from a template. This flag will also be active if no arguments are supplied to ```--base-config-files```.


## Step 3: Add helper-debug code and other modifications to your Nextflow pipeline
``` bash 
Rscript develop_mode.downstream.R  --config-file {DEFAULT_NF_CONFIG} --nf-script {MAIN_NF_SCRIPT} --other-workflow-scripts {OTHER_NF_SCRIPT1 } --other-workflow-scripts {OTHER_NF_SCRIPT2} ...  --other-workflow-scripts {OTHER_NF_SCRIPT_N}
```

This step adds some updates to your module scripts to allow for easier troubleshooting (i.e. copy work directory back to ICA if an analysis fails). It also allows for ICA's orchestration of your Nextflow pipeline to properly handle any script/binary in your ```bin/``` directory of your pipeline ```$projectDir```.

### Step 4: Update XML to add parameter options --- if your pipeline uses/could use iGenomes
```bash
Rscript update_xml_based_on_additional_configs.R --config-file {DEFAULT_NF_CONFIG} --parameters-xml {PARAMETERS_XML}
 ```

You may have to edit your ```{PARAMETERS_XML}``` file if these edits are unnecessary.

### Step 5: Sanity check your pipeline code to see if it is valid prior to uploading it into ICA
```bash
Rscript testing_pipelines/test_nextflow_script.R --nextflow-script {MAIN_NF_SCRIPT} --docker-image nextflow/nextflow:22.04.3 --nextflow-config {DEFAULT_NF_CONFIG}
```

[NOTE: 04-10-2023] Currently ICA supports Nextflow versions ```nextflow/nextflow:22.04.3``` and ```nextflow/nextflow:20.10.0```

## Step 6: Create a pipeline in ICA by using the following helper script ```nf-core.create_ica_pipeline.R```
```bash
Rscript nf-core.create_ica_pipeline.R --nextflow-script {NF_SCRIPT} --workflow-language nextflow --parameters-xml {PARAMETERS_XML} --nf-core-mode --ica-project-name {NAME} --pipeline-name {NAME} --api-key-file {PATH_TO_API_KEY_FILE}
```

### Developer mode --- if you plan to develop or modify a pipeline in ICA
Add the flag ```--developer-mode``` to the command line above if you have custom groovy libraries or modules files referenced in your workflow. When this flag is specified, the script will upload these files and directories to ICA and update the parameters XML file to allow you to specify directories under the parameters ```project_dir``` and files under ```input_files```. This will ensure that these files and directories will be placed in the ```$workflow.launchDir``` when the pipeline is invoked.

# How to run a pipeline in ICA via CLI
As a convenience, you can also get a templated CLI command to help run a pipeline (i.e. submit a pipeline request) in ICA via the following:
```bash
Rscript create_cli_templates_from_xml.R --workflow-language {xml or nextflow} --parameters-xml {PATH_TO_PARAMETERS_XML}
```
There will be a corrsponding JSON file (i.e. a file with a file extension ```*ICAv2_CLI_template.json```)  that saves these values that one could modify and configure to build out templates or launch the specific pipeline run you desire.
You can specify the name of this JSON file with the parameter ```--output-json```.

Once you modify this file, you can use ```--template-json``` and specify this file to create the CLI you can use to launch your pipeline. 

If you have a previously successful analysis with your pipeline, you may find this approach more useful.
 - Relaunch pipeline analysis [here](https://github.com/keng404/bssh_parallel_transfer/blob/master/relaunch_pipeline.py)  and [here](https://github.com/keng404/bssh_parallel_transfer/blob/master/requeue.md).

# ica_analysis_requeue_webapp
For a more interactive method of crafting both CLI and API templates, here is a pyscript-based web app that allows a user to generate CLI and API requeue templates for Illumina Connected Analytics (ICA)

See [here](https://keneng87.pyscriptapps.com/ica-analysis-requeue/latest/) to interact with it live.

# ica_analysis_troubleshooting_app
For an interactive method of downloading analysis logs and metadata for troubleshooting an ICA analysis, here is a pyscript-based web app that allows a user to grab logs and start troubleshooting analyses on Illumina Connected Analytics (ICA)

See [here](https://keneng87.pyscriptapps.com/ica-analysis-troubleshooting-app/latest/) to interact with it live.

# Creating your	own tests/pipeline runs	via the	CLI
Where possible, these scripts search for config files that refer to a test (i.e. test.config,test_full.config,test*config) and creates a boolean parameter ```params.ica_smoke_test```
that can be toggled on/off as a sanity check that the pipeline works as intended. By default, this parameter is set to ```false```.

When set to ```true```, these test config files are loaded in your main ```nextflow.config```.


# Additional todos
[todos](https://github.com/keng404/nextflow-to-icav2-config/blob/main/todos.md)
