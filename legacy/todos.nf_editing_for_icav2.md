# IMPLEMENTATION TODO LIST:
- [X] 1) properly identify glob expressions or expressions as strings and not input paths
- [ ] 2) relative / absolute file paths in DSL 1 NF scripts
- [X] 3) get default container
- [X] 4) create lists in parameters XML where appropriate for the nf-core pipelines. 
 	```nextflow_schema.json``` files --under 'properties' have 'list' and 'enum' fields that can be used.
- [X] 5) add dummy process at end of script that:
      	       	- [X] copies intermediate results and logs for dev work
		- [X] grabs trace logs
- [X] 6) NF script updates
      	    	- [X] update params in main script and write updated script
		- [X] parse processes and propose updates saved in another object
		- [X] read updated script and whenever there's a process, update this script
- [X] 7) parse NFscripts for process enclosures
		- [X] look for labels, cpu, memory  ( edits where we remove)
		- [X] add pod anotation, container references when needed (edits where we add)
		- [ ] How to manage processes with no input expression? -- second pass?
		- [ ] How to select for process container where configurations point to a Groovy expression?
		- [ ] Configure whether users want singularity or docker images for their processes/modules --- currently we try to only use docker images
- [X] 8) add DSL2 support
 		- [X] make sure to track and properly edit multiple NF scripts ( main, workflow, subworkflow, module) that get referenced
 		- [X] properly pass publishDir and process.ext.args to downstream processes
- [ ] 9) add/reference user guide on additional development tips/topics when migrating nextflow pipelines to ICA

# DEVOPS TODO LIST:
- [X]  *install_packages.R* script that defines libraries to install
- [ ]   test script to perform dummy ```nextflow run``` command
- [X]   script containing CLI commands to create NF workflows in an ICA domain --- you must be logged in
- [ ]   demo/test containing CLI commands to stage data for testing out these workflows --- Demo data
- [ ]   script containing CLI commands to create project analysis runs for these workflows --- Jinja to parse XML -> JSON and then use CLI
- [X] 	Docker image that contains all the scripts, binaries, and libraries ready for all users to use thes scripts without having to worry about any setup
