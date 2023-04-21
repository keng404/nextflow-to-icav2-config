# XML formatting

When you look at your pipeline's XML file, you may notice that under the dataInputs section, there are 2 parameters : ```input_files``` and ```project_dir```.

These parameters allow you to specify multiple files and directories that will be staged in a workflow run's ```workflow.launchDir```.

When using these parameters and the ICA CLI/API, it allows you to specify any file you want without worry about it's format.
If you were to set up a workflow run in the ICA GUI, there are format checks that will only allow you to select files with a specific format.

It may not be trivial to initally setup your XML to capture all of the different [file formats that ICA sees](https://help.ica.illumina.com/reference/r-dataformats)

One concept design of many nf-core and nextflow pipelines is the use of an input samplesheet that provides metadata for the pipeline to run.
In ICA you would need to specify samplesheet and input data (i.e. FASTQ, BAM, VCF, etc.) in the dataInputs section of your XML.
instead of generating a specific pipeline layout, these scripts use the ```input_files``` and ```project_dir``` parameters to define these files, so that
one could launch pipelines more programatically.

Additionally, you may be developing the piepline or using the 'bleeding-edge' version of a pipeline, in that scenario the use of ```input_files``` and ```project_dir``` can be helpful.
This scenario is discussed more [here](https://github.com/keng404/nextflow-to-icav2-config#developer-mode-----if-you-plan-to-develop-or-modify-a-pipeline-in-ica)

You may decide to edit your XML to make it more specific to your use case and pipeline. That's totally ok and encouraged!

And thus when using the first pipeline conversion from these scripts, by default we encourage users to use the CLI/API when launching your pipeline,
even from the beginning. These scripts are also able to give you ICA CLI templates that you can modify and there is a JSON file containing values
that allow you to configure data and pipeline parameter values as you wish.

See [here](https://github.com/keng404/nextflow-to-icav2-config#how-to-run-a-pipeline-in--ica-via-cli) for more info on how to grab/generate a CLI template to run your pipeline.