process copy_workfiles {
    container 'keng404/utils:0.0.2'
    pod annotation: 'scheduler.illumina.com/presetSize', value: 'standard-medium'
    publishDir "out", mode:  'copy'
    time '1d'
    errorStrategy 'ignore'
    
    input:
        file(dummy_result) from dummy_output


    script:
    """
    # copy intermediate files + directories
    cp -r ${workflow.workDir} ${workflow.launchDir}/out
    # return trace files
    find /ces -type d -name "*.ica" | xargs -i cp -r {} ${workflow.launchDir}/out
    """
}