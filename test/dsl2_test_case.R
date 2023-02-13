library(rlog)
library(stringr)
source('dsl2_compatible_parser.R')
main_script = "/Users/keng/nf-core/rnaseq/main.nf"
scripts_to_check = find_all_nf_scripts(main_script=main_script)[["scripts_to_look_at"]]
if(length(scripts_to_check) > 0){
  workflow_script = scripts_to_check
  scripts_to_add = workflow_script
  scripts_to_parse = workflow_script
  while(length(scripts_to_add) > 0){
    if(length(scripts_to_add) > 0){
      rlog::log_info(paste("LOOKING to parse these files:",paste(scripts_to_add,collapse=", ")))
      scripts_to_parse = c()
      scripts_of_interest_metadata = NULL
      second_pass_scripts = c()
      path_exists_check = c()
      for(i in 1:length(scripts_to_add)){
        rlog::log_info(paste("Looking through the following script:",paste(scripts_to_add[i],sep=", ")))
        scripts_of_interest_metadata = find_all_nf_scripts(main_script=simplify_path(scripts_to_add[i]))
        if(!is.null(scripts_of_interest_metadata)){
          print(scripts_of_interest_metadata)
          scripts_of_interest = scripts_of_interest_metadata[["scripts_to_look_at"]]
          #print(scripts_of_interest)
        if(length(second_pass_scripts) > 0){
          path_exists_check  = apply(t(second_pass_scripts),2, function(x) does_path_exist(x))
          if(sum(!path_exists_check) > 0){
            fois = second_pass_scripts[!path_exists_check]
            stop(paste("DOUBLE-CHECK if these scripts exist:",paste(fois,collapse=", ")))
          }
        }
          scripts_to_parse = c(scripts_to_parse,scripts_of_interest[!scripts_of_interest %in% scripts_to_check])
          scripts_to_check = c(scripts_to_check,scripts_of_interest[!scripts_of_interest %in% scripts_to_check])
        }
      }
    }
    #print(scripts_to_parse)
    scripts_to_add = scripts_to_parse
  } 
} else{
  rlog::log_warn(paste("Did not find additional NF scripts to parse in addition to:",main_script))
}
scripts_to_check = c(main_script,apply(t(unique(scripts_to_check)),2,function(x) simplify_path(x)))
rlog::log_info(paste("Parsing through the following scripts:",paste(scripts_to_check,collapse=", ")))