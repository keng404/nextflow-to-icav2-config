options(stringsAsFactors=FALSE)
nfcore_manifest = read.csv('/Users/keng/icav2.nfcore_bundle.manifest.20220624.csv',header=FALSE)

path_or_dir <- function(path_of_interest){
  if(grepl(".tsv$",path_of_interest) || grepl(".csv$",path_of_interest)){
    return(TRUE)
  } else{
    return(FALSE)
  }
}

get_script <- function(path_of_interest){
  script_basename = "main.ica.dev.nf"
  path_split = strsplit(path_of_interest,"/")[[1]]
  if(path_or_dir(path_of_interest)){
    idx = length(path_split)-1
    if(sum(path_split %in% "assets") > 0){
      idx = (1:length(path_split))[path_split %in% "assets"][1] - 1
    }
    return(paste(paste(path_split[1:idx],collapse="/"),script_basename,sep="/"))
  } else{
    if(length(path_split) > 5){
      path_split[length(path_split)] = script_basename
      return(paste(path_split,collapse="/"))
    } else{
      return(paste(path_of_interest,script_basename,sep="/"))
    }
  }
}


scripts_to_test = apply(t(nfcore_manifest[,1]),2,get_script)

for(i in 1:length(scripts_to_test)){
  script_test_cmd = paste("Rscript test_nextflow_script.R --nextflow-script ",scripts_to_test[i])
  cat(paste(script_test_cmd),"\n")
}