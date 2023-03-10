options(stringsAsFactors=FALSE)
library(argparse)
library(rvest)
library(rlog)
library(XML)
# create parser object
parser <- ArgumentParser()
##"https://raw.githubusercontent.com/nf-core/configs/master/conf/pipeline/viralrecon/genomes.config",###
# specify our desired options 
# by default ArgumentParser will add an help option 
parser$add_argument("-c","--config-file","--config_file", default=NULL,
                    help = "nextflow config file to parse")
parser$add_argument("-u","--config-url","--config_url",
                    default=NULL, help = "URL for config")
parser$add_argument("-x","--parameters-xml","--parameters_xml", default=NULL,
                    required=TRUE, help = "parameters XML file")
parser$add_argument("-o","--output-file","--output_file",
                    default="test_parsed_json_like.txt", help = "output file with attempted conversion")
args <- parser$parse_args()
config_url = args$config_url
config_file = args$config_file
output_file = args$output_file
parameter_xml_file = args$parameters_xml
if(!is.null(config_url)){
    test_doc = rvest::read_html(config_url,encoding = "ISO-8859-1")
    test_content = strsplit(rvest::html_text(test_doc),"\n")[[1]]
} else if(!is.null(config_file)){
    test_content = t(t(read.delim(config_file,header=F,quote="")))
    test_content = as.vector(test_content)
    output_file = paste(dirname(config_file),output_file,sep="/")
} else{
    rlog::log_error(paste("No config file or URL provided\nExiting\n"))
    stop(paste("Please define a URL to a nextflow config[ --config-url ] or a path to a nextflow config [ --config-file ]"))
}
#################################
remove_comments_from_config <- function(config_content){
    clean_lines = c()
    in_comment_block = FALSE
    for( i in 1:length(config_content)){
        config_line = config_content[i]
        modified_line = FALSE
        # remove ${params.mystring} expressions to help with our parser
        remove_groovy_expression = TRUE
        while(remove_groovy_expression){
          if(grepl("\\$\\{",config_line) & grepl("\\}",config_line)){
            config_line = sub("\\$\\{","",config_line)
            config_line = sub("\\}","",config_line)
            modified_line = TRUE
          } else{
            remove_groovy_expression = FALSE
          }
        }
        remove_squiggly_bracket_pairs = TRUE
        while(remove_squiggly_bracket_pairs){
          if(grepl("\\{",config_line) & grepl("\\}",config_line)){
            config_line = sub("\\{","",config_line)
            config_line = sub("\\}","",config_line)
            modified_line = TRUE
          } else{
            remove_squiggly_bracket_pairs  = FALSE
          }
        }
        config_line_split = strsplit(config_line,"\\s+")[[1]]
        clean_line = apply(t(config_line_split),2,trimws)
        clean_line = clean_line[clean_line!=""]
        is_comment_line = FALSE
        if(length(clean_line) > 0){
            rlog::log_info(paste("CLEAN_LINE_TOKEN:",clean_line[1]))
            if(grepl("\\/\\*",clean_line[1])){
                rlog::log_info(paste("FOUND comment token:",clean_line,collapse=" "))
                in_comment_block = TRUE
            } else if(grepl("\\*\\/",clean_line[1])){
                rlog::log_info(paste("FOUND comment token:",clean_line,collapse=" "))
                in_comment_block = FALSE
                next
            } 
            if(clean_line[1] == "//"){
                is_comment_line = TRUE
            }
            if(!in_comment_block & !is_comment_line){
              if(!modified_line){
                clean_lines = c(clean_lines,config_content[i])
              } else{
                clean_lines = c(clean_lines,config_line)
              }
            }
        }
    }
    return(c("{",clean_lines,"}"))
}
################
config_to_json_like <- function(config_content){
    updated_lines = c()
    for( i in 1:length(config_content)){
        add_comma = TRUE
        config_line = config_content[i]
        config_line = gsub("\'","\"",config_line)
        config_line_split = strsplit(config_line,"\\s+")[[1]]
        clean_line = apply(t(config_line_split),2,trimws)
        left_brackets_bool = apply(t(config_line_split),2,function(x) x =="{")
        right_brackets_bool = apply(t(config_line_split),2,function(x) x =="}")
        clean_line = clean_line[clean_line!=""]
        clean_line_split = strsplit(clean_line,"\\s+")[[1]]
        left_bracket_bool = unlist(apply(t(config_line_split),2,function(x) grepl("\\{",x)))
        #rlog::log_info(paste(paste("LINE_OF_INTEREST:",config_line,sep=" ",collapse=" ")))
        #rlog::log_info(paste(paste("LEFT_BRACKET_BOOLEAN:",left_bracket_bool,sep=" ",collapse= " ")))
        if(grepl("\\[",clean_line[length(clean_line)] ) | grepl("\\]",clean_line[length(clean_line)] )  | grepl("\\,$",clean_line[length(clean_line)]) ){
          add_comma = FALSE
        }
        if(clean_line[length(clean_line)] == "}" | grepl("\\{",clean_line[length(clean_line)])){
            add_comma = FALSE
        } 
        if(i < length(config_content) -1){
            next_config_line_split = strsplit(config_content[i+1],"\\s+")[[1]]
            next_line_right_bracket_bool = unlist(apply(t(next_config_line_split),2,function(x) grepl("\\}",x))) 
            next_line_right_closed_bracket_bool = unlist(apply(t(next_config_line_split),2,function(x) grepl("\\]",x))) 
            next_line_right_bracket_bool = next_line_right_bracket_bool | next_line_right_closed_bracket_bool
            if(sum(next_line_right_bracket_bool) > 0){
                rlog::log_info(paste('hello1:',config_line,sep=" "))
                add_comma = FALSE
            } else if(grepl("\\{$",config_content[i+1]) & sum(left_bracket_bool) ==0 ){
                rlog::log_info(paste('hello2:',config_line,sep=" "))
                add_comma = TRUE
            }
        }
        if(length(clean_line) > 1){
          if(clean_line[2] == "{"){
                config_line = sub("\\{",": {",config_line)
          }
          if(clean_line[2] == "="){
                config_line = sub("\\=",":",config_line)
                #if(!grepl("\"",clean_line[length(clean_line)]) & (!grepl("\\[",clean_line[length(clean_line)]) & !grepl("\\]",clean_line[length(clean_line)]))){
                #  token_of_interest = clean_line[length(clean_line)]
                #  double_quoted_token_version = paste("\"",clean_line[length(clean_line)],"\"",sep="")
                #  config_line = sub(token_of_interest,double_quoted_token_version,config_line)
                #}
          } 
        }
        if(grepl("\\=",config_line)){
          config_line = gsub("\\="," : ",config_line)
          config_line_split = strsplit(config_line,"\\s+")[[1]]
          clean_line = apply(t(config_line_split),2,trimws)
          clean_line = clean_line[clean_line!=""]
        }
        if(!grepl("\"",clean_line[1]) & (clean_line[1] != "{" & clean_line[1] != "}" & clean_line[1] != "[" & clean_line[1] != "]")){
            first_token_match = clean_line[1]
            double_quoted_token_version = paste("\"",clean_line[1],"\"",sep="")
            config_line = sub(first_token_match,double_quoted_token_version,config_line)
        } 
        if(add_comma){
            config_line = paste(trimws(config_line,which = "right"),",",sep ="")
        }

        #replace brackets
        if(grepl("\\[",clean_line[length(clean_line)] ) | grepl("\\]",clean_line[length(clean_line)] )){
          config_line = sub("\\[","{",config_line)
          config_line = sub("\\]","}",config_line)
        }
        if(grepl("\"",config_line) & !grepl("\\:",config_line)){
          #config_line = sub("\\s+{2}"," : ",config_line)
          config_line = trimws(config_line)
          line_splitt = strsplit(config_line,"\\s+")[[1]]
          if(length(line_splitt) > 0){
            rlog::log_warn(paste(line_splitt,sep="__",collapse="--"))
            if(sum(line_splitt == "") >0 ){
              spaces_in_line = 1:length(line_splitt)[line_splitt == ""]
            } else{
              spaces_in_line = c()
            }
            rlog::log_warn(paste(line_splitt))
            if(length(spaces_in_line) > 0){
              line_splitt[spaces_in_line[1]] = " : "
              config_line = paste(line_splitt,sep = " ",collapse = " ")
            }
          }
        }
        updated_lines = c(updated_lines,config_line)
    }
    return(updated_lines)
}
test_parsed_cleaned = remove_comments_from_config(config_content = test_content)
test_parsed_json_like = config_to_json_like(config_content = test_parsed_cleaned)
rlog::log_info(paste("Writing converted config file to JSON-like format:",output_file,collapse=" ",sep=" "))
write.table(test_parsed_json_like,file=output_file,quote=F,row.names=F,col.names=F,sep="\n")
###############################################
parse_json <- function(converted_file){
out = tryCatch(
        {
          jsonlite::fromJSON(converted_file)
        }, 
         error = function(cond){
           rlog::log_warn(cond)
           rlog::log_warn(paste("Could not properly convert:",converted_file))
           return(NULL)
         }
        )
return(out)
}

genome_in_parameters_list <- function(parameter_xml){
  parameter_names = names(parameter_xml)
  found_genome = FALSE
  parameter_list = list()
  for(i in 1:length(parameter_names)){
    tools = parameter_xml[i][["step"]][["tool"]]
    for(j in 1:length(tools)){
      param_setting = tools[j]
      if("parameter" %in% names(param_setting)){
        rlog::log_info(paste("looking at",param_setting))
        parameter_name = param_setting[["parameter"]][[".attrs"]][["code"]]
        if(parameter_name == "genome"){
          found_genome = TRUE
        }
      }
    }
  }
  return(found_genome)
}

add_genome_to_parameters_list <- function(keys_to_add,xml_file){
  doc = xmlTreeParse(parameter_xml_file,useInternalNodes = TRUE)
  root = xmlRoot(doc)
  ###### Grab all parameters from XML under each tool and output to list ----
  tools = root[["steps"]][[1]][["tool"]]
  #######
  nested_parameter_node = XML::newXMLNode("parameter",parent=tools)
  xmlAttrs(nested_parameter_node) = c(code = "genome",minValues = "1",maxValues="1",classification="USER")
  newXMLNode("label","genome",parent=nested_parameter_node)
  newXMLNode("description","Select Genome:",parent=nested_parameter_node)
  if(length(keys_to_add) > 0){
    options_node = XML::newXMLNode(paste("optionsType"),parent=nested_parameter_node)
    for(lv in 1:length(keys_to_add)){
      newXMLNode("option",keys_to_add[lv],parent=options_node)
    }
  }
  newXMLNode("value",NULL,parent=nested_parameter_node)
  outputPath = gsub(".xml$",".updated.xml",xml_file)
  #rlog::log_info(paste("Updating parameters XML here:",outputPath))
  #prefix='<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n'
  prefix.xml <- "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n"
  saveXML(doc , file=outputPath,encoding="utf-8")
  system(paste("mv",outputPath,xml_file))
  rlog::log_info(paste("Updating parameters XML to:",xml_file))
}

#################################################
params_list = parse_json(converted_file=output_file)
if(is.null(params_list)){
  if(!is.null(config_url)){
    rlog::log_error(paste("Error converting:",config_url))
  } else{
    rlog::log_error(paste("Error converting:",config_file))
  }
} else{
  rlog::log_info(paste("modifying XML with params_list"))
  # check if genome is in parameters section of XML
  # if not, add options,
  # if yes, add names(params_list[["params"]][["genomes"]]) to options
  #########################################
  rlog::log_info(paste("PARSING parameters XML file:",parameter_xml_file))
  doc = XML::xmlToList(parameter_xml_file)
  ###### Grab all parameters from XML under each tool and output to list ----
  tool_names = doc[["steps"]]
  add_genome_to_xml = !genome_in_parameters_list(tool_names)
  rlog::log_info(paste("add_genome_to_xml:", add_genome_to_xml))
  if(add_genome_to_xml){
    #add_genome_to_parameters_list
    add_genome_to_parameters_list(keys_to_add = names(params_list[["params"]][["genomes"]]),xml_file = parameter_xml_file)
  } else{
    #modify_genome_in_parameters_list
    rlog::log_warn(paste(c("Parameter[ genome ] is already present in your XML. Double-check that the following options are selectable for genome:",names(params_list[["params"]][["genomes"]]))))
  }
}

