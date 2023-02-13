url = "https://help.ica.illumina.com/project/p-flow/f-pipelines#compute-types"
#library(XML)
#doc <- htmlParse(url)

library(rvest)
html = read_html(url,encoding = "ISO-8859-1")
html_tr_nodes = html %>% html_elements("tr")
nodes_that_have_table_data = html %>% html_elements("tr") %>% html_attr("data-rnw-int-class")

#html_attrs(html_div_nodes[!is.na(nodes_that_have_table_data)][1])

nodes_to_check = (1:length(html_tr_nodes))[nodes_that_have_table_data == "table-row____"]
nodes_to_check = nodes_to_check[!is.na(nodes_to_check)]

computeTypes = FALSE
lines_to_keep = c()
for(i in 1:length(nodes_to_check)){
  text_of_interest = html_tr_nodes[nodes_to_check[i]] %>% html_text2()
  cpuReference = FALSE
  if(grepl("Compute Type",text_of_interest)){
    computeTypes = TRUE
    
  } else if(grepl("cpu",text_of_interest) || grepl("small",text_of_interest) || grepl("medium",text_of_interest) || grepl("large",text_of_interest)){
    cpuReference = TRUE
  } else{
    computeTypes = FALSE
  }
  
  if(computeTypes || cpuReference){
    #print(html_attrs(html_div_nodes[nodes_to_check[i]]))
    #print(html_div_nodes[nodes_to_check[i]] %>% html_text2())
    new_line = strsplit(text_of_interest,"\t|\n")[[1]]
    new_line = new_line[new_line!=""]
    lines_to_keep = rbind(lines_to_keep,new_line)
  }
}

header_line = c()
content_lines = c()
for(line in 1:nrow(lines_to_keep)){
  if(line == 1){
    ##header_line = strsplit(lines_to_keep[line],"\n")[[1]]
    header_line = lines_to_keep[line,]
  } else{
    ##content_lines = rbind(content_lines,strsplit(lines_to_keep[line],"\n")[[1]])
    content_lines = rbind(content_lines,lines_to_keep[line,])
  }
}
colnames(content_lines) = header_line
rownames(content_lines) = NULL
print(content_lines)