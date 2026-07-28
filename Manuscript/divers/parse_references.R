uppercase <- function(letter){
  if(letter %in% LETTERS){
    return(paste0("{", letter, "}"))
  }
  else{
    return(letter)
  }
}

biblio <- readLines("./Manuscript/references.bib")
for(i in 1:length(biblio)){
  line <- biblio[i]
  if(line != ""){
    spl_equal <- strsplit(line, split = " = ")[[1]]
    if(spl_equal[1] == "\ttitle"){
      ttl <- spl_equal[2]
      ttl_upr <- paste0(sapply(strsplit(ttl, split = "")[[1]],
                               FUN = uppercase), 
                        collapse = "")
      biblio[i] <- paste0(spl_equal[1], " = ", ttl_upr)
    }
  }
}

writeLines(biblio, "./Manuscript/references1.bib")
