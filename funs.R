library(rscopus)

if(is.na(getOption("elsevier_api_key", NA)) && !is.null(getOption("ScopusKey")))
     options(elsevier_api_key = getOption("ScopusKey"))

if(FALSE) {
  info = lapply(authorIDs, getAuthorPubs)
  # The next version will continue on if one fails for any reason.
#  info = lapply(authorIDs, function(id) try(getAuthorPubs(id)  ))
  # check fails
  # failed = sapply(info, is, "try-error")
  # authorIDs[failed]
  # explore with
  #   options(error = recover)
  #  getAuthorPubs(authorIDs[failed][1])
  names(info) = authorIDs
}

getAuthorPubs =
function(scopusID, docs = author_search(scopusID, ...),
           citations = lapply(docs$entries, getDocCitations), ...)
{
  w = sapply(citations, is.null)
  if(any(w)) {
     warning("dropping citation info for ", sum(w), " papers")
#     
# docs$entries[w]     
     citations = citations[ !w ]
     docs$entries = docs$entries[!w]
  }
  
   # combining all of the separate data frames corresponding to each cited paper
   # into one big one.  
  ans = do.call(rbind, citations)
   # Identifying each row with the actual author's paper id.
  pid = sapply(docs$entries, function(r) r[["dc:identifier"]])
  ans$PaperID = rep(pid, sapply(citations, nrow))

  ans$scopusID = scopusID # rep(scopusID, nrow(ans))
  
  ans
}


getDocCitations =
function(entry, id = entry[["dc:identifier"]],
         doc = abstract_retrieval(id, identifier = "scopus_id"))
{
  refs = doc$content$`abstracts-retrieval-response`$item$bibrecord$tail$bibliography$reference
  if("ref-info" %in% names(refs))
     return(processRef(refs))
  
  ans = lapply(refs, processRef)
   # each element of ans should be a data.frame with 7 columns
   # and a row for each of the authors
   # Stack them on top of each other into a single data frame for this one document
   #  each row refers to one citation. But there are several rows for each citation
   # corresponding to multiple authors.
  do.call(rbind, ans)
}


processRef =
function(ref)
{
   if(is.null(ref[["ref-info"]]))
      return(data.frame(`@seq` = NA, `ce:initials` = NA, `ce:indexed-name` = NA, 
                    `ce:surname` = NA, check.names = FALSE))
    
   id = getItemID( ref[["ref-info"]][["refd-itemidlist"]]$itemid )
   aut = getRefAuthors(ref)
   aut$id = id
   aut$idType = names(id)
   ti = ref[["ref-info"]][["ref-sourcetitle"]]
   aut$title = if(is.null(ti)) NA else ti
   aut
}

getRefAuthors =
function(ref)
{
   aut = ref[["ref-info"]][["ref-authors"]]
      # XXX 
   ans = if(length(aut) && length(aut$author)) {
     if("@seq" %in% names(aut$author)) {
       tmp = as.data.frame(aut$author, stringsAsFactors = FALSE)
       names(tmp) = names(aut$author)
       tmp
     } else 
       as.data.frame(do.call(rbind, aut$author), stringsAsFactors = FALSE)
   } else
     data.frame(`@seq` = NA, `ce:initials` = NA, `ce:indexed-name` = NA, 
                    `ce:surname` = NA, check.names = FALSE)

   if(!("ce:suffix" %in% names(ans)))
       ans[["ce:suffix"]] = NA
   if(!("ce:initials" %in% names(ans)))
       ans[["ce:initials"]] = NA   
   if(!("ce:given-name" %in% names(ans)))
       ans[["ce:given-name"]] = NA   
   
   ans
}

getItemID =
function(info, collapse = TRUE)
{
  if(length(names(info)) == 0) { 
     ans = structure(sapply( info,  `[[`, 2), names = sapply(info, `[[`, 1))
     if(collapse)
        structure(paste(ans, collapse = ";"), names = paste(names(ans), collapse = ";"))         
     else
        ans
  } else
     structure(info[[2]], names = info[[1]])
}


#setwd("/Users/jessicagold/Desktop/GSR")
#dir()
#test <- read.csv("Test_auth_id.csv", header = TRUE, stringsAsFactors = FALSE)
#citation_info = lapply(test$scopus_author_id, getAuthorPubs)
#saveRDS(citation_info, file = "citation_info.rds")

#library(data.table)
#citation_table <- Map(as.data.frame, citation_info)
#citation_table <- rbindlist(citation_table)
