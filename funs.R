# options(elsevier_api_key = getOption("ScopusKey"))
library(rscopus)

if(FALSE) {
  info = lapply(authorIDs, getAuthorPubs)
  names(info) = authorIDs
}

getAuthorPubs =
function(scopusID, ...)
{
  docs = author_search(scopusID, ...)
  tmp = lapply(docs$entries, getDocCitations)
   # combining all of the separate data frames corresponding to each cited paper
   # into one big one.
  ans = do.call(rbind, tmp)
   # Identifying each row with the actual author's paper id.
  pid = sapply(docs$entries, function(r) entry[["dc:identifier"]])
  ans$PaperID = rep(pid, sapply(tmp, nrow))
  ans
}


getDocCitations =
function(entry, id = entry[["dc:identifier"]],
         doc = abstract_retrieval(id, identifier = "scopus_id"))
{

  refs = doc$content$`abstracts-retrieval-response`$item$bibrecord$tail$bibliography$reference
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
   if(length(aut)) {
     if(length(aut$author) > 1)
       as.data.frame(do.call(rbind, aut$author), stringsAsFactors = FALSE)
     else
       as.data.frame(aut$author[[1]], stringsAsFactors = FALSE)
   } else
     data.frame(`@seq` = NA, `ce:initials` = NA, `ce:indexed-name` = NA, 
                    `ce:surname` = NA)

}

getItemID =
function(info)
{
  structure(info[[2]], names = info[[1]])
}
