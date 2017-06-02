
```r
source("funs.R")
o = getAuthorPubs("7005912940")
```

This currently drops citation information from 3 of the papers.
The reason for this is that there is no ref-info element in the 
reference object for each of these articles/papers.
There is an author element and this has more detailed information about 
the authors, including a scopus ID.  We can process this information.
However, we have to decide how/where to store it in the data frame.
