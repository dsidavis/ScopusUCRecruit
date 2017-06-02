
```r
source("funs.R")
o = getAuthorPubs("7005912940")
```

This currently drops citation information from 3 of the papers.
The reason for this is that the 'tail' in the element's bibrecord is NULL.
There is information in item-info element but we need to figure out what that is exactly.

