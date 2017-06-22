if(FALSE)
d = data.frame(id = c("1234", "5678;10000", "no;yes", "123;456;789"),
               idType = c("SGR", "SGR;DOI", "DOI;SGR", "DOI;FOO;BAR"),
               stringsAsFactors = FALSE)

getSGR = 
function(df)
{
   type = strsplit(df$idType, ";")
   id = strsplit(df$id, ";")
   ans = mapply(function(x, types) {
                 names(x) = types
                 x[ "SGR" ]
                },
                id, type)
   ans
}
