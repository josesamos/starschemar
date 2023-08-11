## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Troubleshooting why the package was archived

* There was a compatibility issue with the dplyr 1.1.0 package, it has been fixed now.  
   - dplyr::group_by(as.data.frame(ft), .dots = dim_keys) 
      becomes 
      dplyr::group_by(as.data.frame(ft), dplyr::across(dplyr::all_of(dim_keys)))
   - unique(append(class(ft), a_class)) 
      becomes 
      unique(c(a_class, class(ft)))
