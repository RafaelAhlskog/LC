# LC
R code for calculating jack-knifed "local context" measures based on geospatial data

This code was originally written to facilitate calculating individual-level jack-knifed neighborhood measures in situations where datasets differ in their individual identifiers, but have consistent spatial divisions (i.e. where the observations in one dataset are a subset of the observations in another larger dataset, but where cross-identification is either impossible due to inconsistent id's, or not permitted for e.g. contractual reasons). However, it can also be used to calculate these measures (fairly efficiently) in any single dataset that has coordinate or grid information. Both fixed area and fixed population size neighborhoods can be calculated, with maximum and minimum distance thresholds. Additionally, it's possible to jack-knife any given set of observations (i.e. "family members") that reside in the same location.

It's probably possible to make it run substantially faster, but this version runs ~200 times faster than the original implementation in Stata.

Proper documentation coming.
