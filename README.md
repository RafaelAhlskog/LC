# LC
R code for calculating jack-knifed "local context" measures based on geospatial data

This code was written to facilitate calculating jack-knifed neighborhood measures in situations where datasets differ in their individual identifiers, but have consistent spatial divisions. Both fixed area and fixed population size neighborhoods can be calculated, with maximum and minimum distance thresholds. Additionally, it's possible to jack-knife any given set of observations (i.e. "family members") that reside in the same location.

It's probably possible to make it run substantially faster, but this version runs ~200 times faster than the original implementation in Stata.

Proper documentation coming.
