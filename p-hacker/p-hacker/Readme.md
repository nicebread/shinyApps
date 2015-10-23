## R-Index, TIVA and p-curve

This R code implements the **p-curve** (Simonsohn, Nelson, & Simmons, 2013; see http://www.p-curve.com), the **R-Index**, and the **Test of Insufficient Variance, TIVA** (Schimmack, 2014; see http://www.r-index.org/).

Some code is from Uri Simonsohn (http://www.p-curve.com/Supplement/R/) and from Moritz Heene.

WARNING: This app is *not thoroughly tested yet*!

*Version 0.2*
---
### Known issues:

- p-curve plot missing
- pp33b function gives slightly different results than the original pp33 function
- TODO: Separate inference functions from UI functions
- *All* ES are used for calculations (the R-Index Excel sheet allows to differentiate between focal hypotheses etc.)
- Extension of p-curve to other alpha-levels should be tested more
- Implementation of p-uniform