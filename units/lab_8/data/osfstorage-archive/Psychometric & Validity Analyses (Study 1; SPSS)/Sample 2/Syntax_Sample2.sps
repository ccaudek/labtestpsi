* Encoding: UTF-8.

* Renaming variables
    
*RENAME VARIABLES (ES01_01 to ES03_08 = MJS1 to MJS24). 
*EXECUTE.

* Scale computations

COMPUTE MJS_Cognitive     = MEAN (MJS1 to MJS8). 
COMPUTE MJS_Emotional    = MEAN (MJS9 to MJS16). 
COMPUTE MJS_Behavioral    = MEAN (MJS17 to MJS24). 
EXECUTE.

* Internal consistency analysis + corrected item-total correlations

RELIABILITY
  /VARIABLES= MJS1 to MJS8
  /SCALE('MJS_Cognitive') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE
  /SUMMARY=TOTAL.

RELIABILITY
  /VARIABLES= MJS9 to MJS16
  /SCALE('MJS_Emotional') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE
  /SUMMARY=TOTAL.

RELIABILITY
  /VARIABLES= MJS17 to MJS24
  /SCALE('MJS_Behavioral ') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE
  /SUMMARY=TOTAL.

* Descriptive statistics
    
DESCRIPTIVES VARIABLES=MJS_Cognitive MJS_Emotional MJS_Behavioral
  /STATISTICS=MEAN STDDEV MIN MAX KURTOSIS SKEWNESS.
