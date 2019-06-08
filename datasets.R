# Enum-like object to categorize input datasets
datasets <- function() {
  list(IRIS = list(id = 0, attrsCount = 4, firstAttrIdx = 1, lastAttrIdx = 4, 
                   examplesCount = 150, isInteger = FALSE), 
       LETTER_RECOGNITION = list(id = 1, attrsCount = 16, firstAttrIdx = 2, 
                                 lastAttrIdx = 17, examplesCount = 20000, isInteger = TRUE))
}

