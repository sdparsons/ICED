cran-comments
================
Thank you for checking this submission. The following notes I was unable to remove, but they pose no issues for the running of the package

# From devtools::check_win_release()

1. Possibly misspelled words in DESCRIPTION:
  IntraClass (2:8, 19:14)

IntraClass Effect Decomposition is the name of the method this package runs, so IntraClass is not misspelled here. 

# from CRAN submission checks (thank you Victoria Wimmer)

1. I have updated the description and added a reference
2. I have fixed the "Warning: Unexecutable code in man/iced_syntax.Rd:" error
3. I have removed \dontrun{}
4. "Please ensure that you do not use more than 2 cores in your examples,
vignettes, etc." - all code and examples default to a single core. 

