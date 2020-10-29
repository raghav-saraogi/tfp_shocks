# tfp_shocks
A set of R scripts that illustrates the effects of two different types of TFP shocks on macroeconomic measures.

This code plots the response of core macroeconomic variables to changes in total factor productivity (TFP). A local projection approach is used to compute the response of these variables at various horizons to “shocks” to TFP. I use two different measures of the "shock" series:

1. Ford TFP Shocks from Francis, Owyang, Roush and DiCecio (2014).
2. Fernald TFP Series from the San Fracisco Fed.

The input data are contained in the "input" folder, and final graphs in the "output folder.

The user must have R programming language software installed to run this code.
The "_master.R" file is the only file the user must edit and run to generate all
outputs. In the "_master.R" file, the user must do the following:

1. Enter the location (directory) of this folder on their computer in line 21 
in place of "//SET WORKING DIRECTORY//".

2. replace the "//ENTER FRED API KEY//" in line 24 with their valid FRED API key
that the user can obtain from <https://research.stlouisfed.org/docs/api/api_key.html>.
