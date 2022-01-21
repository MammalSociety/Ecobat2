# Ecobat2
New version of Ecobat
FRIDAY 21 JAN 2022

Files contained in this repository: server and ui, plus ecobat2.r script to run the app. Nightly.rmd file to run Ecobat report on the data. 
The ecobat.r script prepared the data for the Nightly file, at the end of ecobat.r it creates a Bprp dataframe which is picked up at the start of Nightly.

ggforce: the package needs to be taken from the version on the MammalSocity repository for facet_wrap_paginate function in Nightly script to work.

Charlie still needs to switch .csv dataframe to rdata format. However it is possible the issue is not related to actual memory problem because the "out of memory" issue has
cropped up at various points of the ecobat.r script for example, it previously got as far as putting the Bprep data frame together. It did previously also run the entire way
through and produce a report from Shiny.io.

In order for this code to run you will also need: an example proforma to upload and access to where the dataframe is stored.
