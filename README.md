# 2017_AEM_Figure_S2
Interactive plots of NMDS results

## Launching the App
1. Download R, if necessary: https://www.r-project.org/ 
2. Dowload R Studio, if necessary: https://www.rstudio.com/products/rstudio/download/
3. Open R Studio, and install the shiny package, if necessary, by typing the following command into R Studio's console:
  ```
  install.packages("shiny")
  ```
4. Install ggplot2, if necessary, by typing the following command into R Studio's console:
  ```
  install.packages("ggplot2")
  ```
5. Launch the app by typing the following command into R Studio's console:
  ```
  runGitHub("2017_AEM_Figure_S2","lmc297")
  ```

## Visualizing NMDS Data with the App
This app can be used to visualize and explore the non-metric multidimensional scaling (NMDS) results presented
in this manuscript. Users can view different data sets by selecting different options in the panel to the right of the plot.
These options are described below.

### Selecting a data set to visualize.

#### Serotype(s)
Select which serotype(s) you would like to display by choosing an option from the drop-down menu. Options are (i) All Serotypes
(Typhimurium, Newport, Dublin), (ii) Typhimurium, (iii) Newport, and (iv) Dublin.

#### AMR Data Set
Select which data set you would like to view. The data sets are:
  1. AMR Gene Sequences: NMDS was performed using a distance matrix created using the sequences of AMR genes detected in each 
  genome and their unweighted UniFrac distances.

  2. AMR Phenotypes (Resistance/Susceptible): NMDS was performed using a presence/absence matrix, where a "resistant" phenotype
  (resistant or intermediately-resistant) was denoted by "1" and a susceptible phenotype was denoted by "0". Raup-Crick
  dissimilarities were used.

  3. Plasmid Replicon Presence/Absence: NMDS was performed using a presence/absence matrix, where plasmid replicons present in a
  genome were denoted by "1", and those that were not detected in the genome were denoted by "0". Raup-Crick dissimilarities
  were used.

#### Grouping Variable
Display convex hull(s) around isolate points with a particular attribute. For example, selecting "Source" will show all Bovine
Salmonella isolates in blue, and all Human Salmonella isolates in red. 

### Viewing isolate name(s).
To view the name(s) of isolate(s) represented by points in the NMDS plot, left-click on the point of interest. The name(s)
of isolate(s) represented by the point, as well as their serotype(s) ("Serotype"), source(s) ("Source"), geographic location(s)
("Geographic Location"), location(s) on the plot ("NMDS1" and "NMDS2" for the X and Y axes, respectively), and distance(s)
from the user-selected coordinates ("dist_", in pixels) will appear in the "Selected Isolate(s)" section of the app.

### Zooming in and out of the plot.
To zoom in on any portion of a plot, left-click and drag your mouse to select the portion of the plot on which you would like
to zoom. Double-click to zoom in on your selected region of the plot. To zoom back out, double-click on the plot again.
