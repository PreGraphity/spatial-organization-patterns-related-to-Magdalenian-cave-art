# spatial-organization-patterns-related-to-magdalenian-cave-art
This repository contains the codes from DOI.

First, in the "0 DATA" folder, you can find an .xlsx file containing the iconographic data for the graphic units (GU) analysed in this study. This table summarises the database created for analysis. The photogrammetric files for each GU could not be included in this repository due to the large volume of data. The models of the GUs are essential for the "Script.py" code in the next folder ("1 GIS") to function properly. These photogrammetric models were made in standard files (.obj for the 3D model, .tif for the texture, and .mtl for the mapping), and were archived after being georeferenced using common markers with the cave's point cloud. Later, the 3D model (.obj) for each GU was converted to a .wrl file, so that they could be processed through ArcGIS.


Next, in the "1 GIS" folder, we have stored the Python code for use in ArcGIS. For installation instructions, you can refer to the supplementary materials from the article with DOI https://doi.org/10.1007/s10816-022-09552-y, available through the following link: https://static-content.springer.com/esm/art%3A10.1007%2Fs10816-022-09552-y/MediaObjects/10816_2022_9552_MOESM1_ESM.pdf

In any case, We need the following files (With the name that we indicate): 
# CeilingsMax.wrl (the entire 3D of the cave, modified to show its paleo-state), 
# CeilingsMin.wrl (3D of the "internal ceilings" in the cave, the obstacles between 
# the ground and the highest Z), GroundOK.wrl (the ground level, presumably utilised 
# by the societies in the past), GU.wrl (3D of the analysed depiction), 
# Access.shp (the entrance used in the past).
# PUT THE FILES IN THE SAME FOLDER, "C/:Paleospeleology". (you need to create it)

The "0 Output" folder, included within the "1 GIS" folder, contains a table that summarises the statistical data obtained by applying the code from this repository to each of the GUs analysed in this research.

Unfortunately, it was not possible to store the generated data for each of the GUs because of its size (each GU contains an average size of 1.5 Gb). That is, for each of the GUs, we would have:

# a file named GU.wrl that contains the photogrammetric model of the figure, cropped as described in Figure 2 of the article.
# CeilingsMax.wrl (the entire 3D of the cave, modified to show its paleo-state), CeilingsMin.wrl (3D of the "internal ceilings" in the cave, the obstacles between  the ground and the highest Z)
# GroundOK.wrl (the ground level, presumably utilised by the societies in the past)
# Access.shp (the entrance used in the past)

As well as three folders named as:
# "Accessibility" with .shp files like the "EstimatedPositionArtist.shp" (it gives the distance to the ground of the GU, or the estimated posture of the artist), the Least Cost Path "LCP.shp" (it gives the "Difficulty value" of the accessibility of this GU, as well as the topographic distance of the LCP), or "time.shp" (it gives the Estimated Time of Arrival in minutes). It also contains other files, as the .tif file "GreaterDifficulties.tif", a Kernel concentration HeatMap showing the biggest concentration of the difficulties over the LCP.
# "Metadata": It contains several files created in the analyses, like de Lines of Sight (LSSelect.shp), or the Buffer (Buffer.shp).
# "Visibility": it contains several files as the files "VisibilityLyingDown.tif", "VisibilityStanding.tif" or "VisibilityStooping.tif", which counts the estimated viewers in each position.
# "Rock-Art": these files are related with the photogrammetric files of the GU, like its centroid (GUCentroid.shp).

Finally, we have a folder called "2 Statistics" that contains an .xlsx table that combines the iconographic information of each figure extracted from the database summarised in the "0 Data" folder, as well as the results of the spatial analyses explained in (but unfortunately not yet stored in) the "1 GIS" folder.

We've also stored the code in R, used in multivariate analyses via the RStudio software (as well as the installed packages). This file is executable using the table contained in this folder.

The scripts can be edited by other researchers, according to their interests and problems. The results originated using this script can also be shared in this repository.
