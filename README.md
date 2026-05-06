# HoshiMap Project
This project aims to highlight locations across Japan that are promissing for any stargazing activities, based on major criteria and thresholds set by the user. Everything is written using R.
This tool helps anyone find prime locations in a prefecture depending on various criteria, and visualize and download the associated map.

The repository is composed of :
  One Quarto file being the project itself (HoshiMapMainProject), which contains all the explanatory information and all the code blocks except for the initial preprocessing code.
  Two Quarto files of the final HoshiMap tool, the two versions use either local or cloud data.
  Two Scripts of the code used during the preprocessing steps, one that was actually used and one cleaned afterwards, which is of course functional.

Informations about the project itself (data origin, method followed, advised thresholds) are detailed in the HoshiMapMainProject quarto.


List of raster values associated with land use coverages (JAXA Simplified Classification) :

Raster Value | Land Use Name
1. Water
2. Built-up
3. Paddy field
4. Cropland
5. Grassland
6. Deciduous broadleaf forest
7. Deciduous needleleaf forest	
8. Evergreen broadleaf forest	
9. Evergreen needleleaf forest	
10. Bare soil	
11. Bamboo forest
12. Wetland
13. Solar panel
14. Greenhouse

In the base application, values 1, 2, 6, 7, 8, 9 and 11 were removed.
