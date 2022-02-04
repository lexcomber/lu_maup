# The importance of scale and the MAUP for robust Ecosystem Service evaluations and landscape decisions

Alexis Comber<sup>1*</sup> and Paul Harris<sup>2</sup>

<sup>1</sup> School of Geography, University of Leeds, Leeds, UK.\
<sup>2</sup> Sustainable Agriculture Sciences North Wyke, Rothamsted Research, Okehampton, UK.

<sup>*</sup> contact author: a.comber@leeds.ac.uk

## Abstract

Spatial data are used in many scientific domains including Ecosystem Services (ES) and natural capital (NC) analyses, whose results are used to inform planning and policy. However, data spatial scale (or support) has a fundamental impact on outputs and thus process understanding, inference, and policy advice. The Modifiable Areal Unit Problem (MAUP) describes the effects of scale on analyses of spatial data and outputs, but has been ignored in much environmental research, including evaluations of agriculture, ES and NC. This paper illustrates the MAUP through an ES optimisation problem. The results show that MAUP effects are unpredictable and non-linear, with discontinuities specific to the spatial properties of the case study. Four key recommendations are: 1) The MAUP should always be tested for in ES evaluations. This is commonly done in socio-economic analyses. 2) Spatial aggregation scales should be matched to the granularity of the processes being evaluated  by identifying the scale at which processes are considered to be stable (stationary) with respect to variances, covariances and other moments. 3) Aggregation scales should be evaluated along with the scale of decision making (e.g. agricultural field, farm holding, catchment).  4) Researchers in ES and related disciplines should up-skill themselves in spatial analysis and core paradigms related to scale, to overcome the scale blindness commonly found in much research.

**Keywords**: spatial support; land use; genetic algorithm

The paper has been submitted to Land


## Code
To run the analysis in this paper you should download the the R script `lu_paper.R` and install the packages. Package and other info is below. The data files and supporting scripts will need will need to be locally available . The code recreates the results as the same sequence in the paper. 

If you have any problems with data / code / versions etc please contact Lex Comber at the email above.

```{r}
sessionInfo()
R version 4.0.4 (2021-02-15)
Platform: x86_64-apple-darwin17.0 (64-bit)
Running under: macOS Big Sur 10.16

Matrix products: default
LAPACK: /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRlapack.dylib

locale:
[1] en_GB.UTF-8/en_GB.UTF-8/en_GB.UTF-8/C/en_GB.UTF-8/en_GB.UTF-8

attached base packages:
[1] grid      stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] raster_3.5-2       quadtree_0.1.8     lemon_0.4.5        gtable_0.3.0      
 [5] units_0.7-1        gridExtra_2.3      cowplot_1.1.1      genalg_0.2.0      
 [9] scales_1.1.1       GISTools_0.7-4     rgeos_0.5-5        MASS_7.3-53.1     
[13] RColorBrewer_1.1-2 maptools_1.1-2     sp_1.4-6           tmap_3.3-1        
[17] sf_0.9-8           forcats_0.5.1      stringr_1.4.0      dplyr_1.0.5       
[21] purrr_0.3.4        readr_1.4.0        tidyr_1.1.3        tibble_3.1.1      
[25] ggplot2_3.3.5      tidyverse_1.3.1   

loaded via a namespace (and not attached):
 [1] fs_1.5.0           lubridate_1.7.10   httr_1.4.2         tools_4.0.4       
 [5] backports_1.2.1    rgdal_1.5-23       utf8_1.2.1         R6_2.5.0          
 [9] KernSmooth_2.23-18 DBI_1.1.1          colorspace_2.0-0   withr_2.4.2       
[13] tidyselect_1.1.0   leaflet_2.0.4.1    compiler_4.0.4     leafem_0.1.3      
[17] cli_2.4.0          rvest_1.0.0        xml2_1.3.2         labeling_0.4.2    
[21] classInt_0.4-3     proxy_0.4-26       digest_0.6.27      foreign_0.8-81    
[25] base64enc_0.1-3    dichromat_2.0-0    pkgconfig_2.0.3    htmltools_0.5.1.1 
[29] highr_0.9          dbplyr_2.1.1       htmlwidgets_1.5.3  rlang_0.4.10      
[33] readxl_1.3.1       rstudioapi_0.13    farver_2.1.0       generics_0.1.0    
[37] jsonlite_1.7.2     crosstalk_1.1.1    magrittr_2.0.1     Rcpp_1.0.7        
[41] munsell_0.5.0      fansi_0.4.2        abind_1.4-5        lifecycle_1.0.0   
[45] terra_1.4-11       stringi_1.5.3      leafsync_0.1.0     plyr_1.8.6        
[49] tmaptools_3.1-1    parallel_4.0.4     crayon_1.4.1       lattice_0.20-41   
[53] stars_0.5-2        haven_2.4.0        hms_1.0.0          knitr_1.32        
[57] ps_1.6.0           pillar_1.6.0       codetools_0.2-18   reprex_2.0.0      
[61] XML_3.99-0.6       glue_1.4.2         modelr_0.1.8       png_0.1-7         
[65] vctrs_0.3.7        cellranger_1.1.0   assertthat_0.2.1   xfun_0.22         
[69] lwgeom_0.2-6       broom_0.7.6        e1071_1.7-9        class_7.3-18      
[73] viridisLite_0.4.0  ellipsis_0.3.1    
