Post-Classification Change Comparison
================
Evelyn Gorey and Jerónimo Rodriguez
3/13/2022

## Purpose

The purpose of this script is to compare 2007 and 2020 classified maps
within the Magdalena River Valley study area in northern Colombia. The
goal is to isolate natural dense forest in the 2007 map and compare it
to the land cover classes detected in 2020, particularly mature oil
palm, in order to quantify deforestation attributed to oil palm
expansion.

This methodology was developed by Evelyn Gorey and further refined by
[Jerónimo
Rodriguez](https://liberalarts.temple.edu/content/jeronimo-rodriguez),
PhD student at Temple University.

## Set up

``` r
library(raster)
```

    ## Loading required package: sp

``` r
library(rgdal)
```

    ## rgdal: version: 1.5-23, (SVN revision 1121)
    ## Geospatial Data Abstraction Library extensions to R successfully loaded
    ## Loaded GDAL runtime: GDAL 3.2.1, released 2020/12/29
    ## Path to GDAL shared files: C:/Users/tug97794/Documents/R/win-library/4.1/rgdal/gdal
    ## GDAL binary built with GEOS: TRUE 
    ## Loaded PROJ runtime: Rel. 7.2.1, January 1st, 2021, [PJ_VERSION: 721]
    ## Path to PROJ shared files: C:/Users/tug97794/Documents/R/win-library/4.1/rgdal/proj
    ## PROJ CDN enabled: FALSE
    ## Linking to sp version:1.4-5
    ## To mute warnings of possible GDAL/OSR exportToProj4() degradation,
    ## use options("rgdal_show_exportToProj4_warnings"="none") before loading rgdal.
    ## Overwritten PROJ_LIB was C:/Users/tug97794/Documents/R/win-library/4.1/rgdal/proj

``` r
library(parallel)
library(sf)
```

    ## Linking to GEOS 3.9.1, GDAL 3.2.1, PROJ 7.2.1

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.2     v dplyr   1.0.7
    ## v tidyr   1.1.3     v stringr 1.4.0
    ## v readr   1.4.0     v forcats 0.5.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x tidyr::extract() masks raster::extract()
    ## x dplyr::filter()  masks stats::filter()
    ## x dplyr::lag()     masks stats::lag()
    ## x dplyr::select()  masks raster::select()

``` r
library(purrr)
library(furrr)
```

    ## Loading required package: future

    ## 
    ## Attaching package: 'future'

    ## The following object is masked from 'package:raster':
    ## 
    ##     values

## Prepare the classified maps

Create a binary mask to ensure the same extents for both classified
maps.

``` r
m <- c(-Inf, 0, NA, 0.1, Inf, 1)
m <- matrix(m, ncol=3, byrow = TRUE)
msk <- reclassify(class2007, m)
```

![](changeanalysis_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

Mask to ensure the same extents for both classified maps.

``` r
class2020 <- mask(class2020, msk)
```

## Reclassify to include only 4 classes

Instead of 11 classes representing specific land covers such as river,
grains, urban, flooded grasslands, young oil palm, etc., these classes
will be aggregated into 4 classes - unvegetated, dense natural forest,
mature oil palm (most accurate oil palm class), and other vegetated.

``` r
mclass <- c(-Inf, 1.1, 1, 
            1.12, 2.1, 2, 
            2.12, 3.1, 1, 
            3.12, 5.1, 4, 
            5.12, 7.1, NA, 
            7.12, 8.1, 3, 
            8.12, 10.1, 4, 
            10.12, 11.1, NA, 
            11.12, 12.1, 1, 
            12.12, 13.1, 4,
            13.12, 14.1, 1,
            14.12, 15.1, 4,
            15.12, Inf, 1) 
mclass <- matrix(mclass, ncol=3, byrow = TRUE)
class2007new <- reclassify(class2007, mclass)
class2020new <- reclassify(class2020, mclass)
```

*New 2007 classification:*

![](changeanalysis_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

*New 2020 classification:*

![](changeanalysis_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

## Isolating 2007 natural dense forest

Set a reclassification matrix and reclassify the 2007 classified map
(aggregated output) to include only class 2, or natural dense forest.

``` r
m2 <- c(-Inf, 1.9, NA, 2.1, Inf, NA)
m2 <- matrix(m2, ncol=3, byrow = TRUE)
class2007forest <- reclassify(class2007new, m2)
```

Apply the dense forest binary mask to the 2020 classified map
(aggregated output) to determine forest change.

``` r
class2020_fc <- mask(class2020new, class2007forest)
```

*Map output*, which shows the 2020 class of every pixel that was
classified as natural dense forest in 2007. 1 turned into unvegetated
(tilled soil, bare soil, urban, etc.), 2 remained natural dense forest,
3 turned into mature oil palm, and 4 turned into other vegetated
(grasslands potentially for pastures, grains, etc.).

![](changeanalysis_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

## Frequency data frame to quantify deforestation

Omit NAs and create a frequency dataframe to represent what 2007 natural
dense forest has turned into in 2020.

``` r
freq <- freq(class2020_fc, useNA='no')
freq <- as.data.frame(freq)
```

    ##   value   count
    ## 1     1  287983
    ## 2     2 6841764
    ## 3     3  294426
    ## 4     4 2127342

Pixel count can be measured in hectares.

``` r
freq <- freq %>%
  add_column(hectares = (30*30*.$count)/10000)
```

    ##   value   count  hectares
    ## 1     1  287983  25918.47
    ## 2     2 6841764 615758.76
    ## 3     3  294426  26498.34
    ## 4     4 2127342 191460.78

## Plot the forest change with ggplot2

Give class names for a more attractive and comprehensible graph.

``` r
class_names <- c('Unvegetated', 'Old growth forest', 'Mature oil palm', 'Vegetated')
class_names <- as.data.frame(class_names)

freq <- cbind(class_names, freq)
names(freq) <- c('Class', 'Value', 'Pixel_count', 'Hectares')
```

Simple plot to show comparison results.

``` r
p <- ggplot(freq, aes(x=Class, y=Hectares, color=Class, fill=Class))+
      geom_bar(stat='identity')+
      theme(axis.text.x = element_text(size = 10, angle = 90))

p + scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
```

![](changeanalysis_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

Only about 3% of the natural dense forest change between 2007 and 2020
is attributed to oil palm according to this analysis. The majority of
2007 forest was converted into other vegetation (specifically unflooded
grassland, which usually indicates the creation of pastures for
ranching).

Other regional and global forest products will be added into this
analysis to compare how accurately other products classify forest and
therefore deforestation in this study area.
