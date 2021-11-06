Haodong Chen 2021 submission 3
================
Haodong Chen
10/27/2021

# Goal

This document shows how **individual bottle** bacterial abundance data
from 2021 remineralization experiments were processed, QC’d and
analyzed.

Load packages that we’ll need to analyze our data.

``` r
library(tidyverse)
library(readxl)
library(lubridate)
```

# Import Data

``` r
excel_sheets("~/Desktop/144l_students_2021/Input_Data/week4/144L_2021_BactAbund.xlsx")
```

    ## [1] "Metadata"  "FCM_Data"  "DAPI_Data" "TOC_Data"

``` r
metadata <- read_excel("~/Desktop/144l_students_2021/Input_Data/week4/144L_2021_BactAbund.xlsx", sheet = "Metadata")

glimpse(metadata)
```

    ## Rows: 80
    ## Columns: 16
    ## $ Experiment           <chr> "144L_2021", "144L_2021", "144L_2021", "144L_202…
    ## $ Location             <chr> "Goleta Pier", "Goleta Pier", "Goleta Pier", "Go…
    ## $ Temperature          <dbl> 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, …
    ## $ Depth                <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
    ## $ Bottle               <chr> "A", "A", "A", "A", "A", "A", "A", "A", "A", "A"…
    ## $ Timepoint            <dbl> 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, …
    ## $ Treatment            <chr> "Control", "Control", "Control", "Control", "Con…
    ## $ Target_DOC_Amendment <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ Inoculum_L           <dbl> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, …
    ## $ Media_L              <dbl> 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, …
    ## $ Datetime             <chr> "2021-10-04T16:00", "2021-10-05T08:00", "2021-10…
    ## $ TOC_Sample           <lgl> TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, F…
    ## $ Cell_Sample          <lgl> TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, …
    ## $ DAPI_Sample          <lgl> TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, F…
    ## $ DNA_Sample           <lgl> TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, F…
    ## $ Nutrient_Sample      <lgl> TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, …

``` r
dapi_data <- read_excel("~/Desktop/144l_students_2021/Input_Data/week4/144L_2021_BactAbund.xlsx", sheet = "DAPI_Data")
glimpse(dapi_data)
```

    ## Rows: 12
    ## Columns: 6
    ## $ Treatment                <chr> "Control", "Control", "Control", "Kelp Exuda…
    ## $ Timepoint                <dbl> 0, 4, 8, 0, 4, 8, 0, 4, 8, 0, 4, 8
    ## $ Cells_mL                 <dbl> 660667.0, 919405.6, 1133869.7, 663088.1, 104…
    ## $ Cells_mL_Stdev           <dbl> 73217.76, 363326.27, 99930.05, 113546.27, 18…
    ## $ Mean_Biovolume_um3_cell  <dbl> 0.04556209, 0.05080353, 0.04093212, 0.038714…
    ## $ Biovolume_Stdev_um3_cell <dbl> 0.006054805, 0.011000369, 0.004684495, 0.005…

``` r
dapi_metadata <- metadata %>%
  select(-Bottle) %>%
  unique()
glimpse(dapi_metadata)
```

    ## Rows: 40
    ## Columns: 15
    ## $ Experiment           <chr> "144L_2021", "144L_2021", "144L_2021", "144L_202…
    ## $ Location             <chr> "Goleta Pier", "Goleta Pier", "Goleta Pier", "Go…
    ## $ Temperature          <dbl> 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, …
    ## $ Depth                <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
    ## $ Timepoint            <dbl> 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, …
    ## $ Treatment            <chr> "Control", "Control", "Control", "Control", "Con…
    ## $ Target_DOC_Amendment <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 10, 10, 10, 10…
    ## $ Inoculum_L           <dbl> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, …
    ## $ Media_L              <dbl> 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, …
    ## $ Datetime             <chr> "2021-10-04T16:00", "2021-10-05T08:00", "2021-10…
    ## $ TOC_Sample           <lgl> TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, F…
    ## $ Cell_Sample          <lgl> TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, …
    ## $ DAPI_Sample          <lgl> TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, F…
    ## $ DNA_Sample           <lgl> TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, F…
    ## $ Nutrient_Sample      <lgl> TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, …

``` r
joined <-  left_join(dapi_metadata, dapi_data) 
```

    ## Joining, by = c("Timepoint", "Treatment")

Complete: prepare data, plot growth curves for Cells\_L and Cell
Biovolume data AND identify exponential growth (same as previous
assignment now with the new data).

# Prepare Data

Convert the Date and Time column values from characters to dates, add
columns with time elapsed for each treatment, and convert to cells/L
because it will help us match up with the TOC data later. Also drop NA
values.

``` r
#insert your code here
DAPI_Data <- joined %>%
  mutate(Datetime = ymd_hm(Datetime), 
  Cells_L = as.numeric(Cells_mL) * 0.001,
  Cells_L_Stdev = as.numeric(Cells_mL_Stdev) * 0.001) %>%
  group_by(Treatment) %>%
  ungroup() %>%
  select(Experiment:Cells_L, Cells_L_Stdev, Treatment, Timepoint) %>%
  drop_na(Cells_L)
glimpse(DAPI_Data)
```

    ## Rows: 12
    ## Columns: 21
    ## $ Experiment               <chr> "144L_2021", "144L_2021", "144L_2021", "144L…
    ## $ Location                 <chr> "Goleta Pier", "Goleta Pier", "Goleta Pier",…
    ## $ Temperature              <dbl> 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, …
    ## $ Depth                    <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
    ## $ Timepoint                <dbl> 0, 4, 8, 0, 4, 8, 0, 4, 8, 0, 4, 8
    ## $ Treatment                <chr> "Control", "Control", "Control", "Kelp Exuda…
    ## $ Target_DOC_Amendment     <dbl> 0, 0, 0, 10, 10, 10, 10, 10, 10, 10, 10, 10
    ## $ Inoculum_L               <dbl> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2
    ## $ Media_L                  <dbl> 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5
    ## $ Datetime                 <dttm> 2021-10-04 16:00:00, 2021-10-06 20:00:00, 2…
    ## $ TOC_Sample               <lgl> TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TR…
    ## $ Cell_Sample              <lgl> TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TR…
    ## $ DAPI_Sample              <lgl> TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TR…
    ## $ DNA_Sample               <lgl> TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TR…
    ## $ Nutrient_Sample          <lgl> TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE…
    ## $ Cells_mL                 <dbl> 660667.0, 919405.6, 1133869.7, 663088.1, 104…
    ## $ Cells_mL_Stdev           <dbl> 73217.76, 363326.27, 99930.05, 113546.27, 18…
    ## $ Mean_Biovolume_um3_cell  <dbl> 0.04556209, 0.05080353, 0.04093212, 0.038714…
    ## $ Biovolume_Stdev_um3_cell <dbl> 0.006054805, 0.011000369, 0.004684495, 0.005…
    ## $ Cells_L                  <dbl> 660.6670, 919.4056, 1133.8697, 663.0881, 104…
    ## $ Cells_L_Stdev            <dbl> 73.21776, 363.32627, 99.93005, 113.54627, 18…

``` r
view(DAPI_Data)
```

# Plot Growth Curves

Plot growth curves for each treatment using DAPI cell abundance and
biovolume data.

## Cell Abundance Growth Curve

``` r
#insert your code here

custom.colors <- c("Control" = "#377EB8", "Ash Leachate" = "#4DAF4A", "Mud Leachate" = "#E41A1C", "Glucose_Nitrate_Phosphate" = "#FF7F00", "Kelp Exudate_Nitrate_Phosphate" = "#00FF00", "Kelp Exudate" = "#DDA0DD")
levels <- c("Control", "Ash Leachate", "Mud Leachate", "Glucose_NitrateExudate")
DAPI_Data %>%
  mutate(DAPI_Data = ifelse(Cells_L == T, "*", NA)) %>%
  ggplot(aes(x=Timepoint, y=Cells_L, group = interaction(Treatment))) +
  geom_line(aes(color = factor(Treatment, levels = levels)), size =1) +
  geom_point(aes(fill = factor(Treatment, levels = levels)), size = 3, color = "black", shape = 21) + 
  labs(x = "Timepoint", y = expression(paste("Cells, L"^-1)), fill = "") + 
  guides(color = "none") + 
  scale_color_manual(values = custom.colors) +
  scale_fill_manual(values = custom.colors) +
  #facet_grid(rows = "Treatment")
  theme_bw()
```

![](144l-Haodong-Chen-submission-3_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

Q: What differences between the treatments do you observe? Does this
make sense in the context of the oxygen drawdown data (pictured below)?

A: For Cell abundance, Glucose Nitrate Phosphate \> Kelp Exudate Nitrate
Phosphate \> Kelp Exudate \> Control, which means the more nutrients
added in the treatment, the greater bacteria growth efficiency would be.
All curve except the control one become smoother after timepoint 4. A
possible reason might be the nutrients added were running out. Also, as
the cell abundance increases, they would consume more oxygen. The graph
we get is mostly supported by the change in oxygen data. The only
difference is kelp DOM and N/P treatment and Kelp DOM treatment are
reversed. The two lines in oxygen are very close to eanch other, and
maybe some other factors leading to this situation.

Oxygen Drawdown:

![O2 drawdown](EEMB144_remin_autoBOD.png)

## Cell Biovolume Growth Curve

``` r
#insert your code here
custom.colors <- c("Control" = "#377EB8", "Ash Leachate" = "#4DAF4A", "Mud Leachate" = "#E41A1C", "Glucose_Nitrate_Phosphate" = "#FF7F00", "Kelp Exudate_Nitrate_Phosphate" = "#00FF00", "Kelp Exudate" = "#DDA0DD")
levels <- c("Control", "Ash Leachate", "Mud Leachate", "Glucose_Nitrate_Phosphate", "Kelp Exudate_Nitrate_Phosphate", "Kelp Exudate")
DAPI_Data %>%
  mutate(DAPI_Data = ifelse(Mean_Biovolume_um3_cell == T, "*", NA)) %>%
  ggplot(aes(x=Timepoint, y=Mean_Biovolume_um3_cell, group = interaction(Treatment))) +
  geom_line(aes(color = factor(Treatment, levels = levels)), size =1) +
  geom_point(aes(fill = factor(Treatment, levels = levels)), size = 3, color = "black", shape = 21) + 
  labs(x = "Timepoint", y = expression(paste("Biovolume, um"^3)), fill = "") + 
  guides(color = "none") + 
  scale_color_manual(values = custom.colors) +
  scale_fill_manual(values = custom.colors) +
  #facet_grid(rows = "Treatment")
  theme_bw()
```

![](144l-Haodong-Chen-submission-3_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

Q: What differences do you notice between the cell abundance data and
the cell biovolume data?

A: Biovolume decreased after timepoint 4 for each group. Also, Kelp
exudate nitrate phosphate can be higher in biovolume than glucose
nitrate phosphate at most of the time. The Kelp Exudate treatment’s
biocolume could even drop into a lower point than the control group at
timepoint 8.
