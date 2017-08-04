# CytoClusteR #

CytoClusteR employs hierarchical clustering to identify cell population in SPADE data and facilitate comparisons among multiple groups of samples.

### Setup ###

#### Requirements ####

Install R 3.4 from https://cran.r-project.org/bin/

Within R, install and load shiny package by running the following commands:
> install.packages("shiny")  
> library(shiny)

You are all set. To run CytoClusteR run within R:
> runGitHub("cytocluster", "kordastilab")

Please note that when you run CytoClusteR for the first time, all additional dependencies will be installed automatically.
This may take a couple of minutes. Keep an eye on R's console in case any dependencies need to be installed from source.

Some users reported rJava-related errors while running CytoclusteR in MacOS. If you experience any such error during setup please update MacOS, R and Rstudio (if applicable) to their latest versions and then follow [`these steps`](https://github.com/kordastilab/cytocluster/blob/master/docs/rJava.md) to resolve the error.

### Usage ###

#### Data loading ####

Below you can find a very basic description of CytoClusteR's GUI. A detailed step-by-step guide can be found [`here`](https://github.com/kordastilab/cytocluster/blob/master/docs/step_by_step_guide.md).

* **Cytobank login**. Use this tab to download your data directly from Cytobank. You can log in either using your username, password and your site or by using an authentication token and your site. Please not that when entering Cytobank's site, you only need to omit "cytobank.org". For example, if your site is **mrc.cytobank.org** you only need to enter **mrc** in the "site" field of the login page.

* **Manual input**. Use `Single-sample mode` or `Multiple-sample mode` to load SPADE data directly without logging in to Cytobank. In each tab you can select among multiple options while importing your input:
    1. **Input file:** Your SPADE table.
    2. **Marker cleaning file:** File to clean raw column names as exported from SPADE. See [`example_data/needed_columns.xlsx`](https://github.com/kordastilab/cytocluster/blob/master/example_data/needed_columns.xlsx).
    3. **Filter by cell count:** Set the minimum `cell count` for SPADE nodes to be included in the analysis.
    4. **Filter by percenttotal:** Set the minimum `percent total` for SPADE nodes to be included in the analysis.
    5. **Data transformation:** Set the transformation method. For now only `arcsinh` is available. Default co-factor is 5 - remember to change it if different value was used during your SPADE analysis.

* In `Multiple-sample mode` you need to define the directory with the SPADE files of each group you want to import.

* As soon as you press `Select markers` button, you data will be imported and you will be presented with the option to select the markers for clustering.

* In `Multiple-sample mode`, if you supply `Output.Directory`, you can press the `Clean and Save` button to clean and save your data in your desired directory.

#### Define sample tags ####

After importing your data using the `Cytobank log in` option you can switch to `Sample tags` tab to group your samples and push these tags to Cytobank

#### Clustering ####

1. **Main group:**. Heatmap of the main group. You can select which group to plot using the dropdown menu (visible only when using Multiple-sample mode).
2. **Forced group:**. This tab is useful for comparison of SPADE nodes among different groups of samples. When you press `Force heatmap` button the clustering of SPADE nodes in the **Main group** is applied to the group selected. This allows for direct comparisons of SPADE nodes and cell subpopulations among groups.
3. **Overlay groups:** Comparison of the two groups in the `Forced group` tab. Third heatmap illustrates the difference in expression between the two groups. Expression values of the `Forced group` are subtracted from the `Main group`.

Visualization parameters can be altered in the `Heatmap configuration` box. Using the `Heatmap configuration` box users can also select additional markers to annotate or sort the heatmap. Download the heatmap or the data using the `Download plot` or `Download data` buttons.

Hierarchical clustering and visualisation is performed using the default settings of [`ComplexHeatmap R package`](https://bioconductor.org/packages/release/bioc/html/ComplexHeatmap.html).

#### Post-processing ####

In this tab you can define up to three markers to divide the SPADE nodes in `High`, `Medium` and `Low` expression for each marker. A z-score is calculated for each node and marker. When the z-score >= 1 the expression level is set to High(H), when the z-score > -1 and < 1 the expression level is set to Medium (M) and when the z-score <= -1 the expression level is set to Low(L) for each marker selected.

#### Node identification ####

In this tab you can define cell populations and push SPADE bubbles to Cytobank using the `Push node groups` button. There is a known bug in this functionality which will be resolved shortly.

#### Data storage ####

Please note that if you use the `Cytobank log in` utility, CytoClusteR will create a directory (~/cytocluster_temp) to download your data. This directory is overwritten every time you run CytoClusteR. If you want to store your raw data locally, make sure you copy them to another directory.

#### Example data ####

See [`example_data`](https://github.com/kordastilab/cytocluster/tree/master/example_data) for a example input data.

### Contributors ###

CytoClusteR has been designed by Mr. Thanos Mourikis and Dr. Shahram Kordasti.

Main developer: Thanos Mourikis

Contributions are always welcome!

### Support ###

Please contact Dr. Shahram Kordasti, shahram@bioconsultpro.co.uk or Thanos Mourikis, thmourikis@gmail.com.

### License ###

GPLv3
