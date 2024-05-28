---
title: ""
output: 
  html_document:
    template: assets/template.html
    css: assets/style.css
    keep_md: true
editor_options: 
  markdown: 
    wrap: sentence
---


<br>
This repository contains data, code, and output for a project entitled "Childhood adversity is not associated with lowered inhibition, but lower perceptual processing: A Drift Diffusion Model analysis". The manuscript is currently under review at *Cognitive Development*.

Do you want to download or clone the materials for this project? Go to [https://github.com/stefanvermeent/attention_project](https://github.com/stefanvermeent/attention_project).

## Directory Structure {#structure}

The names of each folder are intended to be self-explanatory.
There are eight top-level folders to organize the inputs and outputs of this project:

1.  [`Manuscript`](https://stefanvermeent.github.io/attention_project/manuscript/README.html): The Registered Report written in Quarto.
2.  [`Supplement`](https://stefanvermeent.github.io/attention_project/supplement/README.html): a supplemental text (to be submitted with the manuscript) documenting all secondary analyses in detail.
3.  [`Preregistrations`](https://stefanvermeent.github.io/attention_project/preregistrations/README.html): Contains a sub-folder for each study, which in turn contain all scripts, materials, analysis objects, and preregistration documents for that study (see below for more information).
4.  [`Data`](https://stefanvermeent.github.io/attention_project/data/README.html): Contains a sub-folder for each study, which in turn contain all raw and processed data files needed to reproduce the results.

Click on each of the folders to get more details.

## Overview of project milestones

Below is an overview of all the project milestones, such as first-time data access, submissions, and revisions.
Data access events were automatically captured using custom code, which over the course of this project was collected in the R package `projectlog` [https://stefanvermeent.github.io/projectlog/](https://stefanvermeent.github.io/projectlog/).
For more information about how tracking worked, Go to the [Open science workflow tab](https://stefanvermeent.github.io/abcd_ddm/opensci_workflow/README.html).

- **[2023-05-10 15:48:49](https://github.com/stefanvermeent/attention_project/tree/b773bfde511683fe530855fa63bdcdcbb2fa1f81): final timestamped version of preregistration for study 2**
    - **Milestone:** Study2 Preregistration
    - **Data MD5 hash**: 
    - [Link to code snippet](https://github.com/StefanVermeent/liss_wm_profiles_2023/blob/main/.projectlog/b773bfde511683fe530855fa63bdcdcbb2fa1f81.R)
    

- **[2022-03-23 09:17:06](https://github.com/stefanvermeent/attention_project/tree/219a2d05feefb0696a070c20f454e7e9d0635dd0): Amendment to Preregistration of Study 1**
    - **Milestone:** Study1 Preregistration_amendment
    - **Data MD5 hash**: 
    - [Link to code snippet](https://github.com/StefanVermeent/liss_wm_profiles_2023/blob/main/.projectlog/219a2d05feefb0696a070c20f454e7e9d0635dd0.R)
    

- **[2022-03-18 13:44:18](https://github.com/stefanvermeent/attention_project/tree/85115b0afefe571f5d6ae22c2b03125e95b28d03): final rendered versions of preregistration**
    - **Milestone:** Study1 Preregistration
    - **Data MD5 hash**: 
    - [Link to code snippet](https://github.com/StefanVermeent/liss_wm_profiles_2023/blob/main/.projectlog/85115b0afefe571f5d6ae22c2b03125e95b28d03.R)
    

- **[2021-11-08 10:12:19](https://github.com/stefanvermeent/attention_project/tree/b4d01a5837c1dbb04f939960ba291fa6ffad1cdc): Preregistration of Pilot Study**
    - **Milestone:** Preregistration
    - **Data MD5 hash**: 
    - [Link to code snippet](https://github.com/StefanVermeent/liss_wm_profiles_2023/blob/main/.projectlog/b4d01a5837c1dbb04f939960ba291fa6ffad1cdc.R)
    

## How to reproduce this repository {#reproduce}

All scripts to reproduce our analyses can be found under `preregistrations`, grouped per study.
Each study sub-folder contains an `analysis objects` folder, which contains both the raw and cleaned data files.
It is therefore possible to immediately run the primary analyses scripts (e.g., `1_primary_analyses.R`) without preprocessing the data first.
However, if you'd like to go through the analysis pipeline start-to-finish, you should take the following steps:

### 1. Loading Dependencies

1.  [`dependencies.R`](https://github.com/stefanvermeent/attention_project/blob/main/scripts/dependencies): Loads (and installs if necessary) all the necessary R packages as they were on 2024-01-01 using the [`Groundhog`](https://groundhogr.com/) package. 
This ensures that scripts will continue functioning as intended even after packages have been updated. 
`Groundhog` is a more lightweight, easy-to-use alternative to package dependency managers such as `renv`.

### 2. Data Preparation

Each study `script` folder contains a subfolder names `0_data_prep`. 
This subfolder contains all the scripts necessary to get from the (minimally) preprocessed data to the cleaned data.
The scripts are numbered, and should be run in that order.
Importantly, the `1_create_scales.R` script will not work, as it requires access to the raw data.
We do not share these raw data files as they contain variables that are not necessary for the analyses, as well as identifying information (e.g., Prolific IDs).
So, data preprocessing should start at `2_clean_self_report.R`. 

**A note on running DDM models**. Please note that rerunning the DDM models takes a substantial amount of time, in particular the Hierarchical Bayesian DDM models and the SSP models.
The SSP model in particular, takes around 20-30 minutes per participant to complete.
Even in parallel, this will take multiple hours.
Thus, in most cases, it might be better to use the DDM result objects in each study's `analysis_objects` folder as a starting point.

Direct links to:

1.  [Pilot preprocessing scripts](https://github.com/stefanvermeent/attention_project/blob/main/1_pilot/scripts/0_data_prep).
2.  [Study 1 preprocessing scripts](https://github.com/stefanvermeent/attention_project/blob/main/2_study1/scripts/0_data_prep).
3.  [Study 2 preprocessing scripts](https://github.com/stefanvermeent/attention_project/blob/main/3_study2/scripts/0_data_prep).



### 3. Primary Analyses

The script for the primary analyses for each study can be found under `preregistrations/<study>/scripts`. 
This script reads in the cleaned data, runs all the analyses, and stores the main results as an .RData file under `preregistrations/<study>/analysis_objects`.

### 4. Manuscript

1.  [`manuscript/scripts/staging.R`](https://github.com/stefanvermeent/attention_project/blob/main/manuscript/scripts): This folder contains four scripts: one for each separate study, and one for the pooled analyses. These scripts load the .RData files stored in the `analysis_objects` folder, and states the statistics that are reported in the manuscript (e.g., creating Figures and Tables, and formatting descriptive statistics). The results per study are stored in a single .RData file under [`manuscript`](https://github.com/stefanvermeent/attention_project/blob/main/manuscript) (e.g., `study1_staged_results.RData`).
2.  [`manuscript/manuscript.qmd`](https://github.com/stefanvermeent/abcd_ddm/blob/main/manuscript/manuscript.qmd): loads the staged results and knits together the manuscript and all statistics, tables, and figures computed in previous scripts.

### 5. Supplement

1.  [`supplement/supplements.qmd`](https://github.com/stefanvermeent/attention_project/blob/main/supplement/supplements.qmd): loads the staged results list and knits together the written supplement and all tables and figures computed by the staging scripts.

### 6. Issues with dependencies? {#using_docker}

Even if you use the `dependencies.R` file to reinstate the correct versions of packages, the code might break over time as a result of changing operation systems or incompatibilities with future R versions.
To solve, this, we supply a Docker image that instantiates an Rstudio cloud environment with all the required dependencies.

In short, Docker provides a 'container'---similar to a virtual machine---with all the project dependencies (R packages, software, operating systems) pre-loaded in their (historic) versions as they were used when the project was created.
Thus, Docker ensures that scripts will still function as intended even into the future.
**Note: It is not necessary to use Docker to reproduce our results**.
If you're cloning the repository within a couple of years of publication and installing the proper dependencies (see step 1 below) you should be able to run all scripts without issues.
However, some of our scripts might stop to function properly over time as other dependencies get updated and/or deprecated (e.g., future versions of RStudio)
If that case, Docker is the solution.

Below is the exact order these scripts should be ran (irrespective of whether you are working with or without Docker):

If you want to create a Docker container in which to run the scripts, you will have to follow the following steps:

1.  Download and install docker at <https://docs.docker.com/get-docker/>.
2.  If on Windows, open the PowerShell. If on Mac, open the terminal through 'Applications \> Utilities \> Terminal'.
3.  On the command line, type: `docker run --rm -d -e PASSWORD=my_password -p 8787:8787 stefanvermeent/attention_project`
4.  Open a browser window and enter: `localhost:8787` as the URL.
5.  You will be redirected to an Rstudio cloud login page. As the username, type *rstudio*. As the password, type *my_password*, unless you changed it under step 3.
6.  You should now see an RStudio environment with the required dependencies pre-installed and loaded.

### Contact

For questions or comments, feel free to contact me at p.c.s.vermeent@gmail.com.

