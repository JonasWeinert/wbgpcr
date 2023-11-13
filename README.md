# Shiny Application: Sahel Graph Generator

## Overview

The Sahel Graph Generator is a Shiny application designed to visualize regression outputs across different treatment groups in a clear and interactive manner. It is particularly tailored for handling datasets in .dta format.

## Installation and Setup
- Install R and RStudio: Ensure that R and RStudio are installed on your system.
- Clone Repository: Clone the repository containing the Sahel Graph Generator code.
- Install Required Packages: The application requires several R packages. The script contains the following passage for installing them:

```
packages <- c("shiny", "bslib", "shinyWidgets", "ggplot2", "dplyr", "tidyr", "haven", "gridExtra")

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}
```

## Features

- Data Input: The application supports uploading multiple .dta datasets.
- Customizable Plot Titles and Labels: Users can specify plot titles, axis labels, and apply custom transformations to data variables.
- Dynamic UI: The UI adapts to the uploaded data, allowing for interactive selection of variables and treatment groups.
- Graphical Output: Generates bar plots with error bars, significance stars, and percentage differences.
- Downloadable Plots: Plots can be downloaded as PNG files.

## Usage

- Upload Data: Use the file input to upload .dta files.
- Configure Plot Settings: Customize the plot title, axis labels, and choose variables and treatment groups.
- Generate Plot: Click the "Generate Plot" button to visualize the data.
- Download Plot: Use the "Save as PNG" button to download the plot.


## Custom Functions

Significance Stars: A function is included to determine significance stars based on p-values.
Gather Group Data: Custom function to structure data for each group.
Custom Label Wrapping: Facilitates label formatting in the plot.
UI Customization

The UI is built using fluidPage and employs the bs_theme for styling. It has a sidebar for inputs and a main panel for displaying outputs. Custom CSS and JavaScript are included for additional UI elements like a sticky sidebar, back-to-top button, and dynamic tabs.

### Server Logic

The server part of the application handles data processing, including reading data files, merging datasets, calculating averages, confidence intervals, significance stars, and rendering the plot. It also includes dynamic UI rendering based on the uploaded data.

## Running the App

To run the app, execute shinyApp(ui, server) in RStudio after ensuring all dependencies are installed and loaded.
