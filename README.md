# Financial Econometrics 871 Practical Test Repository

This repository contains the materials for the Financial Econometrics 871 practical test, including datasets, utility scripts for analysis, and documentation templates for questions and presentations.

---

## Table of Contents
1. [Installation](#installation)
2. [Project Organization](#project-organization)
3. [Question 1](#q1)
4. [Question 2](#q2)
5. [Documentation](#documentation)
6. [Authors](#authors-and-acknowledgment)


---
##  Installation

The project makes use of the following packages, that can be loaded with the external script `libraries.R` (see `UTILITIES` below):
- dplyr
- ggplot2
- tidyverse
- stringr
- tidytext
- readxl

These libraries are loaded using the pacman package, which offers a more convenient way to load R packages, installing them if necessary. Ensure that pacman is installed on your machine by running the following code in R:

```r
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

```

---

## Project Organization

### `DATA`:
Contains all data files for the practical.

### `UTILITIES`:
This folder houses external scripts that enhance functionality.
  - `aesthetics.R`: Provides a standardised plotting theme.
  - `libraries.R`: Loads all necessary libraries and packages for the project via pacman.

### `WRITE-UPS`:
Stores all write-ups and documentation for questions.
  - `Basic-Question-Template`: A folder housing the template for basic questions.
  - `Presentation-Question-Template`: A folder housing template for presentations.
  
### `LICENSE`:
This project is licensed under the MIT License, which is a highly permissive and flexible open-source license. Key points include:
  - Freedom to Use: Allows commercial use, modification, distribution, private use, and sublicensing.
  - Minimal Restrictions: Requires only the inclusion of the original license and copyright notice with any substantial portions of the software.
  - No Liability: The software is provided "as is", without warranty of any kind.

--- 

## Documentation

Links to further documentation, if available.

---

## Author

- Jan-Hendrik Pretorius, Stellenbosch University
