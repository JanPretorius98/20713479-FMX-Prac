# 20713479-FMX-Prac
Repository for Financial Econometrics 871 Practical Test.

---
###  Installation

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

### Project Organization

#### `DATA`:
Contains all data files for the practical.

#### `UTILITIES`:
This folder houses external scripts that enhance functionality.
  - `aesthetics.R`: Provides a standardised plotting theme.
  - `libraries.R`: Loads all necessary libraries and packages for the project via pacman.

#### `WRITE-UPS`:
Stores all write-ups and documentation for questions.
  - `Basic-Question-Template`: A folder housing the template for basic questions.
  - `Presentation-Question-Template`: A folder housing template for presentations.

--- 

