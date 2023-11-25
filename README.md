# Financial Econometrics 871 Practical Test Repository

This repository contains the materials for the Financial Econometrics 871 practical test, including datasets, utility scripts for analysis, and documentation templates for questions and presentations.

##  Note on question documentation:

Documentation for questions will follow this general format:
- Questions guiding analysis
- Code
  - Data Operations
  - Plotting Code
  - Any additional code
- Pitfalls and problems faced throughout analysis

---

## Table of Contents
1. [Installation](#installation)
2. [Project Organization](#project-organization)
3. [Question 1](#question-1)
4. [Question 2](#question-2)
5. [Question 3](#question-3)
6. [Question 4](#question-4)
7. [Question 5](#question-5)
8. [Question 6](#question-6)
9. [Utilities Functions](#utilities-functions)
10. [Documentation](#documentation)
11. [Authors](#authors)


---
##  Installation

The project makes use of the following packages, that can be loaded with the external script `libraries.R` (see `UTILITIES` below):
- dplyr
- ggplot2
- tidyverse
- stringr
- tidytext
- readxl
- xtable

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
  - `capping.R`: Provides the `Proportional_Cap_Foo` function
  - `missing-values.R`: Provides the `impute_missing_returns` function

### `WRITE-UPS`:
Stores all write-ups and documentation for questions.
  - `Basic-Question-Template`: A folder housing the template for basic questions.
  - `Presentation-Question-Template`: A folder housing template for presentations.
  - `Question-#`: Write-up and code for Questions
  
### `LICENSE`:
This project is licensed under the MIT License, which is a highly permissive and flexible open-source license.

---

##  Question 1

This question is stored in `WRITE-UPS`->`Question-1`. The following section explains step by step how the problem in question 1 was approached. This section also contains code chunks used in the creation of the write-up document.

### The folllowing questions guide the analysis:


### Code:


#### Data operations:


#### Plotting:



### Some pitfalls along the way:

---

##  Question 2

This question is stored in `WRITE-UPS`->`Question-2`. The following section explains step by step how the problem in question 1 was approached. This section also contains code chunks used in the creation of the write-up document.

### The folllowing questions guide the analysis:


### Code:


#### Data operations:


#### Plotting:



### Some pitfalls along the way:

---

##  Question 3

This question is stored in `WRITE-UPS`->`Question-3`. The following section explains step by step how the problem in question 1 was approached. This section also contains code chunks used in the creation of the write-up document.

### The folllowing questions guide the analysis:


### Code:


#### Data operations:


#### Plotting:



### Some pitfalls along the way:

---

##  Question 4

This question is stored in `WRITE-UPS`->`Question-4`. The following section explains step by step how the problem in question 1 was approached. This section also contains code chunks used in the creation of the write-up document.

### The folllowing questions guide the analysis:


### Code:


#### Data operations:


#### Plotting:



### Some pitfalls along the way:

---

##  Question 5

This question is stored in `WRITE-UPS`->`Question-5`. The following section explains step by step how the problem in question 1 was approached. This section also contains code chunks used in the creation of the write-up document.

### The folllowing questions guide the analysis:


### Code:


#### Data operations:


#### Plotting:



### Some pitfalls along the way:

---

##  Question 6

This question is stored in `WRITE-UPS`->`Question-6`. The following section explains step by step how the problem in question 1 was approached. This section also contains code chunks used in the creation of the write-up document.

### The folllowing questions guide the analysis:


### Code:


#### Data operations:


#### Plotting:



### Some pitfalls along the way:

---


##  Utilities Functions:

The following section shows the code used in the UTILITIES folder (in alphabetical order):

### `aesthetics.R`

```r
#   Goal: Creating themes and aesthetics for plots.

path <- "/Users/janhendrikpretorius/Library/CloudStorage/OneDrive-StellenboschUniversity/01-Masters-2023/02 Financial Econometrics/20713479-FMX-Prac/"
source(paste0(path, "UTILITIES/libraries.R")) #   Load libraries

# Define colour palette
palette <- c("#1F6F70", "#D98515", "#1E3364", "#75A21B", "#C93D44", "#38B0FF", "#8E22AC")

# Define plot theme
th <- theme(
    # Background and grid
    panel.background = element_blank(),
    plot.background = element_rect(fill = "white", color = "white"),
    panel.grid.major = element_line(color = "#565857", size = 0.1),
    panel.grid.minor = element_line(color = "#565857", size = 0.1),
    axis.line = element_line(size = 0.3, color = "black"),

    # Axis titles and labels
    axis.title.x = element_text(size = 12,family = "Palatino", vjust = 0.5, hjust = 0.5, face = "bold"),
    axis.title.y = element_text(size = 12,family = "Palatino",vjust = 0.5, hjust = 0.5,face = "bold"),
    axis.text.y = element_text(size = 10,family = "Palatino"),
    axis.text.x = element_text(size = 10,family = "Palatino"),

    # Title, subtitle, and caption
    plot.title = element_text(size = 14,family = "Palatino",face = "bold"),
    plot.subtitle = element_text(size = 12,family = "Palatino"),
    plot.caption = element_text(size = 10,family = "Palatino", hjust = 0),

    # Legend
    legend.position = "bottom",
    legend.text = element_text(size = 12,family = "Palatino"),
    legend.title = element_text(size = 12,family = "Palatino",face = "bold"),
    legend.key = element_blank(),

    # Other
    axis.ticks = element_blank(),
    strip.text = element_text(size = 12,family = "Palatino",vjust = 1,hjust = 0.5, face="bold"),
    strip.background = element_blank(),
    text = element_text(family = "Palatino")
)

```

### `capping.R`

```r
#   Goal: Creating themes and aesthetics for plots.

path <- "/Users/janhendrikpretorius/Library/CloudStorage/OneDrive-StellenboschUniversity/01-Masters-2023/02 Financial Econometrics/20713479-FMX-Prac/"
source(paste0(path, "UTILITIES/libraries.R")) #   Load libraries

# Define colour palette
palette <- c("#1F6F70", "#D98515", "#1E3364", "#75A21B", "#C93D44", "#38B0FF", "#8E22AC")

# Define plot theme
th <- theme(
    # Background and grid
    panel.background = element_blank(),
    plot.background = element_rect(fill = "white", color = "white"),
    panel.grid.major = element_line(color = "#565857", size = 0.1),
    panel.grid.minor = element_line(color = "#565857", size = 0.1),
    axis.line = element_line(size = 0.3, color = "black"),

    # Axis titles and labels
    axis.title.x = element_text(size = 12,family = "Palatino", vjust = 0.5, hjust = 0.5, face = "bold"),
    axis.title.y = element_text(size = 12,family = "Palatino",vjust = 0.5, hjust = 0.5,face = "bold"),
    axis.text.y = element_text(size = 10,family = "Palatino"),
    axis.text.x = element_text(size = 10,family = "Palatino"),

    # Title, subtitle, and caption
    plot.title = element_text(size = 14,family = "Palatino",face = "bold"),
    plot.subtitle = element_text(size = 12,family = "Palatino"),
    plot.caption = element_text(size = 10,family = "Palatino", hjust = 0),

    # Legend
    legend.position = "bottom",
    legend.text = element_text(size = 12,family = "Palatino"),
    legend.title = element_text(size = 12,family = "Palatino",face = "bold"),
    legend.key = element_blank(),

    # Other
    axis.ticks = element_blank(),
    strip.text = element_text(size = 12,family = "Palatino",vjust = 1,hjust = 0.5, face="bold"),
    strip.background = element_blank(),
    text = element_text(family = "Palatino")
)
```

### `libraries.R`

```r
pacman::p_load(dplyr,
               ggplot2,
               tidyverse,
               stringr,
               tidytext,
               readxl,
               xtable,
               PerformanceAnalytics,
               tbl2xts,
               lubridate,
               gt,
               rmsfuns,
               RiskPortfolios,
               fitHeavyTail)

```

### `missing-values.R`

```r

#   Goal: Handling missing values in data

path <- "/Users/janhendrikpretorius/Library/CloudStorage/OneDrive-StellenboschUniversity/01-Masters-2023/02 Financial Econometrics/20713479-FMX-Prac/"
source(paste0(path, "UTILITIES/libraries.R")) #   Load libraries

impute_missing_returns <- function(return_mat, impute_returns_method = "NONE"){
  # Make sure we have a date column called date:
  if( !"date" %in% colnames(return_mat) ) stop("No 'date' column provided in return_mat. Try again please.")
  
  # Note my use of 'any' below
  # Also note that I 'return' return_mat - which stops the function and returns return_mat.
  if( impute_returns_method %in% c("NONE", "None", "none") ) {
    if( any(is.na(return_mat)) ) warning("There are missing values in the return matrix.. Consider maybe using impute_returns_method = 'Drawn_Distribution_Own' / 'Drawn_Distribution_Collective'")
    return(return_mat)
  }
  
  
  if( impute_returns_method  == "Average") {
    
    return_mat <-
      return_mat %>% gather(Stocks, Returns, -date) %>%
      group_by(date) %>%
      mutate(Avg = mean(Returns, na.rm=T)) %>%
      mutate(Avg = coalesce(Avg, 0)) %>% # date with no returns - set avg to zero
      ungroup() %>%
      mutate(Returns = coalesce(Returns, Avg)) %>% select(-Avg) %>% spread(Stocks, Returns)
    
    # That is just so much easier when tidy right? See how I gathered and spread again to give back a wide df?
    return(return_mat)
  } else
    
    if( impute_returns_method  == "Drawn_Distribution_Own") {
      
      N <- nrow(return_mat)
      return_mat <-
        # DIY: see what density function does
        left_join(return_mat %>% gather(Stocks, Returns, -date),
                  return_mat %>% gather(Stocks, Returns, -date) %>% group_by(Stocks) %>%
                    mutate(Dens = list(density(Returns, na.rm=T))) %>%
                    summarise(set.seed(as.numeric(format( Sys.time(), format = "%s"))/1e3*sample(1:100)[1]), Random_Draws = list(sample(Dens[[1]]$x, N, replace = TRUE, prob=.$Dens[[1]]$y))),
                  by = "Stocks"
        ) %>%  group_by(Stocks) %>%
        # Random draw from sample:
        mutate(Returns = coalesce(Returns, Random_Draws[[1]][row_number()])) %>%
        select(-Random_Draws) %>% ungroup() %>% spread(Stocks, Returns)
      return(return_mat)
    } else
      
      if( impute_returns_method  == "Drawn_Distribution_Collective") {
        NAll <- nrow(return_mat %>% gather(Stocks, Returns, -date))
        # DIY: see what density function does
        return_mat <-
          bind_cols(
            return_mat %>% gather(Stocks, Returns, -date),
            return_mat %>% gather(Stocks, Returns, -date) %>%
              mutate(Dens = list(density(Returns, na.rm=T))) %>%
              summarise(set.seed(as.numeric(format( Sys.time(), format = "%s"))/1e3*sample(1:100)[1]), Random_Draws = list(sample(Dens[[1]]$x, NAll, replace = TRUE, prob=.$Dens[[1]]$y))) %>%
              unnest(Random_Draws)
          ) %>%
          mutate(Returns = coalesce(Returns, Random_Draws)) %>% select(-Random_Draws) %>% spread(Stocks, Returns)
        return(return_mat)
      } else
        
        if( impute_returns_method  == "Zero") {
          warning("This is probably not the best idea but who am I to judge....")
          return_mat[is.na(return_mat)] <- 0
          return(return_mat)
        } else
          stop("Please provide a valid impute_returns_method method. Options include:\n'Average', 'Drawn_Distribution_Own', 'Drawn_Distribution_Collective' and 'Zero'.")
  
  return_mat
  
}


```


---

## Documentation

Links to further documentation, if available.

---

## Authors

- Jan-Hendrik Pretorius, Stellenbosch University
  - Project and analysis

- Nico Katzke
  - `Proportional_Cap_Foo` function
  - `impute_missing_returns` function
