# Goal: Calculate portfolio return by reweigthing

path <- "/Users/janhendrikpretorius/Library/CloudStorage/OneDrive-StellenboschUniversity/01-Masters-2023/02 Financial Econometrics/20713479-FMX-Prac/"
source(paste0(path, "UTILITIES/libraries.R")) #   Load libraries

calculatePortfolioReturns <- function(Indexes, RebMonths) {
    # Convert to long format
    Indexes_long <- Indexes %>%
        gather(key = "Index", value = "Return", -date)

    # Make a weighting dataframe
    Weights <- Indexes_long %>%
        mutate(
            Weight = ifelse(Index %in% c("MSCI_ACWI", "J433"), 0.6, 0.4) *
                ifelse(Index %in% c("J433", "ALBI"), 0.7, 0.3)
        ) %>%
        mutate(Months = as.numeric(format(date, format = "%m")),
               YearMonths = as.numeric(format(date, format = "%Y%m"))) %>%
        filter(Months %in% RebMonths) %>%
        group_by(YearMonths, Months, Index) %>% filter(date == last(date)) %>% ungroup() %>%
        select(-c(Months, YearMonths))

    # Assume an initial R1000 investment
    Fund_Size_at_Start <- 1000

    # Step 2: Calculate Portfolio Returns

    EW_weights <- Weights %>%
        select(date, Index, Weight) %>%
        spread(Index, Weight) %>%
        tbl_xts()

    Indexes[is.na(Indexes)] <- 0
    xts_Indexes <- Indexes %>% tbl_xts()

    EW_RetPort <-
        rmsfuns::Safe_Return.portfolio(xts_Indexes,
                                       weights = EW_weights, lag_weights = TRUE,
                                       verbose = TRUE, contribution = TRUE,
                                       value = Fund_Size_at_Start, geometric = TRUE)

    # Clean and save portfolio returns and weights:
    EW_Contribution <-
        EW_RetPort$"contribution" %>% xts_tbl()

    EW_BPWeight <-
        EW_RetPort$"BOP.Weight" %>% xts_tbl()

    EW_BPValue <-
        EW_RetPort$"BOP.Value" %>% xts_tbl()

    names(EW_Contribution) <- c("date", names(EW_RetPort$"contribution"))
    names(EW_BPWeight) <- c("date", names(EW_RetPort$"BOP.Weight"))
    names(EW_BPValue) <- c("date", names(EW_RetPort$"BOP.Value"))

    # Bind dataframes together
    df_port_return_EW <-
        left_join(Indexes_long %>% rename("date" = date),
                  EW_BPWeight %>% gather(Index, Weight, -date),
                  by = c("date", "Index") ) %>%

        left_join(.,
                  EW_BPValue %>% gather(Index, value_held, -date),
                  by = c("date", "Index") ) %>%

        left_join(.,
                  EW_Contribution %>% gather(Index, Contribution, -date),
                  by = c("date", "Index"))

    # Calculate Portfolio Returns:
    Return <-
        df_port_return_EW %>% group_by(date) %>% summarise(PortfolioReturn = sum(Return*Weight, na.rm =TRUE)) %>%
        filter(PortfolioReturn != 0)

    # Return the final dataframe
    return(Return)
}