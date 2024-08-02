# Shift Tables

Shift tables display the change in the frequency of subjects across specified categories from baseline to post-baseline time points. They are commonly used in clinical data to show shifts in the values of laboratory parameters, ECG interpretations, or other ordinal variables of interest across visits, providing a comparison between treatment groups (Jenna Cody, 2019).

## Get Big N

These counts display the total number of subjects in each treatment group at the top level of the report headers.

## Generate Missing Rows

Check the mock shell or SAP as this is sometimes optional. Use the `ADSL` dataset, as it contains one record per subject, and assign the expected visits (`AVISITN = 8, 11, 12`). The `expand.grid` function creates a data frame from all combinations of the supplied vectors. For the baseline (`BASEC`) and post-baseline values (`AVALC`), assign "Missing". If the provided SAP/mock shell does not contain the post-baseline and baseline result value as "Missing", then ignore this step.

## Treatment Total

Use `rbind` or `bind_rows` to duplicate the created data frame and use `mutate` or `transform` within `rbind` to assign `trt01an = 3`, which represents the treatment total.

## Update Missing Rows Dataset

The `ADEG` dataset contains the actual baseline (`AVALC`) and post-baseline values (`BASEC`). The dataset created using the `expand.grid` function has all possible visits a subject can have with `AVALC` and `BASEC` values assigned as 'Missing'. Update this with the actual values after doing a `left_join`. You can use `ifelse` or `coalesce` depending on your preference. If a subject has no actual value from `ADEG`, those values will remain as "Missing".

## Get Count

```r
# Summary statistics function
f_summary <- function(group = c("AVISITN", "AVISIT", "TRT01AN", "BASEC_", "AVALC_")) {
  adeg02 %>%
    group_by_at(group) %>% # Dynamic grouping - vars passed as arguments to a function
    summarise(count = n(), .groups = "drop")
}

result1 <- f_summary()
View(result1)
