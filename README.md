# Shift Tables

Shift tables display the change in the frequency of subjects across specified categories from baseline to post-baseline time points. They are commonly used in clinical data to show shifts in the values of laboratory parameters, ECG interpretations, or other ordinal variables of interest across visits, providing a comparison between treatment groups (Jenna Cody, 2019).
## Steps:

### Get Big N

These counts display the total number of subjects in each treatment group at the top level of the report headers.

### Generate Missing Rows

Check the mock shell or SAP as this is sometimes optional. Use the `ADSL` dataset, as it contains one record per subject, and assign the expected visits (`AVISITN = 8, 11, 12`). The `expand.grid` function creates a data frame from all combinations of the supplied vectors. For the baseline (`BASEC`) and post-baseline values (`AVALC`), assign "Missing". If the provided SAP/mock shell does not contain the post-baseline and baseline result value as "Missing", then ignore this step.

### Treatment Total

Use `rbind` or `bind_rows` to duplicate the created data frame and use `mutate` or `transform` within `rbind` to assign `trt01an = 3`, which represents the treatment total.

```          R
missing_rows <- adsl1 %>%
    bind_rows(mutate(adsl1, TRT01AN = 3)) %>% ...
 ```


### Update Missing Rows Dataset

The `ADEG` dataset contains the actual baseline (`AVALC`) and post-baseline values (`BASEC`). The dataset created using the `expand.grid` function has all possible visits a subject can have with `AVALC` and `BASEC` values assigned as 'Missing'. Update this with the actual values after doing a `left_join`. You can use `ifelse` or `coalesce` depending on your preference. If a subject has no actual value from `ADEG`, those values will remain as "Missing".

```r
adeg02 <- missing_rows %>% 
          left_join(adeg1, by = c("USUBJID", "AVISITN", "AVISIT", "TRT01AN")) %>%
          mutate(
                  BASEC_ = ifelse(!is.na(BASEC.y), BASEC.y, BASEC.x),
                  AVALC_ = AVALC_=coalesce(AVALC.y,AVALC.x)
    ) %>% ...

```

### Get Count

We have created a user-defined function to get the counts at different grouping levels. The `group_by_at()` function is used to dynamically specify the grouping variables. This is useful when you want to pass a variable or a list of variables as arguments to a function. The following functions play a similar role: `group_by(pick(all_of(group)))` or `group_by(across(all_of(group)))`.

```r
# Summary statistics function
f_summary <- function(group = c("AVISITN", "AVISIT", "TRT01AN", "BASEC_", "AVALC_")) {
  adeg02 %>%
    group_by_at(group) %>% # Dynamic grouping - vars passed as arguments to a function
    summarise(count = n(), .groups = "drop")
}

result1 <- f_summary()
View(result1)
```
#### Why is `group_by(group)` wrong?

Using `group_by(group)` treats `group` as a single variable name, so it looks for a column literally named `group` in our data frame, which does not exist. It does not interpret `group` as a vector of column names to be used for grouping. Therefore, a dynamic approach is required to select and use the column names.

### Dummy Shell

The "dummy shell" ensures that all possible combinations of certain grouping variables are present in the dataset, even if some of them are missing in the actual data. Initialize the `count` variable to zero and update it with the actual count data. This requires merging using `left_join`.

```r
# Using expand.grid function
shell <- expand.grid(
  AVISITN = unique(results$AVISITN),
  BASEC_ = unique(results$BASEC_),
  AVALC_ = unique(results$AVALC_),
  TRT01AN = unique(results$TRT01AN)
  ) %>%
  mutate(count = 0,...))

View(shell)
```
### Calculate Percentages

Percentage in a particular cell is calculated as follows: the number of subjects in that particular cell divided by the total number of subjects per visit per treatment group multiplied by 100.

```r
reshell1 <- reshell %>% 
            group_by(AVISITN, TRT01AN) %>%
            mutate(
                   den = last(count),
                   count_c = sprintf("%3d", count),
                   perc_ = sprintf("%5.1f", round(count / den * 100, 1)),
                   ...
                  )   
```

### Transposing from Narrow (Long) to Wide

The `clean_names()` function comes from the `janitor` package ensures all variable names are consistent in just one line of code.

```r
t_reshell <- reshell1 %>%
  pivot_wider(names_from = c(BASEC_, TRT01AN), values_from = count_per) %>%
  select(AVISIT, everything()) %>%
  clean_names()

