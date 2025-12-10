# SUSTAIN Salmo trutta data

A subset of data from the SUSTAIN trout capture-recapture data set from
Moe et al. (2020). Observations are of total body length in centimetres.
Data prepared by taking a stratified sample of individual IDs based on
the number of observations per individual: 25 individuals with 2
observations, 15 with 3, 10 with 4. Within the groups a simple random
sample without replacement was used. Data was then transformed and
renamed to match the required structure to act as demonstration for the
package.

## Usage

``` r
Trout_Size_Data
```

## Format

### `Trout_Size_Data`

A data frame with 135 rows and 4 columns:

- ind_id:

  ID number for individual

- time:

  Years since first capture and tagging of individual.

- y_obs:

  Individual length in centimetres.

- obs_index:

  Index of observations for individual

## Source

[doi:10.3897/BDJ.8.e52157](https://doi.org/10.3897/BDJ.8.e52157)
