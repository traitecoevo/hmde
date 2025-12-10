# Garcinia recondita - Barro Colorado Island data

A subset of data from the Barro Colorado Island long term forest plot
managed by the Smithsonian Tropical Research Institute (Condit et al.
2019). Data was prepared by taking a simple random sample without
replacement of 30 individual IDs from Garcinia recondita. The sampling
frame was restricted to individuals with 6 observations since 1990, and
a difference between observed first and last sizes of more than 3cm in
order to avoid identifiability issues. Data was then transformed and
renamed to match the required structure to act as demonstration for the
package.

## Usage

``` r
Tree_Size_Data
```

## Format

### `Tree_Size_Data`

A data frame with 300 rows and 4 columns:

- ind_id:

  ID number for individual

- time:

  Years since first observation.

- y_obs:

  Individual diameter at breast height (DBH) in centimetres.

- obs_index:

  Index of observations for individual

## Source

[doi:10.15146/5xcp-0d46](https://doi.org/10.15146/5xcp-0d46)

## References

[doi:10.1002/ecy.4140](https://doi.org/10.1002/ecy.4140)
