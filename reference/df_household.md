# Sample dataset of households

This is a synthetic dataset containing household information primarily
used for demonstration purposes on how to use the `mpindex` package.

## Usage

``` r
df_household
```

## Format

A tibble with 198 rows and 21 variables:

- uuid:

  Unique ID

- class:

  Urbanity: `Rural` or `Urban`

- drinking_water:

  Acess to drinking water: `1` - improved; `2` - unimproved

- toilet:

  Service level of toilet or sanitation facility: `1` - basic; `2` -
  limited; `3` - unimproved; `4` - open defecation

- with_child_died:

  With at least one (1) child died in the last five (5) years: `1` -
  with child died; `2` - without child died

- roof:

  Main construction material of the roof: `1` - galvanized
  iron/aluminum; `2` - concrete/clay tile; `3` - half galvanized iron
  and half concrete; `4` - wood/bamboo; `5` - cogon/nipa/anahaw; `6` -
  asbestos; `7` - makeshift/salvaged/improvised materials; `9` - other
  construction material

- walls:

  Main construction material of the outer walls: `1` -
  concrete/brick/stone; `2` - wood; `3` - half concrete/brick/stone and
  half wood; `4` - Galvanized iron/aluminum; `5` -
  bamboo/sawali/cogon/nipa; `6` - asbestos; `7` - glass; `8` -
  makeshift/salvaged/improvised materials; `9` - none; `10` - concrete
  hollow blocks; `11` - concrete hollow blocks/wood; `12` - shear walls;
  `99` - other construction material

- floor:

  Main construction material of the floor: `1` - concrete; `2` - wood;
  `3` - coconut lumber; `4` - bamboo; `5` - earth/sand/mud; `6` -
  makeshift/salvaged/improvised materials; `9` - other construction
  material

- electricity:

  Access to electricity: `1` - with access to electricity; `2` - without
  access to electricity

- cooking_fuel:

  Fuel use for cooking: `1` - electricity; `2` - kerosene (gaas); `3` -
  liquified petroleum gas (LPG); `4` - charcoal; `5` - wood; `6` - none;
  `9` - other cooking fuel such as dung, agricultural crop, or shrubs

- asset_radio:

  Number of working radio owned by the household

- asset_tv:

  Number of working television owned by the household

- asset_telephone:

  Number of working telephone owned by the household

- asset_mobile_phone:

  Number of working mobile phone owned by the household

- asset_computer:

  Number of working computer owned by the household

- asset_animal_cart:

  Number of animal carts owned by the household

- asset_bicycle:

  Number of bicycle owned by the household

- asset_motorcycle:

  Number of motorcylce owned by the household

- asset_refrigerator:

  Number of working refrigerator owned by the household

- asset_car:

  Number of car owned by the household

- asset_truck:

  Number of trucks owned by the household

## See also

[df_household_roster](https://yng-me.github.io/mpindex/reference/df_household_roster.md)

## Examples

``` r
df_household
#> # A tibble: 198 × 21
#>    uuid            class drinking_water toilet with_child_died  roof walls floor
#>    <chr>           <chr>          <int>  <int>           <int> <int> <int> <int>
#>  1 5dbec60a-ebda-… Rural              1      1               2     1     1     1
#>  2 8b70c208-8642-… Rural              1      1               2     1     1     1
#>  3 aa7cb64d-ba16-… Rural              1      1               2     1     3     1
#>  4 df3e5c9b-7218-… Rural              1      1               2     1     3     1
#>  5 57babe6a-c163-… Rural              1      1               2     1     3     1
#>  6 ba3f75cd-102d-… Rural              1      1               2     1     5     4
#>  7 291c03d9-7947-… Rural              1      1               2     1     5     1
#>  8 b8d1b52e-2b5d-… Rural              1      1               2     1     1     1
#>  9 2e80bf1a-03e9-… Rural              1      1               2     1     3     1
#> 10 208992f0-9c6d-… Rural              1      1               2     1     1     1
#> # ℹ 188 more rows
#> # ℹ 13 more variables: electricity <int>, cooking_fuel <int>,
#> #   asset_refrigerator <int>, asset_radio <int>, asset_tv <int>,
#> #   asset_telephone <int>, asset_mobile_phone <int>, asset_animal_cart <int>,
#> #   asset_computer <int>, asset_motorcycle <int>, asset_bicycle <int>,
#> #   asset_car <int>, asset_truck <int>
```
