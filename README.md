# Main objective
Creates geographic matching tables between Oxford Economics eFUA, GHS-FUA and GHS Urban Centres.

# Definitions 
Each dataset has a different urban definition, as such:

- OE eFUA: 
- GHS-FUA: Areas in which at least 15% of the population is commuting to the main Urban Centre of the area. Delineated commuting area of the Urban Centres of epoch 2015, estimated through an automated classification procedure developed using OECD defintion/methodology.
- GHS-UC: Areas defined by specific cut-off values on resident population and built-up surface share in a 1x1 km uniform global grid. 

# Tasks
1. Review defintions and temporal dimension
2. Create code for uploading and generate unique matching code
3. Create matching table

# Technical documentation
- GHS-UCDB R2019A:[Description of the GHS Urban Centre Database 2015](https://op.europa.eu/en/publication-detail/-/publication/aec4581b-29c5-11e9-8d04-01aa75ed71a1/language-en)
- GHS-FUAs: [GHSL-OECD Functional Urban Areas](https://human-settlement.emergency.copernicus.eu/documents/GHSL_FUA_2019.pdf?t=1583246033)
- OE: proprietary data
