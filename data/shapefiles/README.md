The cleaned version of the shapefile we are going to use in our analysis is located in 

    alhasan-pk-adm-all-with-clusters

The attributes of the shapes can be grouped as follows:
- All columns with the prefix `S_` are attributes coming from the cluster location of official prevalence survey dataset. 
- All columns with the prefix `MATCH_` are attributes providing details on the name matching process from the survey clusters to the Alhasan shapes.
- Some columns are described as "Cleaning" below, these are columns that were generated from scratch based on other columns, or heavily processed with generated data. In particular, some admin division codes `ADM1_CODE`, `ADM2_CODE` and `ADM3_CODE` of somes provinces ("AZAD KASHMIR", "GILGIT BALTISTAN", "FEDERAL CAPITAL TERRITORY", and "JAMMU & KASHMIR") were missing and so they were all re-generated from scratch. These were generated to enable us to build a Province/District/Tehsil name matching table between various data providers.
- Other columns are cleaned versions of Alhasan's attributes.

The attributes are the following ones:

|   Column     | Description                                                 |
|--------------|-------------------------------------------------------------|
| `S_CLUSTER`  | (Prev. survey) Cluster number                               |
| `S_PROVINCE` | (Prev. survey) Name of provinces                            |
| `S_DISTRICT` | (Prev. survey) Name of district                             |
| `S_TEHSIL`   | (Prev. survey) Name of selected tehsils                     |
| `S_UC`       | (Prev. survey) Selected Union Councils                      |
| `S_CLUSTER`  | (Prev. survey) Selected Cluster                             |
| `S_URBAN`    | (Prev. survey) Urbanization of the selected cluster         |
| `S_NOTES`    | (Prev. survey) Notes on the selected cluster                |
| `S_ADM2_ID`  | (Prev. survey) Id of the District (from official shapefile) |
| `S_ADM3_ID`  | (Prev. survey) Id of the Tehsils (does not match anything known) |
| `MATCH_QUAL` | (Name matching) Quality of the matching (AUTO or MANUAL)    |
| `MATCH_INFO` | (Name matching) Notes on the matching                       |
| `ADM_LEVEL`  | (Alhasan) Level of admin division of the shape (1..4)       |
| `ADM0_EN`    | (Alhasan) Name of the Country                               |
| `ADM1_EN`    | (Alhasan) Name of the Province                              |
| `ADM1ALT_EN` | (Alhasan) Alternate name of the Province (only for FATA)    |
| `ADM2_EN`    | (Alhasan) Name of the District                              |
| `ADMIN_UNIT` | (Alhasan) Name of the Admin Unit (an admin division not used by HDX or the survey, see Wikipedia for more details) |
| `ADM3_EN`    | (Alhasan) Name of the Tehsil                                |
| `ADM4_EN`    | (Alhasan) Name of the Union Council                         |
| `ADM4ALT_EN` | (Alhasan) Aternate name of the Union Council                |
| `TOWN`       | (Alhasan) Name of the Town                                  |
| `A_UC_URBAN` | (Alhasan) Urbanization of the Union Council                 |
| `ADM1_CODE`  | (Alhasan/Cleaning) Numerical code of the Province           |
| `ADM2_CODE`  | (Alhasan/Cleaning) Numerical code of the District           |
| `ADM3_CODE`  | (Alhasan/Cleaning) Numerical code of the Tehsil             |
| `UC_IS_FAKE` | (Cleaning) Indicate if the UC was missing and created from Tehsil |
| `DOT_NAME`   | (Cleaning) Normalized shape name. It corresponds to colon-separated `ADM{i}_EN` for all levels until `ADM_LEVEL` |
| `HAS_CLUST`  | (Cleaning) Indicate if the shape corresponds to a prevalence survey cluster |
