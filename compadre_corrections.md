Compadre Corrections
================
Patrick Barks
2018-01-25

 

### Load libraries and Com(p)adre databases, and adjust console width

``` r
# load libraries
library(dplyr)
library(tidyr)
library(tibble)
library(purrr)

# load Comadre and Compadre databases
load('COMADRE_v.X.X.X.RData')
load('COMPADRE_v.X.X.X.RData')

# increase console width
options(width = 120)
```

 

### `DOI.ISBN` with &gt;1 unique `Authors`, `YearPublication`, or `Journal`

A given DOI or ISBN should be associated with a single publication (e.g a single journal, year, and set of authors). The code chunk below finds instances where this is not the case. Some of these cases seem to reflect data entry errors, but some represent subtle differences in spelling or formatting of author names (e.g. spelled with vs. without accents, only the first author is listed vs. all authors listed, etc.). Also, some discrepencies in `YearPublication` for a given DOI may reflect the time between an 'early-online' version and the final published version.

``` r
# COMADRE
comadre$metadata %>% 
  filter(!is.na(DOI.ISBN)) %>% 
  group_by(DOI.ISBN) %>% 
  mutate(n_uniq_author = length(unique(Authors)),
         n_uniq_year = length(unique(YearPublication)),
         n_uniq_journal = length(unique(Journal))) %>% 
  ungroup() %>% 
  filter(n_uniq_author > 1 | n_uniq_year > 1 | n_uniq_journal > 1) %>% 
  dplyr::select(DOI.ISBN, Journal, Year = YearPublication, Authors) %>% 
  unique() %>% 
  arrange(DOI.ISBN, Authors) %>% 
  mutate(Authors = substr(Authors, 1, 40)) %>% 
  as.data.frame()
```

    ##                     DOI.ISBN                Journal Year                                  Authors
    ## 1           10.1002/jwmg.835       Neu Par Sci Comp 2015          Chiquet; Montgomery; Ma; Ackleh
    ## 2           10.1002/jwmg.835      J Wildlife Manage 2015 Chitwood; Lashley; Kilgo; Moorman; Deper
    ## 3  10.1007/s10646-010-0507-y               Ecotoxic 2011 Duchet; Coutellec; Franquet; Lagneau; La
    ## 4  10.1007/s10646-010-0507-y               Ecotoxic 2010 Duchet; Coutellec; Franquet; Lagneau; La
    ## 5  10.1007/s10750-007-9136-8               Hydrobio 2008                  Pardo; Vila; Bustamante
    ## 6  10.1007/s10750-007-9136-8               Mar Biol 2001                     Prevedelli; Simonini
    ## 7     10.1080/20018091094835 Hum & Ecol Risk Assess 2001                                  Gleason
    ## 8     10.1080/20018091094835 Hum & Ecol Risk Assess 2001                           Gleason; Nacci
    ## 9            10.2307/1447430              Cons Biol 1993          Congdon; Dunham; van Loben Sels
    ## 10           10.2307/1447430              Ecol Appl 1994         Crowder; Crouse; Heppell; Martin
    ## 11           10.2307/1447430                Ecology 1990                  Frazer; Gibbons; Greene
    ## 12           10.2307/1447430      Wildlife Research 1996  Heppel; Limpus; Crouse; Frazer; Crowder
    ## 13           10.2307/1447430                 Copeia 1998                                  Heppell
    ## 14           10.2307/1447430          Herpetologica 1991                                  Iverson

``` r
# COMPADRE
compadre$metadata %>% 
  filter(!is.na(DOI.ISBN)) %>% 
  group_by(DOI.ISBN) %>% 
  mutate(n_uniq_author = length(unique(Authors)),
         n_uniq_year = length(unique(YearPublication)),
         n_uniq_journal = length(unique(Journal))) %>% 
  ungroup() %>% 
  filter(n_uniq_author > 1 | n_uniq_year > 1 | n_uniq_journal > 1) %>% 
  dplyr::select(DOI.ISBN, Journal, Year = YearPublication, Authors) %>% 
  unique() %>% 
  arrange(DOI.ISBN, Authors) %>% 
  mutate(Authors = substr(Authors, 1, 25)) %>% 
  as.data.frame()
```

    ##                                           DOI.ISBN           Journal Year                   Authors
    ## 1                      10.1016/j.actao.2007.11.005 Bas and Appl Ecol 2008                    Ramula
    ## 2                      10.1016/j.actao.2007.11.005         Acta Oeco 2008                    Ramula
    ## 3                    10.1016/S0006-3207(01)00113-6         Cons Biol 2003                    Garcia
    ## 4                    10.1016/S0006-3207(01)00113-6         Biol Cons 2002      García; Guzman; Goñi
    ## 5                 10.1111/j.1365-2664.2005.01100.x       J Appl Ecol 2005 Buckley; Brockerhoff; Lan
    ## 6                 10.1111/j.1365-2664.2005.01100.x            J Ecol 2005   van Mantgem; Stephenson
    ## 7                 10.1111/j.1365-2745.2007.01298.x            J Ecol 2007 López-Hoffman; Ackerly; A
    ## 8                 10.1111/j.1365-2745.2007.01298.x            J Ecol 2007 López-Hoffman; Ackerly; A
    ## 9               10.1111/j.1442-9993.1988.tb00999.x       Aust J Ecol 1988      Bradstock; O'Connell
    ## 10              10.1111/j.1442-9993.1988.tb00999.x        Aust J Bot 1988      Bradstock; O'Connell
    ## 11              10.1111/j.1469-8137.1984.tb03596.x         New Phyto 1984           Boorman; Fuller
    ## 12              10.1111/j.1469-8137.1984.tb03596.x        Ecol Monog 1992           Cochran; Ellner
    ## 13              10.1111/j.1654-1103.2007.tb02519.x         J Veg Sci 2007                    Dostal
    ## 14              10.1111/j.1654-1103.2007.tb02519.x         J Veg Sci 2007                    Dostál
    ## 15    10.1658/1100-9233(2007)18[91:PDOAIP]2.0.CO;2         J Veg Sci 2007                    Dostal
    ## 16    10.1658/1100-9233(2007)18[91:PDOAIP]2.0.CO;2         J Veg Sci 2007                    Dostál
    ## 17 10.1890/1051-0761(2006)016[2399:DMISOB]2.0.CO;2          Weed Sci 2006                     Davis
    ## 18 10.1890/1051-0761(2006)016[2399:DMISOB]2.0.CO;2         Ecol Appl 2006 Davis; Landis; Nuzzo; Blo
    ## 19                               10.1890/11-1052.1           Ecology 2012                     Ellis
    ## 20                               10.1890/11-1052.1           Ecology 2012 Ellis; Williams; Lesica; 
    ## 21                                 10.2307/2404276       J Appl Ecol 1993                  O'Connor
    ## 22                                 10.2307/2404276       J Appl Ecol 1992         O'Connor; Pickett
    ## 23                                 10.2307/2445615     Great Bas Nat 1995                    Lesica
    ## 24                                 10.2307/2445615          Am J Bot 1995            Lesica; Shelly
    ## 25                            10.3170/2007-8-18342         J Veg Sci 2008 Tanaka; Shibata; Masaki; 
    ## 26                            10.3170/2007-8-18342         J Veg Sci 2008 Tanaka; Shibata; Masaki; 
    ## 27                             10.3732/ajb.0900351          Am J Bot 2010 Bucharova; Munzbergova; T
    ## 28                             10.3732/ajb.0900351          Am J Bot 2010 Bucharová; Münzbergová; T
    ## 29                          10.3732/ajb.92.12.1987          Am J Bot 2005               Münzbergova
    ## 30                          10.3732/ajb.92.12.1987          Am J Bot 2005               Münzbergová
    ## 31                               978-84-8014-746-0              Book 2009 Iriondo; Albert; Gimenez;
    ## 32                               978-84-8014-746-0              Book 2009 Iriondo; Albert; Giménez;

 

### `Authors`x`YearPublication`x`Journal` with &gt;1 unique `DOI.ISBN`

A given combination of author, year, and journal should generally correspond to a single DOI or ISBN (though of course a given set of authors could publish more than one paper in a given journal and year). Below are instances where this is not the case. Some may reflect publications legitimately having multiple DOIs for different versions (e.g. 'early-online version' vs. published version vs. corrected version), though others seem to reflect data entry errors.

``` r
# COMADRE
comadre$metadata %>% 
  mutate(AuthorJournalYear = paste(Authors, Journal, YearPublication)) %>% 
  group_by(AuthorJournalYear) %>% 
  mutate(n = length(unique(DOI.ISBN))) %>% 
  ungroup() %>% 
  filter(n > 1) %>% 
  dplyr::select(Authors, Year = YearPublication, Journal, DOI.ISBN) %>% 
  unique() %>% 
  arrange(Authors) %>% 
  mutate(Authors = substr(Authors, 1, 35)) %>% 
  as.data.frame()
```

    ##                                Authors Year              Journal                     DOI.ISBN
    ## 1                      Cockrell; Sorte 2013 J Exp Mar Bio & Ecol  10.1016/j.jembe.2012.11.009
    ## 2                      Cockrell; Sorte 2013 J Exp Mar Bio & Ecol  10.1016/j.jembe.2012.11.010
    ## 3     Crowder; Crouse; Heppell; Martin 1994            Ecol Appl              10.2307/1941948
    ## 4     Crowder; Crouse; Heppell; Martin 1994            Ecol Appl              10.2307/1447430
    ## 5  Cruz; Pech; Seddon; Cleland; Nelson 2013            Biol Cons 10.1016/j.biocon.2013.09.006
    ## 6  Cruz; Pech; Seddon; Cleland; Nelson 2013            Biol Cons 10.1016/j.biocon.2013.09.005
    ## 7                              Iverson 1991        Herpetologica              10.2307/1447430
    ## 8                              Iverson 1991        Herpetologica              10.2307/1447431
    ## 9                               Tucker 2001                 Book            978-0-949324-89-4
    ## 10                              Tucker 2001                 Book                         <NA>

``` r
# COMPADRE
compadre$metadata %>% 
  mutate(AuthorJournalYear = paste(Authors, Journal, YearPublication)) %>% 
  group_by(AuthorJournalYear) %>% 
  mutate(n = length(unique(DOI.ISBN))) %>% 
  ungroup() %>% 
  filter(n > 1) %>% 
  dplyr::select(Authors, Year = YearPublication, Journal, DOI.ISBN) %>% 
  unique() %>% 
  arrange(Authors) %>% 
  mutate(Authors = substr(Authors, 1, 35)) %>% 
  as.data.frame()
```

    ##                                Authors Year    Journal                                      DOI.ISBN
    ## 1                               Dostal 2007  J Veg Sci            10.1111/j.1654-1103.2007.tb02519.x
    ## 2                               Dostal 2007  J Veg Sci  10.1658/1100-9233(2007)18[91:PDOAIP]2.0.CO;2
    ## 3                               Dostál 2007  J Veg Sci            10.1111/j.1654-1103.2007.tb02519.x
    ## 4                               Dostál 2007  J Veg Sci  10.1658/1100-9233(2007)18[91:PDOAIP]2.0.CO;2
    ## 5                   Mondragón; Ticktin 2011  Cons Biol              10.1111/j.1523-1739.2011.01691.x
    ## 6                   Mondragón; Ticktin 2011  Cons Biol                                          <NA>
    ## 7                          Nakashizuka 1991  J Veg Sci 10.1658/1100-9233(2007)18[379:VIJSAR]2.0.CO;2
    ## 8                          Nakashizuka 1991  J Veg Sci                                          <NA>
    ## 9              Prevéy; Germino; Huntly 2010  Ecol Appl                               10.1890/09-0750
    ## 10             Prevéy; Germino; Huntly 2010  Ecol Appl                             10.1890/09-0750.1
    ## 11 Stas; Langbroek; Bitariho; Sheil; Z 2016 Afr J Ecol                             10.1111/aje.12309
    ## 12 Stas; Langbroek; Bitariho; Sheil; Z 2016 Afr J Ecol                                          <NA>
    ## 13             van Mantgem; Stephenson 2005     J Ecol               0.1111/j.1365-2745.2005.01007.x
    ## 14             van Mantgem; Stephenson 2005     J Ecol              10.1111/j.1365-2745.2005.01007.x
    ## 15             van Mantgem; Stephenson 2005     J Ecol              10.1111/j.1365-2664.2005.01100.x

 

### Irregular formatting of author names

The code chunk below finds instances of irregular formatting in the `Authors` column (e.g. multiple sequential spaces, or commas where there should be semicolons).

``` r
# check for instances where Authors column contains 2+ spaces in sequence,
#  semicolons followed by character (with no separating space), or commas
#  where there should be semicolons
irregular <- '[[:space:]][[:space:]]+|\\;[[:alpha:]]+|\\,+'
```

``` r
# COMADRE
comadre$metadata %>% 
  dplyr::select(DOI.ISBN, Journal, Year = YearPublication, Authors) %>% 
  unique() %>% 
  mutate(irreg = grepl(irregular, Authors)) %>% 
  filter(irreg == TRUE) %>% 
  dplyr::select(-irreg) %>% 
  arrange(Authors) %>% 
  mutate(Authors = substr(Authors, 1, 40))
```

    ## [1] DOI.ISBN Journal  Year     Authors 
    ## <0 rows> (or 0-length row.names)

``` r
# COMPADRE
compadre$metadata %>% 
  dplyr::select(DOI.ISBN, Journal, Year = YearPublication, Authors) %>% 
  unique() %>% 
  mutate(irreg = grepl(irregular, Authors)) %>% 
  filter(irreg == TRUE) %>% 
  dplyr::select(-irreg) %>% 
  arrange(Authors) %>% 
  mutate(Authors = substr(Authors, 1, 40))
```

    ##                            DOI.ISBN      Journal Year                                  Authors
    ## 1         10.1007/s00442-014-3112-6    Oecologia 2014              Altwegg; De Klerk;  Midgley
    ## 2         10.1017/S0266467400001620  J Trop Ecol 2000                Barot;  Gignoux; Vuattoux
    ## 3                              <NA>       Report 1998 Berlin, Miller, Borovansky, Seal, and By
    ## 4                              <NA> Cact Suc Mex 2013 Demeneghi-Calatayud;Navarro-Carbajal;Sal
    ## 5           10.1111/1365-2745.12140       J Ecol 2013 Gaoue;  Horvitz; Ticktin; Steiner; Tulja
    ## 6  10.1111/j.1365-2745.2007.01298.x       J Ecol 2007 López-Hoffman; Ackerly; Anten; Denoyer;R
    ## 7  10.1111/j.1365-2745.2007.01298.x       J Ecol 2007 López-Hoffman; Ackerly; Anten; DeNoyer;R
    ## 8                              <NA> Cact Suc Mex 2012       Saldivar Sánchez, Navarro Carbajal
    ## 9  10.1111/j.1365-2745.2004.00844.x       J Ecol 2004                 Stokes; Bullok;Watkinson
    ## 10 10.1111/j.1529-8817.2010.00924.x     J Phycol 2010                           Vieira, Santos
    ## 11        10.1017/S0266467409990459  J Trop Ecol 2010                             Wester, Zotz

 

### Country codes different from ISO 3 standard

Country codes listed in the column `Country` should conform to the ISO 3 standard. The code chunk below finds instances where this is not the case.

``` r
# Read list of ISO 3 country codes; obtained manually from:
# https://unstats.un.org/unsd/tradekb/knowledgebase/country-code
country_codes <- read.csv('country_codes.csv')
```

``` r
# function to spread entries containing multiple country codes (separated by ;)
#  across separate lines
SplitCountries <- function(x, DOI.ISBN, Journal, Year, Authors) {
  tibble(Country = strsplit(as.character(x), '; ')[[1]],
         DOI.ISBN, Journal, Year, Authors)
}
```

``` r
# COMADRE
comadre$metadata %>% 
  rowwise() %>% 
  do(SplitCountries(.$Country, .$DOI.ISBN, .$Journal,
                    .$YearPublication, .$Authors)) %>% 
  ungroup() %>% unique() %>% 
  filter(!Country %in% country_codes$ISO, !is.na(Country)) %>% 
  mutate(Authors = substr(Authors, 1, 20)) %>% 
  as.data.frame()
```

    ##       Country                        DOI.ISBN                       Journal Year              Authors
    ## 1         ANT         10.1111/1365-2656.12399                   J Anim Ecol 2015 O'Farrell; Salguero-
    ## 2           3    10.1016/j.ecoenv.2012.01.019     Ecotoxicol Environ Safety 2012 Bergek; Ma; Vetemaa;
    ## 3           1    10.1016/j.ecoenv.2012.01.019     Ecotoxicol Environ Safety 2012 Bergek; Ma; Vetemaa;
    ## 4          1a    10.1016/j.ecoenv.2012.01.019     Ecotoxicol Environ Safety 2012 Bergek; Ma; Vetemaa;
    ## 5          1b    10.1016/j.ecoenv.2012.01.019     Ecotoxicol Environ Safety 2012 Bergek; Ma; Vetemaa;
    ## 6         LAB       10.1007/s11270-014-2207-3         Water Air Soil Pollut 2014 Santadino; Coviella;
    ## 7  Simulation 10.1016/j.ecolmodel.2012.02.018                    Ecol Model 2012 Lesnoff; Corniaux; H
    ## 8           1               10.1111/nrm.12034                   Nat Res Mod 2014 Holma; Lindroos; Oin
    ## 9          57               978-1-100-23563-9 Can Tech Report Fish & Aq Sci 2014 Vélez-Espino; Ford; 
    ## 10         59               978-1-100-23563-9 Can Tech Report Fish & Aq Sci 2014 Vélez-Espino; Ford; 
    ## 11    NODC-27                10.3354/esr00657            Endangered Spp Res 2015      Whitehead; Gero
    ## 12         27                            <NA>                          NMFS 2009             Richards
    ## 13         23                            <NA>                          NMFS 2009             Richards

``` r
# COMPADRE
compadre$metadata %>% 
  rowwise() %>% 
  do(SplitCountries(.$Country, .$DOI.ISBN, .$Journal,
                    .$YearPublication, .$Authors)) %>% 
  ungroup() %>% unique() %>% 
  filter(!Country %in% country_codes$ISO, !is.na(Country)) %>% 
  mutate(Authors = substr(Authors, 1, 35)) %>% 
  as.data.frame()
```

    ##   Country                         DOI.ISBN   Journal Year                             Authors
    ## 1      NL 10.1111/j.1365-3180.2010.00787.x  Weed Res 2010 van den Berg; Gilligan; Gerdessen; 
    ## 2     LAB                10.1890/07-0568.1 Ecol Appl 2008                               Burns

 

### Check for nonstandard values of `MatrixClassOrganized`

Values of `MatrixClassOrganized` should be one of 'active', 'dorm', or 'prop' (or possibly NA). The code chunk below finds instances where this is not the case.

``` r
# function to check whether MatrixClassOrganized column contains 
#  values other than 'active', 'dorm', 'prop', or <NA>
MatrixClassCheck <- function (x) {
  any(!x$MatrixClassOrganized %in% c(NA, 'active', 'dorm', 'prop'))
}
```

``` r
# COMADRE
tibble(DOI.ISBN = comadre$metadata$DOI.ISBN,
       Year = comadre$metadata$YearPublication,
       Authors = comadre$metadata$Authors,
       matrixClass = comadre$matrixClass) %>%
  mutate(flag = map_lgl(matrixClass, MatrixClassCheck)) %>% 
  filter(flag == TRUE) %>% 
  unnest() %>% 
  dplyr::select(-flag, -MatrixClassAuthor) %>% 
  mutate(Authors = substr(Authors, 1, 25)) %>% 
  mutate(MatrixClassOrganized = substr(MatrixClassOrganized, 1, 20)) %>% 
  as.data.frame()
```

    ##                   DOI.ISBN Year                   Authors MatrixClassOrganized MatrixClassNumber
    ## 1   10.1080/20018091094835 2001            Gleason; Nacci               active              1.00
    ## 2   10.1080/20018091094835 2001            Gleason; Nacci               active              2.00
    ## 3   10.1080/20018091094835 2001            Gleason; Nacci               active              3.00
    ## 4   10.1080/20018091094835 2001            Gleason; Nacci               active              4.00
    ## 5   10.1080/20018091094835 2001            Gleason; Nacci First year (nestling              0.00
    ## 6   10.1080/20018091094835 2001            Gleason; Nacci Second year (juvenil              0.50
    ## 7   10.1080/20018091094835 2001            Gleason; Nacci Third year and onwar              0.00
    ## 8   10.1080/20018091094835 2001            Gleason; Nacci First year (nestling              0.00
    ## 9   10.1080/20018091094835 2001            Gleason; Nacci Second year (juvenil              0.50
    ## 10  10.1080/20018091094835 2001            Gleason; Nacci Third year and onwar              0.00
    ## 11  10.1080/20018091094835 2001            Gleason; Nacci First year (nestling              0.00
    ## 12  10.1080/20018091094835 2001            Gleason; Nacci Second year (juvenil              0.50
    ## 13  10.1080/20018091094835 2001            Gleason; Nacci Third year and onwar              0.00
    ## 14 10.1111/1365-2664.12476 2015 Chevallier; Hernandez-Mat               active              1.00
    ## 15 10.1111/1365-2664.12476 2015 Chevallier; Hernandez-Mat               active              2.00
    ## 16 10.1111/1365-2664.12476 2015 Chevallier; Hernandez-Mat               active              3.00
    ## 17 10.1111/1365-2664.12476 2015 Chevallier; Hernandez-Mat    Chicks: 0-1 years              0.00
    ## 18 10.1111/1365-2664.12476 2015 Chevallier; Hernandez-Mat Juveniles: 2-5 years              0.00
    ## 19 10.1111/1365-2664.12476 2015 Chevallier; Hernandez-Mat   Adults: 5-40 years              0.00
    ## 20      10.1111/jofo.12093 2015                     Mumme               active              1.00
    ## 21      10.1111/jofo.12093 2015                     Mumme               active              2.00
    ## 22      10.1111/jofo.12093 2015                     Mumme               active              3.00
    ## 23      10.1111/jofo.12093 2015                     Mumme             Juvenile              0.00
    ## 24      10.1111/jofo.12093 2015                     Mumme               Adults              0.43
    ## 25      10.1111/jofo.12093 2015                     Mumme             Juvenile              0.00
    ## 26      10.1111/jofo.12093 2015                     Mumme               Adults              0.36
    ## 27      10.1111/jofo.12093 2015                     Mumme             Juvenile              0.00
    ## 28      10.1111/jofo.12093 2015                     Mumme               Adults              0.53
    ## 29      10.1111/jofo.12093 2015                     Mumme             Juvenile              0.00
    ## 30      10.1111/jofo.12093 2015                     Mumme               Adults              0.51
    ## 31      10.1111/jofo.12093 2015                     Mumme             Juvenile              0.00
    ## 32      10.1111/jofo.12093 2015                     Mumme               Adults              0.33
    ## 33      10.1111/jofo.12093 2015                     Mumme             Juvenile              0.00
    ## 34      10.1111/jofo.12093 2015                     Mumme               Adults              0.43

``` r
# COMPADRE
tibble(DOI.ISBN = compadre$metadata$DOI.ISBN,
       Year = compadre$metadata$YearPublication,
       Authors = compadre$metadata$Authors,
       matrixClass = compadre$matrixClass) %>%
  mutate(flag = map_lgl(matrixClass, MatrixClassCheck)) %>% 
  filter(flag == TRUE) %>% 
  unnest() %>% 
  dplyr::select(-flag, -MatrixClassAuthor) %>% 
  mutate(Authors = substr(Authors, 1, 25)) %>% 
  mutate(MatrixClassOrganized = substr(MatrixClassOrganized, 1, 20)) %>% 
  as.data.frame()
```

    ##                      DOI.ISBN Year                   Authors MatrixClassOrganized MatrixClassNumber
    ## 1             10.2307/2261637 1995                    Okland                  NDY                 1
    ## 2             10.2307/2261637 1995                    Okland                 <NA>                 2
    ## 3             10.2307/2261637 1995                    Okland                 <NA>                 3
    ## 4             10.2307/2261637 1995                    Okland                 <NA>                 4
    ## 5             10.2307/2261637 1995                    Okland                 <NA>                 5
    ## 6             10.2307/2261637 1995                    Okland                 <NA>                 6
    ## 7     10.1111/1365-2745.12281 2014 Shefferson; Warren II.; P                  NDY                 1
    ## 8     10.1111/1365-2745.12281 2014 Shefferson; Warren II.; P                  NDY                 2
    ## 9     10.1111/1365-2745.12281 2014 Shefferson; Warren II.; P                  NDY                 3
    ## 10    10.1111/1365-2745.12281 2014 Shefferson; Warren II.; P                  NDY                 4
    ## 11    10.1111/1365-2745.12281 2014 Shefferson; Warren II.; P                  NDY                 5
    ## 12    10.1111/1365-2745.12281 2014 Shefferson; Warren II.; P                  NDY                 6
    ## 13    10.1111/1365-2745.12281 2014 Shefferson; Warren II.; P                  NDY                 7
    ## 14    10.1111/1365-2745.12281 2014 Shefferson; Warren II.; P                  NDY                 8
    ## 15    10.1111/1365-2745.12281 2014 Shefferson; Warren II.; P                  NDY                 9
    ## 16    10.1111/1365-2745.12281 2014 Shefferson; Warren II.; P                  NDY                10
    ## 17    10.1111/1365-2745.12281 2014 Shefferson; Warren II.; P                  NDY                11
    ## 18    10.1111/1365-2745.12281 2014 Shefferson; Warren II.; P                  NDY                12
    ## 19    10.1111/1365-2745.12281 2014 Shefferson; Warren II.; P                  NDY                13
    ## 20    10.1111/1365-2745.12281 2014 Shefferson; Warren II.; P                  NDY                14
    ## 21    10.1111/1365-2745.12281 2014 Shefferson; Warren II.; P                  NDY                15
    ## 22    10.1111/1365-2745.12281 2014 Shefferson; Warren II.; P                  NDY                16
    ## 23    10.1111/1365-2745.12281 2014 Shefferson; Warren II.; P                  NDY                17
    ## 24    10.1111/1365-2745.12281 2014 Shefferson; Warren II.; P                  NDY                18
    ## 25    10.1111/1365-2745.12281 2014 Shefferson; Warren II.; P                  NDY                19
    ## 26    10.1111/1365-2745.12281 2014 Shefferson; Warren II.; P                  NDY                20
    ## 27    10.1111/1365-2745.12281 2014 Shefferson; Warren II.; P                  NDY                21
    ## 28    10.1111/1365-2745.12281 2014 Shefferson; Warren II.; P                  NDY                22
    ## 29    10.1111/1365-2745.12281 2014 Shefferson; Warren II.; P                  NDY                23
    ## 30    10.1111/1365-2745.12281 2014 Shefferson; Warren II.; P                  NDY                24
    ## 31    10.1111/1365-2745.12281 2014 Shefferson; Warren II.; P                  NDY                25
    ## 32    10.1111/1365-2745.12281 2014 Shefferson; Warren II.; P                  NDY                26
    ## 33    10.1111/1365-2745.12281 2014 Shefferson; Warren II.; P                  NDY                27
    ## 34    10.1111/1365-2745.12281 2014 Shefferson; Warren II.; P                  NDY                28
    ## 35    10.1111/1365-2745.12281 2014 Shefferson; Warren II.; P                  NDY                29
    ## 36    10.1111/1365-2745.12281 2014 Shefferson; Warren II.; P                  NDY                30
    ## 37    10.1111/1365-2745.12281 2014 Shefferson; Warren II.; P                  NDY                31
    ## 38    10.1111/1365-2745.12281 2014 Shefferson; Warren II.; P                  NDY                32
    ## 39    10.1111/1365-2745.12281 2014 Shefferson; Warren II.; P                  NDY                33
    ## 40    10.1111/1365-2745.12281 2014 Shefferson; Warren II.; P                  NDY                34
    ## 41    10.1111/1365-2745.12281 2014 Shefferson; Warren II.; P                  NDY                35
    ## 42    10.1111/1365-2745.12281 2014 Shefferson; Warren II.; P                  NDY                36
    ## 43    10.1111/1365-2745.12281 2014 Shefferson; Warren II.; P                  NDY                37
    ## 44    10.1111/1365-2745.12281 2014 Shefferson; Warren II.; P                  NDY                38
    ## 45    10.1111/1365-2745.12281 2014 Shefferson; Warren II.; P                  NDY                39
    ## 46    10.1111/1365-2745.12281 2014 Shefferson; Warren II.; P                  NDY                40
    ## 47    10.1111/1365-2745.12281 2014 Shefferson; Warren II.; P                  NDY                41
    ## 48    10.1111/1365-2745.12281 2014 Shefferson; Warren II.; P                  NDY                42
    ## 49    10.1111/1365-2745.12281 2014 Shefferson; Warren II.; P                  NDY                43
    ## 50    10.1111/1365-2745.12281 2014 Shefferson; Warren II.; P                  NDY                44
    ## 51    10.1111/1365-2745.12281 2014 Shefferson; Warren II.; P                  NDY                45
    ## 52    10.1111/1365-2745.12281 2014 Shefferson; Warren II.; P                  NDY                46
    ## 53                       <NA> 2016 Stas; Langbroek; Bitariho                  NDY                 1
    ## 54                       <NA> 2016 Stas; Langbroek; Bitariho                  NDY                 2
    ## 55                       <NA> 2016 Stas; Langbroek; Bitariho                  NDY                 3
    ## 56                       <NA> 2016 Stas; Langbroek; Bitariho                  NDY                 4
    ## 57                       <NA> 2016 Stas; Langbroek; Bitariho                  NDY                 5
    ## 58                       <NA> 2016 Stas; Langbroek; Bitariho                  NDY                 6
    ## 59 10.1890/0012-9623-93.2.173 2012   Caplat; Nothan; Buckley                  NDY                 1
    ## 60 10.1890/0012-9623-93.2.173 2012   Caplat; Nothan; Buckley                  NDY                 2
    ## 61 10.1890/0012-9623-93.2.173 2012   Caplat; Nothan; Buckley                  NDY                 3
    ## 62 10.1890/0012-9623-93.2.173 2012   Caplat; Nothan; Buckley                  NDY                 4
