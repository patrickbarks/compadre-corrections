COM(P)ADRE Corrections
================
Patrick Barks
2018-01-25

 

### Preliminaries

``` r
# load libraries
library(dplyr)
library(tidyr)
library(tibble)
library(purrr)

# load Comadre and Compadre databases
load('COMADRE_v.X.X.X.RData')
load('COMPADRE_v.X.X.X.RData')

# add row ids to metadata
comadre$metadata$Row <- 1:nrow(comadre$metadata)
compadre$metadata$Row <- 1:nrow(compadre$metadata)

# increase console width
options(width = 120)

# function to limit column width, for prettier Markdown document
Subs <- function(x, n) {
 ifelse(nchar(x) < n, x, paste0(substr(x, 1, n-2), "\u2026")) 
}
```

 

### `DOI.ISBN` with &gt;1 unique `Authors`, `YearPublication`, or `Journal`

A given DOI or ISBN should be associated with a single publication (e.g a single journal, year, and set of authors). The code chunk below finds instances where this is not the case. (Note: some of these cases are due to subtle differences in spelling or formatting of author names.)

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
  select(DOI.ISBN, Journal, Year = YearPublication, Authors) %>% 
  unique() %>% 
  arrange(DOI.ISBN, Authors) %>% 
  print(n = 'all', width = 100)
```

    ## # A tibble: 14 x 4
    ##    DOI.ISBN                  Journal                Year  Authors                                  
    ##    <chr>                     <chr>                  <chr> <chr>                                    
    ##  1 10.1002/jwmg.835          Neu Par Sci Comp       2015  Chiquet; Montgomery; Ma; Ackleh          
    ##  2 10.1002/jwmg.835          J Wildlife Manage      2015  Chitwood; Lashley; Kilgo; Moorman; Deper…
    ##  3 10.1007/s10646-010-0507-y Ecotoxic               2011  Duchet; Coutellec; Franquet; Lagneau; La…
    ##  4 10.1007/s10646-010-0507-y Ecotoxic               2010  Duchet; Coutellec; Franquet; Lagneau; La…
    ##  5 10.1007/s10750-007-9136-8 Hydrobio               2008  Pardo; Vila; Bustamante                  
    ##  6 10.1007/s10750-007-9136-8 Mar Biol               2001  Prevedelli; Simonini                     
    ##  7 10.1080/20018091094835    Hum & Ecol Risk Assess 2001  Gleason                                  
    ##  8 10.1080/20018091094835    Hum & Ecol Risk Assess 2001  Gleason; Nacci                           
    ##  9 10.2307/1447430           Cons Biol              1993  Congdon; Dunham; van Loben Sels          
    ## 10 10.2307/1447430           Ecol Appl              1994  Crowder; Crouse; Heppell; Martin         
    ## 11 10.2307/1447430           Ecology                1990  Frazer; Gibbons; Greene                  
    ## 12 10.2307/1447430           Wildlife Research      1996  Heppel; Limpus; Crouse; Frazer; Crowder  
    ## 13 10.2307/1447430           Copeia                 1998  Heppell                                  
    ## 14 10.2307/1447430           Herpetologica          1991  Iverson

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
  select(DOI.ISBN, Journal, Year = YearPublication, Authors) %>% 
  unique() %>% 
  arrange(DOI.ISBN, Authors) %>%
  print(n = 'all', width = 100)
```

    ## # A tibble: 32 x 4
    ##    DOI.ISBN                                        Journal           Year  Authors                 
    ##    <chr>                                           <chr>             <chr> <chr>                   
    ##  1 10.1016/j.actao.2007.11.005                     Bas and Appl Ecol 2008  Ramula                  
    ##  2 10.1016/j.actao.2007.11.005                     Acta Oeco         2008  Ramula                  
    ##  3 10.1016/S0006-3207(01)00113-6                   Cons Biol         2003  Garcia                  
    ##  4 10.1016/S0006-3207(01)00113-6                   Biol Cons         2002  García; Guzman; Goñi    
    ##  5 10.1111/j.1365-2664.2005.01100.x                J Appl Ecol       2005  Buckley; Brockerhoff; L…
    ##  6 10.1111/j.1365-2664.2005.01100.x                J Ecol            2005  van Mantgem; Stephenson 
    ##  7 10.1111/j.1365-2745.2007.01298.x                J Ecol            2007  López-Hoffman; Ackerly;…
    ##  8 10.1111/j.1365-2745.2007.01298.x                J Ecol            2007  López-Hoffman; Ackerly;…
    ##  9 10.1111/j.1442-9993.1988.tb00999.x              Aust J Ecol       1988  Bradstock; O'Connell    
    ## 10 10.1111/j.1442-9993.1988.tb00999.x              Aust J Bot        1988  Bradstock; O'Connell    
    ## 11 10.1111/j.1469-8137.1984.tb03596.x              New Phyto         1984  Boorman; Fuller         
    ## 12 10.1111/j.1469-8137.1984.tb03596.x              Ecol Monog        1992  Cochran; Ellner         
    ## 13 10.1111/j.1654-1103.2007.tb02519.x              J Veg Sci         2007  Dostal                  
    ## 14 10.1111/j.1654-1103.2007.tb02519.x              J Veg Sci         2007  Dostál                  
    ## 15 10.1658/1100-9233(2007)18[91:PDOAIP]2.0.CO;2    J Veg Sci         2007  Dostal                  
    ## 16 10.1658/1100-9233(2007)18[91:PDOAIP]2.0.CO;2    J Veg Sci         2007  Dostál                  
    ## 17 10.1890/1051-0761(2006)016[2399:DMISOB]2.0.CO;2 Weed Sci          2006  Davis                   
    ## 18 10.1890/1051-0761(2006)016[2399:DMISOB]2.0.CO;2 Ecol Appl         2006  Davis; Landis; Nuzzo; B…
    ## 19 10.1890/11-1052.1                               Ecology           2012  Ellis                   
    ## 20 10.1890/11-1052.1                               Ecology           2012  Ellis; Williams; Lesica…
    ## 21 10.2307/2404276                                 J Appl Ecol       1993  O'Connor                
    ## 22 10.2307/2404276                                 J Appl Ecol       1992  O'Connor; Pickett       
    ## 23 10.2307/2445615                                 Great Bas Nat     1995  Lesica                  
    ## 24 10.2307/2445615                                 Am J Bot          1995  Lesica; Shelly          
    ## 25 10.3170/2007-8-18342                            J Veg Sci         2008  Tanaka; Shibata; Masaki…
    ## 26 10.3170/2007-8-18342                            J Veg Sci         2008  Tanaka; Shibata; Masaki…
    ## 27 10.3732/ajb.0900351                             Am J Bot          2010  Bucharova; Munzbergova;…
    ## 28 10.3732/ajb.0900351                             Am J Bot          2010  Bucharová; Münzbergová;…
    ## 29 10.3732/ajb.92.12.1987                          Am J Bot          2005  Münzbergova             
    ## 30 10.3732/ajb.92.12.1987                          Am J Bot          2005  Münzbergová             
    ## 31 978-84-8014-746-0                               Book              2009  Iriondo; Albert; Gimene…
    ## 32 978-84-8014-746-0                               Book              2009  Iriondo; Albert; Giméne…

 

### `Authors`x`YearPublication`x`Journal` with &gt;1 unique `DOI.ISBN`

A given combination of author, year, and journal should generally correspond to a single DOI or ISBN (though legitimate exceptions are possible). The code chunk below finds instances where this is not the case.

``` r
# COMADRE
comadre$metadata %>% 
  mutate(AuthorJournalYear = paste(Authors, Journal, YearPublication)) %>% 
  group_by(AuthorJournalYear) %>% 
  mutate(n = length(unique(DOI.ISBN))) %>% 
  ungroup() %>% 
  filter(n > 1) %>% 
  select(Authors, Year = YearPublication, Journal, DOI.ISBN) %>% 
  unique() %>% 
  arrange(Authors) %>% 
  mutate(Authors = Subs(Authors, 30))
```

    ## # A tibble: 10 x 4
    ##    Authors                       Year  Journal              DOI.ISBN                    
    ##    <chr>                         <chr> <chr>                <chr>                       
    ##  1 Cockrell; Sorte               2013  J Exp Mar Bio & Ecol 10.1016/j.jembe.2012.11.009 
    ##  2 Cockrell; Sorte               2013  J Exp Mar Bio & Ecol 10.1016/j.jembe.2012.11.010 
    ##  3 Crowder; Crouse; Heppell; Ma… 1994  Ecol Appl            10.2307/1941948             
    ##  4 Crowder; Crouse; Heppell; Ma… 1994  Ecol Appl            10.2307/1447430             
    ##  5 Cruz; Pech; Seddon; Cleland;… 2013  Biol Cons            10.1016/j.biocon.2013.09.006
    ##  6 Cruz; Pech; Seddon; Cleland;… 2013  Biol Cons            10.1016/j.biocon.2013.09.005
    ##  7 Iverson                       1991  Herpetologica        10.2307/1447430             
    ##  8 Iverson                       1991  Herpetologica        10.2307/1447431             
    ##  9 Tucker                        2001  Book                 978-0-949324-89-4           
    ## 10 Tucker                        2001  Book                 <NA>

``` r
# COMPADRE
compadre$metadata %>% 
  mutate(AuthorJournalYear = paste(Authors, Journal, YearPublication)) %>% 
  group_by(AuthorJournalYear) %>% 
  mutate(n = length(unique(DOI.ISBN))) %>% 
  ungroup() %>% 
  filter(n > 1) %>% 
  select(Authors, Year = YearPublication, Journal, DOI.ISBN) %>% 
  unique() %>% 
  arrange(Authors) %>% 
  mutate(Authors = Subs(Authors, 30))
```

    ## # A tibble: 15 x 4
    ##    Authors                       Year  Journal    DOI.ISBN                                     
    ##    <chr>                         <chr> <chr>      <chr>                                        
    ##  1 Dostal                        2007  J Veg Sci  10.1111/j.1654-1103.2007.tb02519.x           
    ##  2 Dostal                        2007  J Veg Sci  10.1658/1100-9233(2007)18[91:PDOAIP]2.0.CO;2 
    ##  3 Dostál                        2007  J Veg Sci  10.1111/j.1654-1103.2007.tb02519.x           
    ##  4 Dostál                        2007  J Veg Sci  10.1658/1100-9233(2007)18[91:PDOAIP]2.0.CO;2 
    ##  5 Mondragón; Ticktin            2011  Cons Biol  10.1111/j.1523-1739.2011.01691.x             
    ##  6 Mondragón; Ticktin            2011  Cons Biol  <NA>                                         
    ##  7 Nakashizuka                   1991  J Veg Sci  10.1658/1100-9233(2007)18[379:VIJSAR]2.0.CO;2
    ##  8 Nakashizuka                   1991  J Veg Sci  <NA>                                         
    ##  9 Prevéy; Germino; Huntly       2010  Ecol Appl  10.1890/09-0750                              
    ## 10 Prevéy; Germino; Huntly       2010  Ecol Appl  10.1890/09-0750.1                            
    ## 11 Stas; Langbroek; Bitariho; S… 2016  Afr J Ecol 10.1111/aje.12309                            
    ## 12 Stas; Langbroek; Bitariho; S… 2016  Afr J Ecol <NA>                                         
    ## 13 van Mantgem; Stephenson       2005  J Ecol     0.1111/j.1365-2745.2005.01007.x              
    ## 14 van Mantgem; Stephenson       2005  J Ecol     10.1111/j.1365-2745.2005.01007.x             
    ## 15 van Mantgem; Stephenson       2005  J Ecol     10.1111/j.1365-2664.2005.01100.x

 

### DOI not found using CrossRef API

The code chunk below finds DOIs listed in COM(P)ADRE that cannot be found using the [CrossRef API](https://github.com/CrossRef/rest-api-doc) (accessed via the R library [rcrossref](https://github.com/ropensci/rcrossref)).

``` r
# # load rcrossref
# library('rcrossref')
# 
# # function to get citation info for a given DOI, using rcrossref library
# GetCitation <- function(doi) {
#   # get citation info using rcrossref::cr_cn()
#   ref <- try(cr_cn(dois = doi, format = "citeproc-json"))
#   
#   if(is.null(ref) | class(ref) == "try-error") {
#   # if no citation found, return NAs
#     out <- data.frame(rcross_title = NA,
#                       rcross_author = NA,
#                       rcross_found = FALSE,
#                       stringsAsFactors = F)
#   } else {
#   # else, return title and author info (or NA if those elements not found)
#     rcross_title <- try(ref$`container-title`)
#     rcross_author <- try(ref$author$family[1])
#     out <- data.frame(
#       rcross_title = ifelse(is.null(rcross_title), NA, rcross_title),
#       rcross_author = ifelse(is.null(rcross_author), NA, rcross_author),
#       rcross_found = TRUE,
#       stringsAsFactors = F
#     )
#   }
#   
#   return(out)
# }
# 
# # get COMADRE citations
# citation_comadre <- tibble(DOI.ISBN = sort(unique(comadre$metadata$DOI.ISBN))) %>% 
#   slice(grep('^10', DOI.ISBN)) %>% # limit to DOIs, which start with "10."
#   group_by(DOI.ISBN) %>% 
#   do(GetCitation(.$DOI.ISBN)) %>% 
#   ungroup()
# 
# # get COMPADRE citations
# citation_compadre <- tibble(DOI.ISBN = sort(unique(compadre$metadata$DOI.ISBN))) %>% 
#   slice(grep('^10', DOI.ISBN)) %>%  # limit to DOIs, which start with "10."
#   group_by(DOI.ISBN) %>% 
#   do(GetCitation(.$DOI.ISBN)) %>% 
#   ungroup()
# 
# # write to file
# write.csv(citation_comadre, 'crossref_comadre.csv', row.names = F)
# write.csv(citation_compadre, 'crossref_compadre.csv', row.names = F)
```

``` r
# read citation data already written to file
citation_comadre <- read.csv('crossref_comadre.csv', stringsAsFactors = F)
citation_compadre <- read.csv('crossref_compadre.csv', stringsAsFactors = F)
```

``` r
# COMADRE
comadre$metadata %>% 
  select(DOI.ISBN, Journal, Year = YearPublication, Authors) %>% 
  unique() %>% 
  slice(grep('^10', DOI.ISBN)) %>% 
  left_join(citation_comadre, by = 'DOI.ISBN') %>% 
  filter(rcross_found == FALSE) %>% 
  select(-rcross_author, -rcross_title) %>% 
  mutate(Authors = Subs(Authors, 30))
```

    ## # A tibble: 10 x 5
    ##    DOI.ISBN                         Journal         Year  Authors                       rcross_found
    ##    <chr>                            <chr>           <chr> <chr>                         <lgl>       
    ##  1 10.1111/j.1523-1739.2006.00300.x JER             2016  Lance; Alonzo; Garcia-Sanche… F           
    ##  2 10.1007/s00338-015-1341-8)       Coral Reef      2015  Mercado-Molina; Ruiz-Diaz; P… F           
    ##  3 10.2307/3803419                  J Wild Manag    2006  Fefferman; Reed               F           
    ##  4 10.2307/3830713                  Wildlife Monogr 1991  Ballard; Whitman; Reed        F           
    ##  5 10.1007/s10144-003-0139-7        Popul Ecol      2003  Johannesen; Aars; Andreassen… F           
    ##  6 10.2307/4314005                  Ambio           1992  Heide-Jørgensen; Härkönen; A… F           
    ##  7 10.1890/15-0317.1                Ecol            2016  Bjorkvoll; Lee; Grotan; Saet… F           
    ##  8 10.7936/K7DB7ZTD                 PhD Thesis      2013  Watsa                         F           
    ##  9 10.2307/20113253                 Ecol Bull       2000  Berglind                      F           
    ## 10 10.1007/s10646-014-1267-x)       Ecotoxicology   2014  Li; Ju; Liao; Liao            F

``` r
# COMPADRE
compadre$metadata %>% 
  select(DOI.ISBN, Journal, Year = YearPublication, Authors) %>% 
  unique() %>% 
  slice(grep('^10', DOI.ISBN)) %>% 
  left_join(citation_compadre, by = 'DOI.ISBN') %>% 
  filter(rcross_found == FALSE) %>% 
  select(-rcross_author, -rcross_title) %>% 
  mutate(Authors = Subs(Authors, 30))
```

    ## # A tibble: 3 x 5
    ##   DOI.ISBN                   Journal    Year  Authors                       rcross_found
    ##   <chr>                      <chr>      <chr> <chr>                         <lgl>       
    ## 1 10.1007/s00442-009-1354-6  Oeco       2009  Schutzenhofer; Valone; Knight F           
    ## 2 10.1007/sl0l44-005-0238-8  Popul Ecol 2006  Gross; Morris; Wolosin; Doak  F           
    ## 3 10.1007/sl 1258-008-9460-8 PhD thesis 2009  Schmucki                      F

 

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
  select(DOI.ISBN, Journal, Year = YearPublication, Authors) %>% 
  unique() %>% 
  mutate(irreg = grepl(irregular, Authors)) %>% 
  filter(irreg == TRUE) %>% 
  select(-irreg) %>% 
  arrange(Authors) %>% 
  as_tibble() %>% 
  print(width = 100)
```

    ## # A tibble: 0 x 4
    ## # ... with 4 variables: DOI.ISBN <chr>, Journal <chr>, Year <chr>, Authors <chr>

``` r
# COMPADRE
compadre$metadata %>% 
  select(DOI.ISBN, Journal, Year = YearPublication, Authors) %>% 
  unique() %>% 
  mutate(irreg = grepl(irregular, Authors)) %>% 
  filter(irreg == TRUE) %>% 
  select(-irreg) %>% 
  arrange(Authors) %>% 
  as_tibble() %>% 
  print(width = 100)
```

    ## # A tibble: 11 x 4
    ##    DOI.ISBN                         Journal      Year  Authors                                     
    ##    <chr>                            <chr>        <chr> <chr>                                       
    ##  1 10.1007/s00442-014-3112-6        Oecologia    2014  Altwegg; De Klerk;  Midgley                 
    ##  2 10.1017/S0266467400001620        J Trop Ecol  2000  Barot;  Gignoux; Vuattoux                   
    ##  3 <NA>                             Report       1998  Berlin, Miller, Borovansky, Seal, and Byers 
    ##  4 <NA>                             Cact Suc Mex 2013  Demeneghi-Calatayud;Navarro-Carbajal;Saldiv…
    ##  5 10.1111/1365-2745.12140          J Ecol       2013  Gaoue;  Horvitz; Ticktin; Steiner; Tuljapur…
    ##  6 10.1111/j.1365-2745.2007.01298.x J Ecol       2007  López-Hoffman; Ackerly; Anten; Denoyer;Ramos
    ##  7 10.1111/j.1365-2745.2007.01298.x J Ecol       2007  López-Hoffman; Ackerly; Anten; DeNoyer;Ramos
    ##  8 <NA>                             Cact Suc Mex 2012  Saldivar Sánchez, Navarro Carbajal          
    ##  9 10.1111/j.1365-2745.2004.00844.x J Ecol       2004  Stokes; Bullok;Watkinson                    
    ## 10 10.1111/j.1529-8817.2010.00924.x J Phycol     2010  Vieira, Santos                              
    ## 11 10.1017/S0266467409990459        J Trop Ecol  2010  Wester, Zotz

 

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
SplitCountries <- function(x) {
  data.frame(Country = strsplit(as.character(x), '; ')[[1]], stringsAsFactors = F)
}
```

``` r
# COMADRE
comadre$metadata %>% 
  group_by(Row) %>% 
  do(SplitCountries(.$Country)) %>% 
  ungroup() %>% 
  left_join(select(comadre$metadata, Row, DOI.ISBN, Journal, Authors), by = 'Row') %>% 
  select(-Row) %>% 
  filter(!Country %in% country_codes$ISO, !is.na(Country)) %>% 
  unique() %>% 
  print(width = 100)
```

    ## # A tibble: 13 x 4
    ##    Country    DOI.ISBN                        Journal                       Authors                
    ##    <chr>      <chr>                           <chr>                         <chr>                  
    ##  1 ANT        10.1111/1365-2656.12399         J Anim Ecol                   O'Farrell; Salguero-Gó…
    ##  2 3          10.1016/j.ecoenv.2012.01.019    Ecotoxicol Environ Safety     Bergek; Ma; Vetemaa; F…
    ##  3 1          10.1016/j.ecoenv.2012.01.019    Ecotoxicol Environ Safety     Bergek; Ma; Vetemaa; F…
    ##  4 1a         10.1016/j.ecoenv.2012.01.019    Ecotoxicol Environ Safety     Bergek; Ma; Vetemaa; F…
    ##  5 1b         10.1016/j.ecoenv.2012.01.019    Ecotoxicol Environ Safety     Bergek; Ma; Vetemaa; F…
    ##  6 LAB        10.1007/s11270-014-2207-3       Water Air Soil Pollut         Santadino; Coviella; M…
    ##  7 Simulation 10.1016/j.ecolmodel.2012.02.018 Ecol Model                    Lesnoff; Corniaux; Hie…
    ##  8 1          10.1111/nrm.12034               Nat Res Mod                   Holma; Lindroos; Oinon…
    ##  9 57         978-1-100-23563-9               Can Tech Report Fish & Aq Sci Vélez-Espino; Ford; Ar…
    ## 10 59         978-1-100-23563-9               Can Tech Report Fish & Aq Sci Vélez-Espino; Ford; Ar…
    ## 11 NODC-27    10.3354/esr00657                Endangered Spp Res            Whitehead; Gero        
    ## 12 27         <NA>                            NMFS                          Richards               
    ## 13 23         <NA>                            NMFS                          Richards

``` r
# COMPADRE
compadre$metadata %>% 
  group_by(Row) %>% 
  do(SplitCountries(.$Country)) %>% 
  ungroup() %>% 
  left_join(select(compadre$metadata, Row, DOI.ISBN, Journal, Authors), by = 'Row') %>% 
  select(-Row) %>% 
  filter(!Country %in% country_codes$ISO, !is.na(Country)) %>% 
  unique() %>%
  print(width = 100)
```

    ## # A tibble: 2 x 4
    ##   Country DOI.ISBN                         Journal   Authors                                       
    ##   <chr>   <chr>                            <chr>     <chr>                                         
    ## 1 NL      10.1111/j.1365-3180.2010.00787.x Weed Res  van den Berg; Gilligan; Gerdessen; Gregoire; …
    ## 2 LAB     10.1890/07-0568.1                Ecol Appl Burns

 

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
  select(-flag, -MatrixClassAuthor) %>% 
  mutate(Authors = Subs(Authors, 30)) %>%  
  mutate(MatrixClassOrganized = Subs(MatrixClassOrganized, 20)) %>% 
  print(n = 'all')
```

    ## # A tibble: 34 x 5
    ##    DOI.ISBN                Year  Authors                       MatrixClassOrganized MatrixClassNumber
    ##    <chr>                   <chr> <chr>                         <chr>                            <dbl>
    ##  1 10.1080/20018091094835  2001  Gleason; Nacci                active                           1.00 
    ##  2 10.1080/20018091094835  2001  Gleason; Nacci                active                           2.00 
    ##  3 10.1080/20018091094835  2001  Gleason; Nacci                active                           3.00 
    ##  4 10.1080/20018091094835  2001  Gleason; Nacci                active                           4.00 
    ##  5 10.1080/20018091094835  2001  Gleason; Nacci                First year (nestli…              0    
    ##  6 10.1080/20018091094835  2001  Gleason; Nacci                Second year (juven…              0.500
    ##  7 10.1080/20018091094835  2001  Gleason; Nacci                Third year and onw…              0    
    ##  8 10.1080/20018091094835  2001  Gleason; Nacci                First year (nestli…              0    
    ##  9 10.1080/20018091094835  2001  Gleason; Nacci                Second year (juven…              0.500
    ## 10 10.1080/20018091094835  2001  Gleason; Nacci                Third year and onw…              0    
    ## 11 10.1080/20018091094835  2001  Gleason; Nacci                First year (nestli…              0    
    ## 12 10.1080/20018091094835  2001  Gleason; Nacci                Second year (juven…              0.500
    ## 13 10.1080/20018091094835  2001  Gleason; Nacci                Third year and onw…              0    
    ## 14 10.1111/1365-2664.12476 2015  Chevallier; Hernandez-Matias… active                           1.00 
    ## 15 10.1111/1365-2664.12476 2015  Chevallier; Hernandez-Matias… active                           2.00 
    ## 16 10.1111/1365-2664.12476 2015  Chevallier; Hernandez-Matias… active                           3.00 
    ## 17 10.1111/1365-2664.12476 2015  Chevallier; Hernandez-Matias… Chicks: 0-1 years                0    
    ## 18 10.1111/1365-2664.12476 2015  Chevallier; Hernandez-Matias… Juveniles: 2-5 yea…              0    
    ## 19 10.1111/1365-2664.12476 2015  Chevallier; Hernandez-Matias… Adults: 5-40 years               0    
    ## 20 10.1111/jofo.12093      2015  Mumme                         active                           1.00 
    ## 21 10.1111/jofo.12093      2015  Mumme                         active                           2.00 
    ## 22 10.1111/jofo.12093      2015  Mumme                         active                           3.00 
    ## 23 10.1111/jofo.12093      2015  Mumme                         Juvenile                         0    
    ## 24 10.1111/jofo.12093      2015  Mumme                         Adults                           0.430
    ## 25 10.1111/jofo.12093      2015  Mumme                         Juvenile                         0    
    ## 26 10.1111/jofo.12093      2015  Mumme                         Adults                           0.360
    ## 27 10.1111/jofo.12093      2015  Mumme                         Juvenile                         0    
    ## 28 10.1111/jofo.12093      2015  Mumme                         Adults                           0.530
    ## 29 10.1111/jofo.12093      2015  Mumme                         Juvenile                         0    
    ## 30 10.1111/jofo.12093      2015  Mumme                         Adults                           0.510
    ## 31 10.1111/jofo.12093      2015  Mumme                         Juvenile                         0    
    ## 32 10.1111/jofo.12093      2015  Mumme                         Adults                           0.330
    ## 33 10.1111/jofo.12093      2015  Mumme                         Juvenile                         0    
    ## 34 10.1111/jofo.12093      2015  Mumme                         Adults                           0.430

``` r
# COMPADRE
tibble(DOI.ISBN = compadre$metadata$DOI.ISBN,
       Year = compadre$metadata$YearPublication,
       Authors = compadre$metadata$Authors,
       matrixClass = compadre$matrixClass) %>%
  mutate(flag = map_lgl(matrixClass, MatrixClassCheck)) %>% 
  filter(flag == TRUE) %>% 
  unnest() %>% 
  select(-flag, -MatrixClassAuthor) %>% 
  mutate(Authors = Subs(Authors, 30)) %>% 
  mutate(MatrixClassOrganized = Subs(MatrixClassOrganized, 20)) %>% 
  print(n = 'all')
```

    ## # A tibble: 62 x 5
    ##    DOI.ISBN                   Year  Authors                       MatrixClassOrganized MatrixClassNumber
    ##    <chr>                      <chr> <chr>                         <chr>                            <dbl>
    ##  1 10.2307/2261637            1995  Okland                        NDY                               1.00
    ##  2 10.2307/2261637            1995  Okland                        <NA>                              2.00
    ##  3 10.2307/2261637            1995  Okland                        <NA>                              3.00
    ##  4 10.2307/2261637            1995  Okland                        <NA>                              4.00
    ##  5 10.2307/2261637            1995  Okland                        <NA>                              5.00
    ##  6 10.2307/2261637            1995  Okland                        <NA>                              6.00
    ##  7 10.1111/1365-2745.12281    2014  Shefferson; Warren II.; Pull… NDY                               1.00
    ##  8 10.1111/1365-2745.12281    2014  Shefferson; Warren II.; Pull… NDY                               2.00
    ##  9 10.1111/1365-2745.12281    2014  Shefferson; Warren II.; Pull… NDY                               3.00
    ## 10 10.1111/1365-2745.12281    2014  Shefferson; Warren II.; Pull… NDY                               4.00
    ## 11 10.1111/1365-2745.12281    2014  Shefferson; Warren II.; Pull… NDY                               5.00
    ## 12 10.1111/1365-2745.12281    2014  Shefferson; Warren II.; Pull… NDY                               6.00
    ## 13 10.1111/1365-2745.12281    2014  Shefferson; Warren II.; Pull… NDY                               7.00
    ## 14 10.1111/1365-2745.12281    2014  Shefferson; Warren II.; Pull… NDY                               8.00
    ## 15 10.1111/1365-2745.12281    2014  Shefferson; Warren II.; Pull… NDY                               9.00
    ## 16 10.1111/1365-2745.12281    2014  Shefferson; Warren II.; Pull… NDY                              10.0 
    ## 17 10.1111/1365-2745.12281    2014  Shefferson; Warren II.; Pull… NDY                              11.0 
    ## 18 10.1111/1365-2745.12281    2014  Shefferson; Warren II.; Pull… NDY                              12.0 
    ## 19 10.1111/1365-2745.12281    2014  Shefferson; Warren II.; Pull… NDY                              13.0 
    ## 20 10.1111/1365-2745.12281    2014  Shefferson; Warren II.; Pull… NDY                              14.0 
    ## 21 10.1111/1365-2745.12281    2014  Shefferson; Warren II.; Pull… NDY                              15.0 
    ## 22 10.1111/1365-2745.12281    2014  Shefferson; Warren II.; Pull… NDY                              16.0 
    ## 23 10.1111/1365-2745.12281    2014  Shefferson; Warren II.; Pull… NDY                              17.0 
    ## 24 10.1111/1365-2745.12281    2014  Shefferson; Warren II.; Pull… NDY                              18.0 
    ## 25 10.1111/1365-2745.12281    2014  Shefferson; Warren II.; Pull… NDY                              19.0 
    ## 26 10.1111/1365-2745.12281    2014  Shefferson; Warren II.; Pull… NDY                              20.0 
    ## 27 10.1111/1365-2745.12281    2014  Shefferson; Warren II.; Pull… NDY                              21.0 
    ## 28 10.1111/1365-2745.12281    2014  Shefferson; Warren II.; Pull… NDY                              22.0 
    ## 29 10.1111/1365-2745.12281    2014  Shefferson; Warren II.; Pull… NDY                              23.0 
    ## 30 10.1111/1365-2745.12281    2014  Shefferson; Warren II.; Pull… NDY                              24.0 
    ## 31 10.1111/1365-2745.12281    2014  Shefferson; Warren II.; Pull… NDY                              25.0 
    ## 32 10.1111/1365-2745.12281    2014  Shefferson; Warren II.; Pull… NDY                              26.0 
    ## 33 10.1111/1365-2745.12281    2014  Shefferson; Warren II.; Pull… NDY                              27.0 
    ## 34 10.1111/1365-2745.12281    2014  Shefferson; Warren II.; Pull… NDY                              28.0 
    ## 35 10.1111/1365-2745.12281    2014  Shefferson; Warren II.; Pull… NDY                              29.0 
    ## 36 10.1111/1365-2745.12281    2014  Shefferson; Warren II.; Pull… NDY                              30.0 
    ## 37 10.1111/1365-2745.12281    2014  Shefferson; Warren II.; Pull… NDY                              31.0 
    ## 38 10.1111/1365-2745.12281    2014  Shefferson; Warren II.; Pull… NDY                              32.0 
    ## 39 10.1111/1365-2745.12281    2014  Shefferson; Warren II.; Pull… NDY                              33.0 
    ## 40 10.1111/1365-2745.12281    2014  Shefferson; Warren II.; Pull… NDY                              34.0 
    ## 41 10.1111/1365-2745.12281    2014  Shefferson; Warren II.; Pull… NDY                              35.0 
    ## 42 10.1111/1365-2745.12281    2014  Shefferson; Warren II.; Pull… NDY                              36.0 
    ## 43 10.1111/1365-2745.12281    2014  Shefferson; Warren II.; Pull… NDY                              37.0 
    ## 44 10.1111/1365-2745.12281    2014  Shefferson; Warren II.; Pull… NDY                              38.0 
    ## 45 10.1111/1365-2745.12281    2014  Shefferson; Warren II.; Pull… NDY                              39.0 
    ## 46 10.1111/1365-2745.12281    2014  Shefferson; Warren II.; Pull… NDY                              40.0 
    ## 47 10.1111/1365-2745.12281    2014  Shefferson; Warren II.; Pull… NDY                              41.0 
    ## 48 10.1111/1365-2745.12281    2014  Shefferson; Warren II.; Pull… NDY                              42.0 
    ## 49 10.1111/1365-2745.12281    2014  Shefferson; Warren II.; Pull… NDY                              43.0 
    ## 50 10.1111/1365-2745.12281    2014  Shefferson; Warren II.; Pull… NDY                              44.0 
    ## 51 10.1111/1365-2745.12281    2014  Shefferson; Warren II.; Pull… NDY                              45.0 
    ## 52 10.1111/1365-2745.12281    2014  Shefferson; Warren II.; Pull… NDY                              46.0 
    ## 53 <NA>                       2016  Stas; Langbroek; Bitariho; S… NDY                               1.00
    ## 54 <NA>                       2016  Stas; Langbroek; Bitariho; S… NDY                               2.00
    ## 55 <NA>                       2016  Stas; Langbroek; Bitariho; S… NDY                               3.00
    ## 56 <NA>                       2016  Stas; Langbroek; Bitariho; S… NDY                               4.00
    ## 57 <NA>                       2016  Stas; Langbroek; Bitariho; S… NDY                               5.00
    ## 58 <NA>                       2016  Stas; Langbroek; Bitariho; S… NDY                               6.00
    ## 59 10.1890/0012-9623-93.2.173 2012  Caplat; Nothan; Buckley       NDY                               1.00
    ## 60 10.1890/0012-9623-93.2.173 2012  Caplat; Nothan; Buckley       NDY                               2.00
    ## 61 10.1890/0012-9623-93.2.173 2012  Caplat; Nothan; Buckley       NDY                               3.00
    ## 62 10.1890/0012-9623-93.2.173 2012  Caplat; Nothan; Buckley       NDY                               4.00
