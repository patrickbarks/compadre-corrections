matA != (matU + matF + matC)
================
Patrick Barks
2018-02-28

 

### Preliminaries

``` r
# load libraries
library(tibble)
library(dplyr)
library(Rcompadre)

# load Comadre and Compadre databases
load('COMADRE_v.X.X.X.RData')
load('COMPADRE_v.X.X.X.RData')

# add row ids to metadata
comadre$metadata$id <- 1:nrow(comadre$metadata)
compadre$metadata$id <- 1:nrow(compadre$metadata)

# increase console width
options(width = 100)

# function to add three matrices (replacing NA with 0)
AddMats <- function(M1, M2, M3) {
  M1[which(is.na(M1))] <- 0
  M2[which(is.na(M2))] <- 0
  M3[which(is.na(M3))] <- 0
  out <- M1 + M2 + M3
  colnames(out) <- NULL
  return(out)
}

# function to test whether all elements of two matrices equal
CompareMats <- function(A, B, prec = 0.000001) {
  return(all(abs(A - B) < prec))
}
```

### Compadre

``` r
# subset to divided matrices
compadre_div <- subsetDB(compadre, MatrixSplit == 'Divided')

# create separate lists for mat A, U, F, C
matA <- lapply(compadre_div$mat, function(x) x$matA)
matU <- lapply(compadre_div$mat, function(x) x$matU)
matF <- lapply(compadre_div$mat, function(x) x$matF)
matC <- lapply(compadre_div$mat, function(x) x$matC)

# matU + matF + matC
sumUFC <- mapply(AddMats, matU, matF, matC, USE.NAMES = FALSE)

# compare matA to (matU + matF + matC)
mat_compare <- mapply(CompareMats, matA, sumUFC, USE.NAMES = FALSE)
mat_mismatch <- which(mat_compare == FALSE)

# which matA != (matU + matF + matC)
mat_mismatch
```

    ##   [1]   24   26   33   34  244  493  546  563  567  726  727  728  729  730  731  732 1063 1279 1290
    ##  [20] 1394 1395 1535 1536 1537 1538 1563 1722 1727 1907 1945 2221 2223 2240 2247 2251 2532 2533 2534
    ##  [39] 2535 2763 2773 2775 2792 2793 2803 2804 2847 2848 2859 2860 2861 2862 2863 2864 2865 2866 2867
    ##  [58] 2868 2886 2887 2888 2889 2890 2891 2892 2893 2894 2895 2896 2897 2898 2899 2900 2901 2902 2903
    ##  [77] 2904 2905 2906 2907 2908 2909 2910 2911 2912 2913 2917 2918 2919 2920 2921 2922 2923 2924 2925
    ##  [96] 2929 2963 2968 3620 3679 3680 3681 3682 3733 3734 3735 3736 3737 3738 3739 3740 3741 3742 4125
    ## [115] 4126 4357 4512 4639 4704 4887 4903 4924 4950 5013 5150 5241 5347 5402 5409 5424 5425 5436 5635
    ## [134] 5651 5682 5819 5820 5821 5822 5823 5824 5825 5826 5827 5829 5850 5894 5900 5903 5910 5911 5912
    ## [153] 5955 5978 6035 6131 6189 6202 6232 6233 6243 6259 6260 6261 6268 6271 6369 6376 6377 6378 6379
    ## [172] 6380 6381 6382 6383 6384 6385 6386 6387 6388 6389 6390 6391 6392 6472 6604 6605 6625 6627 6732
    ## [191] 6752 6754 6862 6995 6996 7067

Take a closer look at some of the mismatches:

``` r
# choose index of non-matching matrix
i <- mat_mismatch[161]

# see which transitions don't match (indicated with FALSE)
abs(matA[[i]] - sumUFC[[i]]) < 0.000001
```

    ##        A1   A2    A3    A4    A5
    ## [1,] TRUE TRUE FALSE FALSE FALSE
    ## [2,] TRUE TRUE  TRUE FALSE FALSE
    ## [3,] TRUE TRUE  TRUE  TRUE  TRUE
    ## [4,] TRUE TRUE  TRUE  TRUE  TRUE
    ## [5,] TRUE TRUE  TRUE  TRUE  TRUE

``` r
# compare matA to (matU + matF + matC)
matA[[i]]; sumUFC[[i]]
```

    ##              A1         A2         A3        A4         A5
    ## [1,] 0.03173333 0.09828333 0.07266667 0.2054833 0.02778333
    ## [2,] 0.70628333 0.54065000 0.37898333 0.7213333 0.70290000
    ## [3,] 0.03968333 0.17085000 0.27238333 0.1729000 0.07728333
    ## [4,] 0.00000000 0.03683333 0.13741667 0.2964333 0.12016667
    ## [5,] 0.00000000 0.03108333 0.03713333 0.1137500 0.42235000

    ##            [,1]       [,2]       [,3]      [,4]       [,5]
    ## [1,] 0.03173333 0.09828333 0.14533333 0.4109667 0.05556667
    ## [2,] 0.70628333 0.54065000 0.37898333 1.4426667 1.40580000
    ## [3,] 0.03968333 0.17085000 0.27238333 0.1729000 0.07728333
    ## [4,] 0.00000000 0.03683333 0.13741667 0.2964333 0.12016667
    ## [5,] 0.00000000 0.03108333 0.03713333 0.1137500 0.42235000

In this case, it looks like fecundity transitions from the top two rows have been duplicated in matU.

``` r
# check individual U and F (and C, if applicable)
matU[[i]]; matF[[i]]
```

    ##              U1         U2         U3        U4         U5
    ## [1,] 0.03173333 0.09828333 0.07266667 0.2054833 0.02778333
    ## [2,] 0.70628333 0.54065000 0.37898333 0.7213333 0.70290000
    ## [3,] 0.03968333 0.17085000 0.27238333 0.1729000 0.07728333
    ## [4,] 0.00000000 0.03683333 0.13741667 0.2964333 0.12016667
    ## [5,] 0.00000000 0.03108333 0.03713333 0.1137500 0.42235000

    ##      F1 F2         F3        F4         F5
    ## [1,]  0  0 0.07266667 0.2054833 0.02778333
    ## [2,]  0  0 0.00000000 0.7213333 0.70290000
    ## [3,]  0  0 0.00000000 0.0000000 0.00000000
    ## [4,]  0  0 0.00000000 0.0000000 0.00000000
    ## [5,]  0  0 0.00000000 0.0000000 0.00000000

Print metadata for all non-matching matrices:

``` r
mismatch_compadre <- compadre_div$metadata %>% 
  slice(mat_mismatch) %>% 
  dplyr::select(-id)

write.csv(mismatch_compadre, 'mismatch_matA_compadre.csv', row.names = F)
```

### Comadre

``` r
# subset to divided matrices
comadre_div <- subsetDB(comadre, MatrixSplit == 'Divided')

# create separate lists for mat A, U, F, C
matA <- lapply(comadre_div$mat, function(x) x$matA)
matU <- lapply(comadre_div$mat, function(x) x$matU)
matF <- lapply(comadre_div$mat, function(x) x$matF)
matC <- lapply(comadre_div$mat, function(x) x$matC)

# matU + matF + matC
sumUFC <- mapply(AddMats, matU, matF, matC, USE.NAMES = FALSE)

# compare matA to (matU + matF + matC)
mat_compare <- mapply(CompareMats, matA, sumUFC, USE.NAMES = FALSE)
mat_mismatch <- which(mat_compare == FALSE)

# which matA != (matU + matF + matC)
mat_mismatch
```

    ##  [1]   92  129  224  225  226  233  290  327  600  758  790  825  826  831  836  837  927  928  990
    ## [20] 1173 1174 1175 1176 1177 1178 1179 1180 1181 1182 1183 1184 1185 1186 1187 1188 1189 1190 1191
    ## [39] 1192 1193 1194 1691 1983 1990

We can take a closer look at some of the mismatches:

``` r
# choose index of non-matching matrix
i <- mat_mismatch[3]

# see which transitions don't match (indicated with FALSE)
abs(matA[[i]] - sumUFC[[i]]) < 0.000001
```

    ##         A1    A2    A3    A4
    ## [1,] FALSE FALSE FALSE FALSE
    ## [2,] FALSE FALSE FALSE FALSE
    ## [3,]  TRUE FALSE FALSE FALSE
    ## [4,]  TRUE  TRUE FALSE FALSE

``` r
# compare matA to (matU + matF + matC)
matA[[i]]; sumUFC[[i]]
```

    ##         A1    A2    A3    A4
    ## [1,] 0.382 0.064 0.022 0.125
    ## [2,] 0.393 0.670 0.333 0.063
    ## [3,] 0.000 0.170 0.622 0.188
    ## [4,] 0.000 0.000 0.178 0.688

    ##      [,1] [,2] [,3] [,4]
    ## [1,]    0    0    0    0
    ## [2,]    0    0    0    0
    ## [3,]    0    0    0    0
    ## [4,]    0    0    0    0

In this case, the U, F, and C matrices contain all zeros, even though MatrixSplit == 'Divided'.

``` r
# check individual U, F, and C matrices
matU[[i]]; matF[[i]]; matC[[i]]
```

    ##      U1 U2 U3 U4
    ## [1,]  0  0  0  0
    ## [2,]  0  0  0  0
    ## [3,]  0  0  0  0
    ## [4,]  0  0  0  0

    ##      F1 F2 F3 F4
    ## [1,]  0  0  0  0
    ## [2,]  0  0  0  0
    ## [3,]  0  0  0  0
    ## [4,]  0  0  0  0

    ##      C1 C2 C3 C4
    ## [1,]  0  0  0  0
    ## [2,]  0  0  0  0
    ## [3,]  0  0  0  0
    ## [4,]  0  0  0  0

Print metadata for all non-matching matrices:

``` r
mismatch_comadre <- comadre_div$metadata %>% 
  slice(mat_mismatch) %>% 
  dplyr::select(-id)

write.csv(mismatch_comadre, 'mismatch_matA_comadre.csv', row.names = F)
```
