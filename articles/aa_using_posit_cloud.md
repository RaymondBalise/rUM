# 1. Using rUM on Posit.Cloud or with Positron

## Introduction

rUM has many tools to help you write papers and reports efficiently.
They ***will*** work if you are not working in the RStudio Integrated
Development Environment (IDE) on a Mac or Windows but some of the menu
options that we show in the other vignettes will not be available. So
you will need to type a line or two into the R console instead of
clicking on menu options.

Here we explain how to use rUM on Posit.Cloud (or other IDEs like
Positron).

## rUM on Posit.Cloud

The other vignettes for `rUM` show you that it integrates with the
RStudio IDE menus. Unfortunately, on Posit.Cloud the **New Project**
button does not know about `rUM`. So you will need to type a command to
create a new `rUM` based project. Because Posit.Cloud treats each
project as its own walled-off environment, you need to tell `rUM` to
only work in the current folder/directory. Most of rUM’s functions will
do that automatically. However, `rUM`’s primary function
[`make_project()`](https://raymondbalise.github.io/rUM/reference/make_project.md)
needs you to do this by specifying the location for the project to be
`"./"` (that is UNIX code shorthand that means “in this
folder/directory”). Because Posit.Cloud projects have a
**project.Rproj** file, you will also need to add the `overwrite = TRUE`
option to tell rUM to overwrite the default Posit.Cloud project file.
Here is an example:

    rUM::make_project("./", overwrite = TRUE)

If you want to make a new project that will be a package with a vignette
written with Quarto, use code like this:

    rUM::make_package('./', overwrite = TRUE) 

After typing that in the console and pushing return/enter on your
keyboard, you will need to confirm that you really want to make a new
project. Choose the option that means **Yes** when RStudio asks you to
confirm that you want to overwrite the existing project and namespace
files. Say **Yes** when it asks you if you want to quit and **Switch
Projects**.

### Session

If you are new to R, ignore this.

``` r
sessionInfo()
#> R version 4.5.2 (2025-10-31)
#> Platform: x86_64-pc-linux-gnu
#> Running under: Ubuntu 24.04.3 LTS
#> 
#> Matrix products: default
#> BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
#> LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0
#> 
#> locale:
#>  [1] LC_CTYPE=C.UTF-8       LC_NUMERIC=C           LC_TIME=C.UTF-8       
#>  [4] LC_COLLATE=C.UTF-8     LC_MONETARY=C.UTF-8    LC_MESSAGES=C.UTF-8   
#>  [7] LC_PAPER=C.UTF-8       LC_NAME=C              LC_ADDRESS=C          
#> [10] LC_TELEPHONE=C         LC_MEASUREMENT=C.UTF-8 LC_IDENTIFICATION=C   
#> 
#> time zone: UTC
#> tzcode source: system (glibc)
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> loaded via a namespace (and not attached):
#>  [1] digest_0.6.39     desc_1.4.3        R6_2.6.1          fastmap_1.2.0    
#>  [5] xfun_0.56         cachem_1.1.0      knitr_1.51        htmltools_0.5.9  
#>  [9] rmarkdown_2.30    lifecycle_1.0.5   cli_3.6.5         sass_0.4.10      
#> [13] pkgdown_2.2.0     textshaping_1.0.4 jquerylib_0.1.4   systemfonts_1.3.1
#> [17] compiler_4.5.2    tools_4.5.2       ragg_1.5.0        bslib_0.9.0      
#> [21] evaluate_1.0.5    yaml_2.3.12       jsonlite_2.0.0    rlang_1.1.7      
#> [25] fs_1.6.6          htmlwidgets_1.6.4
```
