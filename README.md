   <!-- badges: start -->
   <!-- [![R-CMD-check](https://img.shields.io/badge/R--CMD--check-passing-brightgreen)](https://github.com/RaymondBalise/rUM) -->
  [![CRAN status](https://www.r-pkg.org/badges/version/rUM)](https://CRAN.R-project.org/package=rUM)
  [![Lifecycle: stable](https://lifecycle.r-lib.org/articles/figures/lifecycle-stable.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
  [![CRAN monthly downloads](https://cranlogs.r-pkg.org/badges/rUM)](https://www.r-pkg.org/pkg/rUM)
  <!-- [![CRAN downloads](https://cranlogs.r-pkg.org/badges/grand-total/rUM)](https://cran.r-project.org/package=rUM) -->
  <!-- badges: end -->
  
# rUM <a href='https://raymondbalise.github.io/rUM/'><img src='man/figures/logo.png' align="right" width="139" /></a>
This is a collection of R things from your friends at UM (The University of Miami).

To see a tutorial video on rUM check out our [presentation at R Medicine 2025](https://www.youtube.com/watch?v=03_5KrQA-mo&list=PL4IzsxWztPdmU2q31ZrTCASr78e0jpKux&index=26){target="_blank"}.

`rUM` includes:

* A research project template.  It creates a new RStudio project that has your choice of an `analysis.qmd` Quarto file or `analysis.Rmd` R markdown file that load the tidyverse and conflicted packages and set many useful default details.  These files can also include example tables and figures.

* `rUM` streamlines creating a package that include a paper outline with the `make_package()` function.

* `rUM` helps with project documentation files (`write_readme()` and `write_notes()`), and Quarto styling files (`write_scss()`).

* 💥 NEW in Version 2.2.0 (rUM Runner) 💥 
    + `rUM` can make documentation/manual pages for datasets with the `write_man()` function.  
    + `rUM` now includes templates to produce slide decks with the `write_slides()` function.  There are now functions (`find_slides()` and `show_slides()`) to find and render/show slides that are included with a package.

## How do I add rUM?

Modern versions of the RStudio interface (v2022.07 or later) ships with Quarto; update to the most recent version of RStudio [here](https://posit.co/download/rstudio-desktop/). You can install the latest version of Quarto directly from [here](https://quarto.org/docs/get-started/).

Then you should run this in the console of RStudio:

```r
if (!requireNamespace("remotes")) install.packages("remotes")
remotes::install_cran("rUM")
```

If you would like the (unstable) development version, use the following code instead:
```r
if (!requireNamespace("remotes")) install.packages("remotes")
remotes::install_github("RaymondBalise/rUM")
```

## Can I make a R package that includes my paper as a vignette?  
Yes!  When you use the `make_project()` function set `vignette = TRUE`.  See the "Make a Package" vignette which ships with rUM. Run this line `vignette("Make a Package", package = "rUM")` to see it now. 

### Can I make a Quarto based vignette?
Yes!  If you would like to write a Quarto vignette you need have Quarto version 1.4.549 or higher.  You can check your version with `quarto::quarto_version()`.  You can install the latest version of Quarto directly from [here](https://quarto.org/docs/get-started/).

## Where can I get rUM?
https://raymondbalise.github.io/rUM/

https://github.com/RaymondBalise/rUM

## How do I make rUM research project?

1. Use File > New Project > New Directory
1. Scroll down and then click **rUM Research Project Template**
1. Specify where you want your research project to be saved.

#### How do change the reference style for my paper?
The default reference style is the New England Journal of Medicine.  The style is set by the [Citation Style Language](https://citationstyles.org/) (`csl:`) option near the top of the file.  To use a different style, download a csl file from [https://www.zotero.org/styles/](https://www.zotero.org/styles/) into the folder with your analysis file.  Then change *the-new-england-journal-of-medicine.csl* to the name of the file you downloaded.

#### How do I control what files appear in my reference section?
The analysis file includes this code near the bottom:

```
  c(
    .packages(),
    "rUM",
    "table1"
  ),
```

If you want to acknowledge a package that is not used directly in your analysis file, add its name inside of the `c()` function.  The authors of the packages that you used for exploratory data analysis will thank you.  

After you knit/render the analysis file once the packages will appear in the "packages.bib" file in the same directory/folder as your analysis file. If you are using the RStudio **Visual Editor**, put the cursor when you want to add the citation, then use the Insert Menu and choose *@ Citation...* and pick the article.  If you are using the Visual Studio **Source Editor**, open the "packages.bib" file, find the manual reference for the package that you want to add and copy it.  For example, if you needed to add a reference to the `rUM` package you would find this line:

```
@Manual{R-rUM
```

and copy the reference name.  Here the name is `R-rUM`. Paste that where you want the citation like this `[@R-rUM]`.

## How do I use rUM for `R Markdown` headers?

1. Use File > New File > RMarkdown...
1. Click **From Template**
1. Choose a template
  + html2 with rUM
  + html2 Details with rUM
  + pdf2 showing LaTeX with rUM
  + bookdown_site with rUM 
  
This will create a new subdirectory in your current working directory with the same name as the name of the `.Rmd` file you specified. Within this directory, you will find the analysis R Markdown file. For example, if you created an R Markdown file called `wrangle_cytometry_data.Rmd` with the steps above, then your current directory will now have a subdirectory called `wrangle_cytometry_data/` which will contain the file `wrangle_cytometry_data.Rmd` and any subsequent files from the knitting process (such as `.PDF`, `.html`, or `.docx` files created by knitting the RMarkdown document).

### What are the headers?

#### html2 with rUM
This is a basic web page

#### html2 Details with rUM
This shows a table of contents

#### pdf2 showing LaTeX with rUM
PDF report where table and figures don't float to other pages.  Many thanks to https://stackoverflow.com/questions/16626462/figure-position-in-markdown-when-converting-to-pdf-with-knitr-and-pandoc

#### bookdown_site with rUM 
A bookdown website with a good table of contents for a book


