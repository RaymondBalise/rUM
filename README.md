# rUM <a href='https://raymondbalise.github.io/rUM/'><img src='man/figures/logo.png' align="right" height="139" /></a>
This is a collection of R things from your friends at UM (The University of Miami).

rUM include:

+ A research project template.  It creates a new RStudio project that has an analysis R markdown file with tidyverse and conflicted.
+ R Markdown templates which include they YAML header and start up blocks that load the tidyverse and conflicted packages.

The plan is to add UM thesis and dissertation Markdown templates.

## How to I add rUM?

```r
if (!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("RaymondBalise/rUM")
```

## Where can I get rUM?
https://raymondbalise.github.io/rUM/

https://github.com/RaymondBalise/rUM

## How do I make rUM research project?

1. Use File > New Project > New Directory
1. Scroll down and then click **Research Project Template**
1. Specify where you want your research project to be saved.

## How do I use rUM for markdown headers?

1. Use File > New File > RMarkdown...
1. Click **From Template**
1. Choose a template
  + html2 with rUM
  + html2 Details with rUM
  + pdf2 showing LaTeX with rUM
  + bookdown_site with rUM 

### What are the headers?

#### html2 with rUM
This is a basic web page

#### html2 Details with rUM
This shows a table of contents

#### pdf2 showing LaTeX with rUM
PDF report where table and figures don't float to other pages.  Many thanks to https://stackoverflow.com/questions/16626462/figure-position-in-markdown-when-converting-to-pdf-with-knitr-and-pandoc

#### bookdown_site with rUM 
A bookdown website with a good table of contents for a book


