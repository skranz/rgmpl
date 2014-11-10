rgmpl
=======

Tools to run GMPL files from R

## Installation
First install the package devtools from CRAN and then run

```{r}
  library(devtools);
  install_github("skranz/rgmpl")
```
To run AMPL files via the NEOS Server, you also need to run

install.packages("XMLRPC", repos = "http://www.omegahat.org/R", type = "source")
install.packages("rneos")
