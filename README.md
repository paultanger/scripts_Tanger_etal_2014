General
-------------------------
This directory contains project files for the following publication:

(insert link to doi here)

Comments and requests should be directed to:
jan.leach@colostate.edu

While all data and code is made freely available, we would appreciate citation when appropriate.


Files
-------------------------
There are 3 directories:

fieldvsGH_final - R scripts

input           - input files

output          - output files that scripts generate

Requirements
-------------------------
Obviously you need R installed (> 3.0.0 should do).

Initially set the working directory to fieldvsgh_final/

Based on wherever you unzipped the file from Datadryad.
```r
setwd("fill/in/your/path/fieldvsgh_final/")
```

If you downloaded from datadryad, you need to get the scripts.

You can either use git clone (if you have git installed),

or just download the files directly:
```r
# with git clone
setwd("../")
system("git clone https://paultanger@bitbucket.org/paultanger/scripts_tanger_etal_2014.git fieldvsgh_final")
setwd("../fieldvsgh_final/")
# or download directly
url = "https://bitbucket.org/paultanger/scripts_tanger_etal_2014/get/9defaf64adda.zip"
thefile = tempfile(tmpdir=tempdir(), fileext=".zip")
download.file(url, thefile, method="curl")
unzip(thefile, exdir="../fieldvsgh_final", overwrite=F)
unlink(thefile)
```

If you downloaded from bitbucket, you need to get the data:
```r
url = ""
thefile = tempfile(tmpdir=tempdir(), fileext=".zip")
download.file(url, thefile, method="curl")
unzip(thefile, exdir="../", overwrite=F)
unlink(thefile)
```

The following packages are required:

This code only needs to be run once.

```r
install.packages("xlsx")
install.packages("rJava")
install.packages("reshape2")
install.packages("Hmisc")
install.packages("car")
install.packages("lsmeans")
install.packages("lme4")
install.packages("glmulti")
install.packages("AICcmodavg")
install.packages("plyr")
install.packages("ggplot2")
install.packages("grid")
install.packages("gridExtra")
install.packages("scales")
install.packages("corrplot")
install.packages("RColorBrewer")
```

Then, load these packages.  This needs to be done every time before running the scripts.

```r
library("xlsx")
library("rJava")
library("reshape2")
library("Hmisc")
library("car")
library("lsmeans")
library("lme4")
library("glmulti")
library("AICcmodavg")
library("plyr")
library("ggplot2")
library("grid")
library("gridExtra")
library("scales")
library("corrplot")
library("RColorBrewer")
```

How to Use
-------------------------
The files used to process the data and generate figures, tables, and objects for further manipulation:

Feel free to explore each script individually to examine how the data was processed and modify as you see fit.

```r
source("mergealldatatogether.R")
```
This requires scripts that process each input file separately.  The input files are named appropriately and located in the input directory.  It outputs tab separated value files that are used in all subsequent calls below.  All calls below this can be run separately in any order.  The most important output from this step is summary statistics of biological replicates in the output file named "SummaryData5Lines_Rearrange.tsv"

```r
source("plot20linedata.R")
```
This generates Fig 1 and Fig S1

```r
source("getcontrasts.R") 
```
This generates Table S2 (formatting was done in excel)

```r
source("getvariancecomps.R")
```
This generates Tables 2 and 3

```r
source("getfullequationsfrommodelselection.R")
```
This generates Tables S10 and S11 (some models were added manually)

```r
source("examinelinearmodelsGHtoField.R")
```
This generates Table S7

```r
source("getandgraphcorrelations.R") 
```
This generates Fig 6 and Tables S8 and S9

```r
source("pairwisecorrelations.R")
```
This generates the pairwise list for Tables S8 and S9

```r
source("graphdata.R")
```
This generates all other figures except Fig 5 (produced in JMP)

License
-------------------------
Copyright (c) 2014, Paul Tanger

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.