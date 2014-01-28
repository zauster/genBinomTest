genBinomTest
=======

`genBinomTest` implements a *generalized binomial test* for independently (**not necessarily identically**) distributed binomal random variables.

## How to download this R-package

### Regular users

There are two ways to download the R-package:

#### First option

1. Download the **whole repository** with `Download ZIP` at the right side of the files list.
2. Go to the folder where you saved the .zip file
3. Extract `genBinomTest-master.zip`
4. Start R (or R-Studio or whatever you use)
5. Inside R: Go into the newly generated folder `genBinomTest-master`, using either the GUI or the `setwd("path\to\folder\genBinomTest-master\R")` command. (In this folder you will find the *whole* repository (the R-Package, the manual and all program code, if you are interested in it)
6. Optionally: Check with `list.files()` if you are in the right folder: You should see a file named `genBinomTest_0.1.tar.gz`.
7. Install the package with `install.packages("genBinomTest_0.1.tar.gz", type = "source")` and load it afterwards with `library(genBinomTest)`
8. `example(genBinomTest)` will provide you with a simple example or see the manual for more information on how to use the package.

#### Second option

1. Click on `genBinomTest_0.1.tar.gz` in the files list above
2. Click on `View Raw` in the center
3. A small dialog will ask you where you want to save the file. Remember the folder.
4. Start R
5. Inside R: Go to the folder which contains the downloaded file and continue with 6. from above.

To just download the manual, click on `genBinomTest.pdf` and `View Raw`.

## How to download this STATA-package

1. As for now, the only possibility is to download the whole repository with `Download ZIP`
2. Extract `genBinomTest-master.zip`
3. Start STATA
4. Set the working directory to `\path\to\genBinomTest\stata` OR
4. Copy `genBinomTest.ado` and `genBinomTest.sthlp` to a folder where you keep your `.ado`-files, so that it gets automatically loaded at STATA-startup

### Developers

Clone the repository onto your computer, using the `git clone https://github.com/zauster/genBinomTest.git` command.
