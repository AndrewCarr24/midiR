The midiR package allows you to create MIDI files in R.  The package is particularly useful for creating drum loops that can be played by a digital audio workstation (for example, Garage Band or Audacity).  

To download this package install the devtools package and follow these instructions.

```{r, eval = F}
devtools::install_github("datadiarist/midiR", build_vignettes=TRUE)
library(midiR)
```

For a tutorial on how to use this package, refer to the vignette.

```{r, eval = F}
browseVignettes("midiR")
```
