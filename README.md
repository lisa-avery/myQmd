
<!-- README.md is generated from README.Rmd. Please edit that file -->

# biostats-report

This template provides a boilerplate for statistical reports using
Quarto in Rstudio.

Usage:

1.  From RStudio, if you would like to create a new folder for the
    report then insure that your working directory is the parent
    directory of the new folder. You can do this by using the `setwd()`
    function in R or by using the RStudio GUI to set the working
    directory. Otherwise, files will be copied into the working
    directory.

2.  Type the following command into the Terminal Window in RStudio
    (beside Console)

*If you can not see the Terminal Window, use the following keyboard
shortcuts:* Shift+Alt+M on Windows or Shift+Option+M on Mac

``` r
quarto use template biostatsPMH/biostats-report
```

> You will be prompted to trust the authors, you can type yes or y to
> accept. You may choose to create a subdirectory for the project, where
> the template files will be copied or, if you select no, the files will
> be copied to the current working directory.

The following files will be created in the new folder:

- *template.qmd* -OR- “name_of_new_folder.qmd”, if you created a
  subdirectory. This is the main report file.
- *wordTemplate.docx* This is the word template for the report
- *Vancouver.csl* This dictates that any references will be shown in
  Vancouver style
- *bibfile.bib* This is the bibtex file for references, it comes
  pre-populated with the citation for R.
- *functions.R* This is a good place to store functions that you create
  for the report. It is sourced in the template.qmd file.
- *checklist.qmd* This is a checklist for good statistical reporting and
  also includes the TRIPOD guidelines.

The following files are listed in `.quartoignore` so that they are not
included when you install a template. You can remove them from the
ignore list if you want to include them in the report:

    .Rproj.user
    .Rhistory
    .RData
    .Ruserdata
    README.Rmd
    useful_tricks.qmd
    repeat_chunks.qmd
    biostats-report.Rprog
    tripod.xlsx
    *.html

There is also a .gitignore file, this is where you can add any files
that you do not want to be included in the git repository.
