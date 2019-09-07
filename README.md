# ml4
Data and analysis scripts for the [Many Labs 4](https://osf.io/8ccnw/) project. The same mortality salience effect ([Greenberg et al., 1994; study 1](https://www.ncbi.nlm.nih.gov/pubmed/7965609)) was replicated in 21 independent labs. Half the labs designed their own replications without any feedback from original authors or other experts. The remaining half ran a version of the study designed with original author feedback. This was done by two different teams within the same university when possible. Data analysis focused on meta-analytic estimates of the overall effect size, and whether the different versions varied in their success obtaining the offect (with a particular eye towards whether original author feedback resulted in more successful replications). See more details and materials on the OSF page: [https://osf.io/8ccnw/](https://osf.io/8ccnw/).

![](./images/flow.png)

# To use:

1. Install the latest version of **[R](https://cran.r-project.org/)**.
2. Install the latest version of **[R Studio](https://www.rstudio.com/products/rstudio/download/#download)**.
2. Download this repository to your computer by clicking the green "Clone or Download" button on the upper right of this page, download the .zip file and unzip it to anywhere on your hard drive. (Git savvy folks can instead fork and clone the repo if they wish).
3. Deidentified data are provided and referenced in the scripts from the ml4/data/public subdirectory, but you may need the not-deidentified data to run some scripts (private due to confidentiality concerns). Email Rick (raklein22@gmail.com) to discuss how to obtain these (typically, this entails providing proof of IRB approval for secondary data analysis from your home institute). If you have those data, you'd unzip them into the ml4/data/raw_site_data subdirectory.
4. Open the Projects file (.Rproj) **in R Studio**. [More info on the benefit of R Project files](https://support.rstudio.com/hc/en-us/articles/200526207-Using-Projects).
5. In R Studio, open the r script (.R) file you want to use. In this case the scripts are labelled numerically in the order they would typically be run, but all files can run stand-alone (e.g., you don't need to run all the scripts). Files are: 
- **000_ml4.rproj** this is simply the .rproj file you should open first to automatically set the working directory.
- **001_data_cleaning.R** which reads in raw site data, and does any necessary tidying to restructure into a single merged file with data from all sites.
- **002_ml4analysis.R** which produces the results for each site, and primary meta analytic results.
- **003_metaviz.R** code to produce Figure 1, the forest plot of all results.
- **004_exp_survey_analysis.R** which analyzes experimenter responses to the 'Experimenters Survey' about researcher expectations.
- **005_analyses_supplemental.R** produces supplemental analyses such as analyses treating the pro- and anti- author ratings as separate DVs.
- **006_ml4_papaja_results.Rmd** is an RMarkdown file that generates the results section from the manuscript and automatically populates the statistical results. 
- **007_tables.Rmd** produces various tables presented in the manuscript.
- **008_ml4_supplemental_results.Rmd** is an RMarkdown file that generates the supplemental results reporting section and automatically populates the statistical results. 

Contact me if you have any issues/comments and I'll help work through them.
