
# Render site ------------------------------------------------------------

###### logic
# there are some limitations for rendering websites in Rmarkdown/Githubpages
# as all files should be in a same directory, which goes against the logic
# of a clean and organized folder structure. Therefore, this script:
# 1. bring all necessary files to the "docs" folder, where the site files are hosted
# 2. renders the site from this location (docs folder)
# 3. delete the Rmd files that where brought in step 1, leaving only the html files


# INSTRUCTIONS-------------------------:
# 1. set your root dir at "merit-scale"
# 2. Run this code.

production<- list.files(path = "production",pattern = "prod_")        # a list with the names of the files to copy
file.copy(file.path("production",production), "docs",overwrite = TRUE)# copy data proc and analysis files

rmarkdown::render_site("docs") # Render site

# before you run this line, check if your R Markdown files have a .rmd or .Rmd extension
# on this case we use both
ext <- c(grep("^prod_.*\\.rmd$",x = dir(path = "docs"),value = T), # for .rmd
         grep("^prod_.*\\.Rmd$",x = dir(path = "docs"),value = T), # for .Rmd
         ".tex",".log")                                            # for .tex and .log

for (i in 1:length(ext)) {
  file.remove(paste0("docs/",dir(path="docs", pattern=ext[i]))) # delete files
}

browseURL(url = "docs/index.html")

# Keep only the original .rmd files (site), configuration (_site.yml and _config.yml)
# and prod_ folders (cache and files)
