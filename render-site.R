
# Render site ------------------------------------------------------------

site<- list.files(path = "site")
file.copy(file.path("site",site), "docs", overwrite = TRUE) # copy main text of the site

production<- list.files(path = "production",pattern = ".Rmd") # copy data proc and analysis code
file.copy(file.path("production",production), "docs",overwrite = TRUE)

rmarkdown::render_site("docs") # Render site

unlink(paste0("docs/",dir(path = "docs",pattern = "_files")), recursive = TRUE) # add folder name to be deleted
unlink(paste0("docs/",dir(path = "docs",pattern = "_cache")), recursive = TRUE) # add folder name to be deleted

ext <- c(".rmd",".Rmd",".yml",".tex",".log") #add file extension to be deleted
for (i in 1:length(ext)) {
  file.remove(paste0("docs/",dir(path="docs", pattern=ext[i])))
}


