# Define your report 
system("RMDFILE=Knitr1234") 
# Knit the Rmd to an Md file 
# Convert the MD file to Html 
system("Rscript -e 'require(knitr);require(markdown);knit('$RMDFILE.rmd', '$RMDFILE.md'); 
markdownToHTML('$RMDFILE.md', '$RMDFILE.html', options=c(\"use_xhml\"))'") 
require(knitr) 
require(markdown) 
knit("Knitr1234.Rmd") 
markdownToHTML('Knitr1234.md','Knitr1234.html', options=c("use_xhml")) 
system("pandoc -s Knitr1234.html -o Knitr1234.pdf") 