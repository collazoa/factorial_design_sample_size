
# set up for running the R-script 

pkgs <- c("Superpower", "tidyverse", "gridExtra", "grid", "plotly", "WebPower", "easypackages")
ipkgs <- rownames(installed.packages()) 
npkgs <- setdiff(pkgs, ipkgs)
if (length(npkgs) > 0) install.packages(npkgs) 
easypackages::libraries(pkgs)
for(p in pkgs) print(sprintf("Package %s, version=%s", p, packageVersion(pkg = p)))
