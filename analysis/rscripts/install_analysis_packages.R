package_names = c("btblv", "tblvArmaUtils")

for(package in package_names) {
  devtools::install_github(
    repo = "pedroaraujo9/tblv", 
    subdir = paste0("packages/", package)
  )
  
}
