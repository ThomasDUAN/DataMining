loadAndInstallPackage = function(packageName){
  avaiable = library(packageName, character.only = T, logical.return = T, quietly = T) # check if extension plyr is avaiable
  if(!avaiable){
    install.packages(packageName) # if extension is not available, install it
  }
}
