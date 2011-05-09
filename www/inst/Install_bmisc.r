# Define repository from with to search for packages
options(repos=c("http://cran.skazkaforyou.com/","http://R-Forge.R-project.org"))

# Function to install missing packages from binary 

  
# Find out what bmisc needs that is not already installed
necessary <- c("car","lattice", "zoo", "robustbase", "methods", "bmisc")

# Find out what is already installed on the local computer
installed <- necessary %in% installed.packages()[, 'Package']

# Update what is already installed and necessary for bmisc to work
if (length(necessary[installed]) >=1)
  update.packages(necessary[installed])

# Install what is not already installed and necessary for bmisc to work
if (length(necessary[!installed]) >=1)
  install.packages(necessary[!installed], dependencies=T)

