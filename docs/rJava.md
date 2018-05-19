### rJava-related errors while you are running cytoClusteR ###

#### Solution ####

The way to solve this issue has been found in [`this post`](https://stackoverflow.com/questions/30738974/rjava-load-error-in-rstudio-r-after-upgrading-to-osx-yosemite).

Steps:

1. Please make sure you install JAVA, update MacOS, R and Rstudio (if applicable) to their latest versions.

2. Add the following to my .bash_profile (open the terminal application and copy and paste the following commands):
> export JAVA_HOME="/usr/libexec/java_home -v your_jave_version"   
> export LD_LIBRARY_PATH=$JAVA_HOME/jre/lib/server

3. Reconfigure java as follows (again copy and paste the following to the terminal):
> sudo R CMD javareconf -n

4. In R now, check type:
> options("java.home")

5. If this returns "NULL", set the java home (copy and paste the following in R's console):
> options("java.home"="/Library/Java/JavaVirtualMachines/jdk1.your_version.jdk/Contents/Home/jre")

6. Finally, copy and paste in the terminal the following:
> sudo ln -f -s $(/usr/libexec/java_home)/jre/lib/server/libjvm.dylib /usr/local/lib

#### OpenMP-related errors #### 
If in any of the steps above you encounter an error related to OpenMP, this is caused because R 3.4.0 is compiled by CRAN with llvm-4.0.0 (which supports OpenMP), but Apple's fork (installed by default on macOS) does not support OpenMP. To resolve this you need to install [`brew`](https://brew.sh/) and then the compilers from the command line with brew install gcc --without-multilib then you will have to add the compiler path to your ~/.R/Makevars file (you need to create this in your home directory).

CC=/usr/local/bin/gcc-7
CXX=/usr/local/bin/gcc-7
CXX11=/usr/local/bin/gcc-7

After that you should be all set. Restart R/Rstudio and run the following commands to run cytoClusteR (the commands are the same as in the main [`README file`](https://github.com/kordastilab/cytocluster/blob/master/README.md):
> library(shiny)  
> runGitHub("cytocluster", "kordastilab")
