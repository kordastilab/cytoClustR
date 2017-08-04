### rJava-related errors while you are running cytoClusteR ###

#### Solution ####

The way to solve this issue has been found in [`this post.`](https://stackoverflow.com/questions/30738974/rjava-load-error-in-rstudio-r-after-upgrading-to-osx-yosemite)

Steps:

1. Please make sure you update MacOS, R and Rstudio (if applicable) to their latest versions.

2. Add the following to my .bash_profile (open the terminal application and copy and paste the following commands):
> export JAVA_HOME="/usr/libexec/java_home -v 1.8"   
> export LD_LIBRARY_PATH=$JAVA_HOME/jre/lib/server

3. Reconfigure java as follows (again copy and paste the following to the terminal):
> sudo R CMD javareconf -n

4. In R now, check type:
> options("java.home")

5. If this returns "NULL", set the java home (copy and paste the following in R's console):
> options("java.home"="/Library/Java/JavaVirtualMachines/jdk1.8.0_45.jdk/Contents/Home/jre")

6. Finally, copy and paste in the terminal the following:
> sudo ln -f -s $(/usr/libexec/java_home)/jre/lib/server/libjvm.dylib /usr/local/lib

After that you should be all set. Restart the R/Rstudio and run the following commands to run cytoClusteR (the commands are the same as in the main [`README file`](https://github.com/kordastilab/cytocluster/blob/master/README.md):
> library(shiny)  
> runGitHub("cytocluster", "kordastilab")
