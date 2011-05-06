lib.code <- function () UseMethod("lib.code")

# Function to be pasted in Rprofile

lib.code.default =function (){
  cat("\n****************************")
  cat("\n**   Read help document   **")
  cat("\n****************************")
  cat("\n\n\ Copy and paste the folowing code in the appropriate 'Rprofile' file:")
  
                                                                                                                                                                              
                                                                                                                                                                              
code=paste("\n\nlib=function (pack, install=TRUE, load=TRUE, quietly=TRUE, warn.conflicts = FALSE)",
            "\n{",
            "\n    if (install) {",
            "\n        installed <- pack %in% .packages(all.available = TRUE)",
            "\n        inst=pack[installed]  ",
            "\n        ninst=pack[!installed]",
            "\n        mess2=''              ",
            "\n                              ",
            "\n        if (length(ninst) >= 1) {",
            "\n            for(i in ninst){",
            "\n            mess.pers=c(' Warning:', getOption('pkgType'),'for',i,'does not exist in selected repository.', ",
            "\n                            'Trying to download and install source file','')",
            "\n            mess.binary=tryCatch(install.packages(i, dependencies = TRUE), warning = invisible)  ",
            "\n	    if(!is.null(mess.binary))                                                                 ",
            "\n	    {                                                                                         ",
            "\n            	mess.source=tryCatch(install.packages(i, dependencies = TRUE,type='source'), warning = invisible)",
            "\n            	if(!is.null(mess.binary) & !is.null(mess.source) & !silent) {mess2=c(mess2,mess.binary$message)} ",
            "\n	    }        ",
            "\n              ",
            "\n            } ",
            "\n        }     ",
            "\n    }         ",
            "\n              ",
            "\n    if(load){",
            "\n             ",
            "\n    	for (i in pack)",
            "\n	{                  ",
            "\n        	try(library(i, character.only = TRUE,warn.conflicts=warn.conflicts,quietly=quietly),silent=T)",
            "\n    	}                  ",
            "\n    }                   ",
            "\n                        ",
            "\n    if(length(mess2)>1){",
            "\n      cat('', fill=T)   ",
            "\n      cat('Warning:')              ",
            "\n        for(i in 1:length(mess2)){ ",
            "\n            cat(mess2[i], fill=T)  ",
            "\n        }                          ",
            "\n    }                              ",
            "\n    rm(list='mess2')               ",
            "\n}                                  ")
  cat(code,fill=T)
  
  
}
  
 


