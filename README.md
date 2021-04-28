# opensilexClientR

 ![alt](https://shields.io/badge/R%20tested%20versions-3.4%20--%204.0.2-blue)

A set of functions to call with R to the Opensilex web service in OpenSILEX. You can retrieve data and metadata from experiments. User can access if he has an account on an instance of the Opensilex information system.

To initialize a client request:

- connectToOpenSILEX()
- getExperiments()
- getVariablesByExperiment()

In progress...
- getExperimentDesign()

 
# Installation

To install the **opensilexClientR** package, the easiest is to install it directly from Github. Open an R session and run the following commands:

```R
library(remotes)
install_github("OpenSILEX/opensilexClientR", build_vignettes=TRUE, ref="1.0.0")
```

You can also download a tar.gz archive of "[1.0.0](https://github.com/OpenSILEX/opensilexClientR/tree/1.0.0)" version and install it with _install_packages()_.

This package use [Semantic Versioning Specification](https://semver.org/) for versioning tags.

# Usage

Once the package is installed on your computer, it can be loaded into a R session:

```R
library(opensilexClientR)
help(package="opensilexClientR")
```

```{r library, echo=TRUE,message=FALSE, warning=FALSE}
  library(opensilexClientR)
```

# Generalities

## HTTP request
This package performs request to PHIS Web Service. We can distinguish two kinds of request:  
1.  GET requests that retrieve data from the web service.  
2.  POST requests that send data to the web service.  

They are nammed accordingly to the HTTP method GET and POST associated to it.

Because these functions rely on HTTP method, the first answer of the functions is the status of the request:  
- 200 and family: means the request has succeded  
- 300 and family: means the request was reoriented elsewhere  
- 400 and family: means the request failed from your side (your fault)  
- 500 and family: means that the request failed from the server (not your fault)  
  
What is distinct between the functions is the value taken in the $data list.  
Because this package aim at providing the most generic functions, the format has to be compatible with a lot of data structures. For examples list containing another list (a properties argument typically). That is to say, data.frames are not always compliant enough.

## Identifiers

OpenSILEX relies on URI (Uniform Resource Identifier) to identify resources. As an identifier, this is an un-ambiguous way to identify objects.  

<!-- When collecting data through the `getData` function, you will need to retrieve metadata of the variables through the `getVariables2` function if you want the label, unit and method used to create those data. -->

# Connect

Ask permission to request to the web service:

```{r connect}
  # If you want to access to a private web service, you have to insert the address of the WS and the port
 opensilexClientR::connectToOpenSILEX(identifier="guest@opensilex.org", password="guest", url = "http://www.opensilex.org/rest")
```

This token is handled automaticaly for you, but you can acces it using the following command :

```{r get user info}
getUserInformations()
```

# Fetch informations about elements in the web service

## Experiments

```{r getExperiments}
  experiments <- getExperiments()
  experiments$data
```

## getVariablesByExperiment

```{r getVariablesByExperiment}
  experimentVariables <- getVariablesByExperiment(uri="http://www.opensilex.org/demo/2018/o18000076")
  experimentVariables$data
```

## Comming soon

<!-- ##  ExperimentDesign

```{r getExperimentDesign}
  getExperimentDesign( uri="http://www.opensilex.org/demo/2018/o18000076")
``` -->

 ------------ Developper part  ------------  
 
## Session info

```{r session,echo=FALSE}
  sessionInfo()
```

# References
1. R Development Core Team (2018). R: A language and environment for statistical computing. R Foundation for
      Statistical Computing, Vienna, Austria. ISBN 3-900051-07-0, URL http://www.R-project.org.

# Citation

You should cite the **opensilexClientR** package:

```R
citation("opensilexClientR")
```

See also citation() for citing R itself.
