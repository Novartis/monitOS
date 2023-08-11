# monitOS: Monitoring OS in chronic or indolent cancers

## Description

The aim of `monitOS` is to put together a number of tools which can facilitate safety monitoring based on OS for trials with low expected number of events. These tools could be used at either interim or final OS analysis depending on study needs. The following material are available as part of this project:

 - `monitOS` R package, a collection of functions to implement the methods discussed in [link to paper draft]
 - `monitOS` R shiny app, where users can automatically explore the proposed methods, assess operational characteristics and download a report with findings for a user specified case study as wel as example case studies from internal and external trials.
 - Webpage accessible via [go/monitos](https://rsconnect-prod.dit.eu.novartis.net/monitOS/scripts/about.html). This document provides the methodological background of each provided tool with accompanied resources for further reading, worked examples showing `monitOS` implementation and useful links where `monitOS` development code is stored and available.

**IMPORTANT !**
`monitOS` intention is to function as a toolkit and provide description and implementation examples of methods for the monitoring of overall survival (OS) in situations where the number of events is limited. It is NOT the package’s, and this website’s, intention to offer recommendations or influence decision making around the design of individual clinical trials. Appropriate methods and design approaches should be adopted on a case-by-case basis, after careful investigation and assessment.

## Installation

__Git (easier)__

- Clone Master branch
```bash
git clone http://gitlabce.apps.dit-prdocp.novartis.net/AEA/monitos.git
``` 
- Go to repository and install using devtools
```bash
devtools::install('/path/to/monitOS')
``` 


__Devtools (advanced but safer):__

- Check if SSH is enabled (__must see__ `$ssh == TRUE`): 

    ```bash
    git2r::libgit2_features()
    ```
- Setup ssh keys (for most users): 

    ```bash
    cred <- git2r::cred_ssh_key(publickey = '~/.ssh/id_rsa.pub', privatekey = '~/.ssh/id_rsa')
    ```
- install package: 
    
    ```bash
    devtools::install_git("ssh://git@ssh-gitlabce.apps.dit-prdocp.novartis.net:32222/AEA/monitos.git", ref="master", credentials=cred, protocol = "ssh")
    ```

__Alternative way:__

- Clone Master branch
```bash
git clone http://gitlabce.apps.dit-prdocp.novartis.net/AEA/monitos.git
``` 
- Within your R session go to *Build* > *Build Source Package*. 

- Then, create a personal `New folder` to store locally installed packages.

- Within your new project install the package by running the following command:
  
  ```
  install.packages("monitOS_0.1.0.tar.gz", lib = "New folder", repos=NULL, type='source')
  ```
- You can now load the package as follows

    ```
    library(monitOS, lib.loc = "New folder")
    ```

## Usage

You can find worked examples demonstrating `monitOS` R package and shiny app usage in [go/monitos](https://rsconnect-prod.dit.eu.novartis.net/monitOS/scripts/about.html). 

## Contributing

Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.

Please make sure to update tests as appropriate.

## Project status

This is a currently **active** project.

**Note that R package and shiny app are currently under development.** An initial R package version can be found within this repo.

Documentation is available [here](https://rsconnect-prod.dit.eu.novartis.net/monitOS/scripts/about.html).

## Team (alphabetical)

- Mouna Akacha (AMDS): <mouna.akacha@novartis.com>
- Bharani Bharani-Dharan (GDD Onco): <bharani.bharani-dharan@novartis.com>
- Frank Bretz (AMDS): <frank.bretz@novartis.com>
- Arunava Chakravartty (GDD Onco) <arunava.chakravartty@novartis.com>
- Thibaud Coroller (AMDS): <thibaud.coroller@novartis.com> 
- Lisa Hampson (AMDS): <lisa.hampson@novartis.com> 
- Evanthia Koukouli (GDD CRM): <evanthia.koukouli@novartis.com> 
- Nigel Yateman (GDD Onco): <nigel.yateman@novartis.com>
- Emmanuel Zuber (GDD Onco): <emmanuel.zuber@novartis.com>

Contact Evanthia Koukouli for comments on `monitOS` documentation. Contact Thibaud Coroller and/or Evanthia Koukouli for comments on package R functions. Thibaud Coroller should, also, be contacted for comments/questions regarding the R shiny app. You can direct your methodological inquiries to Lisa Hampson. 

## License

TBC

