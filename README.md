# monitOS: Monitoring OS in chronic or indolent cancers

## Description

@evanthia to fill

## Team (alphabetical)
- Mouna Akacha (AMDS): <mouna.akacha@novartis.com>
- Bharani Bharani-Dharan (GDD Onco): <bharani.bharani-dharan@novartis.com>
- Frank Bretz (AMDS): <frank.bretz@novartis.com>
- Arunava Chakravartty (GDD Onco) <arunava.chakravartty@novartis.com>
- Thibaud Coroller (AMDS): <thibaud.coroller@novartis.com> 
- Lisa Hampson (AMDS): <lisa.hampson@novartis.com> 
- Evanthia Koukouli (AMDS): <evanthia.koukouli@novartis.com>
- Nigel Yateman (GDD Onco): <nigel.yateman@novartis.com>
- Emmanuel Zuber (GDD Onco): <emmanuel.zuber@novartis.com>


## Installation on HPC

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


