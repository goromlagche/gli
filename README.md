# gli [WIP]

Goal is to fetch PR info etc.

## Setup

    cabal install gli

## Flow

1. **Store the gitlab credentials on a file locally. (e.g. ~/.gli.yml)**

  sample file
  ```
      accounts:
        my_hosted_gitlab:
            key: abcd1234xyz
            url: https://gitlab.dev.my_hosted_gitlab.com/api/v3
        gitlab:
            key: xyz4321dcba
            url: https://gitlab.com/api/v3
  ```
  You can get the credentials from https://gitlab.com/profile/account

2. **Setup `gli` for a git repo**

  This will create a repo specific `gli.yml` file, which will store all the information regarding the repo, and will also be checked out from git.

  ```
    $ cd my_gitlab_repo_path
    $ gli setup -f ~/.gli.yml
    $ cat gli.yml
    project:
      ssh_url_to_repo: git@gitlab.com:organization/repo_name.git
      name: repo_name
      id: 123
      description: 'killer app 42'
    masterFileConfig:
      key: gitlab
      file: /Users/goromlagche/.gli.yml
  ```

3. **Fetch all open PR related info**

  ```
    $ gli prs
  ```
