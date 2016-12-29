# gli [WIP]

Goal is to fetch PR info etc.

## Setup

1. `stack setup`
2. `stack build`
3. `stack exec which gli` to get the binary path


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

2. **Setup `gli` for a git repo**
  This will create a repo specific `.gli.yml` file, which will store all the information regarding the repo.
  ```
    $ gli setup -f ~/.gli.yml
  ```

3. **Fetch all open PR related info**
  ```
    $ gli prs
  ```
