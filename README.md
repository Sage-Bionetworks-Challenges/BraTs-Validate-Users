# BraTs-Validate-Users

This is a tool to monitor google form responses. Technically, it is watching the google sheet that saves the google form's responses. The tool will detect for new submissions every minute and validate the new submission's information by followings:

- whether the user name is in the preregistration team, but not the validated team
- whether first name and last name match
- whether user name is valid

Note: it is now not super friendly set up for non-R users and will improve if needed.
## Installation

* Clone the repo

    ```Bash
    git clone https://github.com/Sage-Bionetworks-Challenges/BraTs-Validate-Users.git
    ```

* Install Miniconda

    ```Bash
    wget https://repo.anaconda.com/miniconda/Miniconda3-py310_23.3.1-0-Linux-x86_64.sh
    bash Miniconda3-py310_23.3.1-0-Linux-x86_64.sh
    rm Miniconda3-py310_23.3.1-0-Linux-x86_64.sh
    ```

    Restart the terminal to apply the changes.

* Initiate Conda Environment

    ```Bash
    conda env create -f environment.yml
    conda activate brats-tool
    ```

* Install r libraries

    ```Bash
    Rscript requirements.R
    ```

    Please copy and **modify** the information in `config.R`, such as 'google form questions' and 'gmail address'. After all information is filled, set the file to read only.

    ```
    cp config_example.R config.R
    vi config.R
    chmod 400 config.R
    ```

## Usage

**Important**: for the first time running, please use Rstudio (or other interactive IDEs) to initiate `google authentication` in order to access to the google form. The `googlesheet4` package is used to read google sheet, (TODO: change to use `service_credential.json`).

1. Open the `setup.R` file in the Rstudio
2. press <kbd>Control</kbd>/<kbd>Command</kbd> + <kbd>A</kbd> to select entire script
3. press <kbd>Control</kbd>/<kbd>Command</kbd> + <kbd>enter</kbd> to run all the codes
4. Press `1` in the console when it asks for permission to pop up browser window. Please sign in your google account which has access to the google sheet and complete the authentication.

If not the first time, simply run below code to test if all setup work and go to the next step if no errors.

```Bash
Rscript setup.R
```

Start monitoring new submissions from the google form

```Bash
Rscript runMonitor.R
```
