# BraTs-Validate-Users

A tool that monitors Google Form responses collected in a Google Sheet.
It will look for new submissions at a set cadence (default, 5 minutes),
and will check the following:

- Does the provided Synapse username exist?
- Is the user registered for the challenge?

> [!NOTE]:
> Not currently super user-friendly for non-R users.

## Getting Started

* Clone the repo

    ```bash
    git clone https://github.com/Sage-Bionetworks-Challenges/BraTs-Validate-Users.git
    ```

* Install Miniconda

    ```bash
    wget https://repo.anaconda.com/miniconda/Miniconda3-py310_23.3.1-0-Linux-x86_64.sh
    bash Miniconda3-py310_23.3.1-0-Linux-x86_64.sh
    rm Miniconda3-py310_23.3.1-0-Linux-x86_64.sh
    ```

    Restart the terminal to apply the changes.

* Initiate a Conda environment

    ```bash
    conda env create -f environment.yml
    conda activate brats-tool
    ```

* Install the required libraries

    ```bash
    Rscript requirements.R
    ```

* Set up your configurations for the user validation.  Update `config_example.R` and save as
    `config.R`.  After updating, set the file to read-only.

    ```bash
    cp config_example.R config.R
    vim config.R
    chmod 400 config.R
    ```

## Usage

> [!IMPORTANT]
> when running for the first time, use Rstudio (or other interactive IDEs) to initiate 
> `google authentication` in order to access to the Google Form. The `googlesheet4` 
> package is used to read Google Sheet, (TODO: change to use `service_credential.json`).

1. Open the `setup.R` file in the Rstudio
2. Press <kbd>Control</kbd>/<kbd>Command</kbd> + <kbd>A</kbd> to select entire script
3. Press <kbd>Control</kbd>/<kbd>Command</kbd> + <kbd>enter</kbd> to run all the codes
4. Press `1` in the console when it asks for permission to pop up browser window. Please
    sign in your google account which has access to the google sheet and complete the 
    authentication.

If not the first time, run the following code to test if the setup works.  If so, run the
last command.

```bash
Rscript setup.R
```

Start monitoring new submissions from the google form

```bash
Rscript runMonitor.R
```
