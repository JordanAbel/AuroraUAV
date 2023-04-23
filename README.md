# How To Run The Program

## Prerequisites
Before you begin, ensure that you have the following installed on your computer:
- R
- PyCharm
- Python 3.9
## R Installation
### Windows
Visit the CRAN website and click on Download R for Windows.

Click on base.

Click on Download R for Windows to download the installer.

Click on the file that was downloaded and follow the steps in the installer to complete the installation process.
### Mac

Visit the CRAN website and click on Download R for Mac.

Depending on whether you have an Intel or M1 Mac, choose the appropriate .pkg file to download.

Install R using the downloaded .pkg file.
You may also need to install Xcode Command Line Tools. To do this, go to developer.apple.com/downloads and log in with your Apple ID.
Type command line tools in the search field and hit Enter.

Click View Details below the entry you want, and click on the listed .dmg file to download it.
Install Command Line Tools using the downloaded .pkg file.
You may also need to install XQuartz on your Mac to view the 3D plots generated. Visit xquartz.org to download the installer.

Install XQuartz using the downloaded .pkg file.
## Installing Python
Download the installer for Mac or Windows. (Note: We recommend using Python 3.9.)
For Windows, during the installation process, make sure you check the appropriate boxes as shown in the images below:


## Adding Python Interpreter to PyCharm
In PyCharm, go to Settings > Project: Aurora UAV > Python Interpreter.
Click Add interpreter > Add Local Interpreter.
Select the Python version that you installed.
You should now be able to select the newly created interpreter as your main interpreter, as shown in the screenshot in Step 2.

## Installing PyCharm
Visit the PyCharm download page and click Download on the Community version.
Once the installer is downloaded, open it and follow the instructions.
Close the installer and open PyCharm.

## Project Setup
Open PyCharm (if you are on a Mac, you may need XQuartz installed).
Click the Projects tab if not already selected.
Click Get from VCS.
In the URL box, paste the following link: https://github.com/JordanAbel/AuroraUAV.git
Click Clone.
Once the project is loaded, go to File > Settings > Plugins > Marketplace.
In the search bar, search for R language for IntelliJ.

Click Install and apply the changes. (Note: Wait for the package to load before doing anything; there is a progress bar at the bottom.)
### Importing Point Cloud and Orthomosaic Files
In PyCharm, right-click on the directory AuroraUAV.
After right-clicking, look in the menu for Open In > Explorer.
Double-click on the AuroraUAV folder in the Explorer window.
In this folder, right-click and create a new folder called RGB Data.
Create copies of the point cloud (.las) file and orthomosaic (.tif) file, and paste them into the RGB Data folder.
Rename the point cloud and orthomosaic files in the folder to:
Orthomosaic: "rgb_map.tif"
Point Cloud: "points.las"
The "RGB Data" folder should now look like this:


## Running the Project
Open the file tree_segmentation_v1.R located in the R Scripts folder by double-clicking on it.

Hover over each of the libraries and click Install.

Check the R console to see when the library is downloaded:

Once the file is open, you should see several different icons in the top left bar below the file name tab:

Click on the play icon to run the program.

Note: For large point cloud files, the run time can take several hours. The output of this script will generate a .csv file, which is used for generating the PDF report in Python.


## Processing CSV Generated from R
Locate the CSV file: Ensure that you have the CSV file generated from the R script, named "crown_metrics.csv".
Place the CSV file: Move the "crown_metrics.csv" file to the folder containing the Python script "csv_processing.py".


Enable the Run button: To display the Run button on the top-right corner of the Visual Studio Code window, follow these steps:
Click on the gear icon in the lower left corner.
In the sub-menu, click on the Settings.
In the search bar, type "Show Run Icon in Editor Title Menu" to filter the settings.
Locate the "Code-runner: Show Run Icon in Editor Title Menu" option and check the box to enable it.


Run the Python script: Open the "csv_processing.py" file in Visual Studio Code and click the Run button located in the top-right corner of the window.

Check the output: After the script finishes executing, you will find two new files created in the same folder:
output.csv: A cleaned-up version of the original "crown_metrics.csv" file.
tree_analysis_report.pdf: The main report containing detailed information and visualizations about the forest area.
