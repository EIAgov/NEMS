National Energy Modeling System (NEMS) Public Release, AEO2026

March 2026

# NEMS Setup Release Description

The NEMS Public Release is a snapshot of the NEMS system at the time of its creation. The NEMS Public Release directory structure has the following subdirectories.

## NEMS Public Release key subdirectories

<table>
<colgroup>
<col style="width: 19%" />
<col style="width: 80%" />
</colgroup>
<thead>
<tr>
<th>
<strong>Subdirectory name</strong>
</th>
<th>
<strong>Description</strong>
</th>
</tr>
</thead>
<tbody>
<tr>
<td>
input
</td>
<td>
Input files
</td>
</tr>
<tr>
<td>
docs
</td>
<td>
Sphinx style documentation (partially implemented in AEO2026)
</td>
</tr>
<tr>
<td>
source
</td>
<td>
Fortran source code for NEMS components as well as some
<em>preprocessor</em> programs
</td>
</tr>
<tr>
<td>
includes
</td>
<td>
Fortran <em>include</em> files used mainly for variable declaration
accessed during compilation
</td>
</tr>
<tr>
<td>
scripts
</td>
<td>
Programs used to implement NEMS
</td>
</tr>
<tr>
<td>
scedes
</td>
<td>
Scenario-descriptor (scedes) files
</td>
</tr>
<tr>
<td>
output
</td>
<td>
Restart files for each scenario
</td>
</tr>
<tr>
<td>
models
</td>
<td>
Source, input and output files for Python, GAMS and AIMMS
programs
</td>
</tr>
<tr>
<td>
utils
</td>
<td>
files for deprecated utilities
</td>
</tr>
</tbody>
</table>

The settings for a particular run, or scenario of NEMS, are stored in
a *scenario-descriptor* file (scedes). NEMS scedes files identify all
settings for a NEMS run.  The scedes files shown below are included in the public release and contain the settings used in a run. The scedes files provided are in the directory created
as part of the instructions (for example, file *scedes.cb2026* in the
*/scedes* folder).

## Scedes files

<table>
<thead>
<tr>
<th><strong>AEO2026 cases</strong></th>
<th>
<strong>Scedes files provided by run submitter</strong>
</th>
</tr>
</thead>
<tbody>
<tr>
<td>Counterfactual Baseline</td>
<td>
scedes.cb2026
</td>
</tr>
<tr>
<td>High Economic Growth</td>
<td>
scedes.highmacro
</td>
</tr>
<tr>
<td>Low Economic Growth</td>
<td>
scedes.lowmacro
</td>
</tr>
<tr>
<td>High Oil Price</td>
<td>
scedes.highprice
</td>
</tr>
<tr>
<td>Low Oil Price</td>
<td>
scedes.lowprice
</td>
</tr>
<tr>
<td>High Oil and Gas Supply</td>
<td>
scedes.highogs
</td>
</tr>
<tr>
<td>Low Oil and Gas Supply</td>
<td>
scedes.lowogs
</td>
</tr>
<tr>
<td>High Zero-Carbon Technology Cost</td>
<td>
scedes.highZTC
</td>
</tr>
<tr>
<td>Low Zero-Carbon Technology Cost</td>
<td>
scedes.lowZTC
</td>
</tr>
<tr>
<td>Alternative Electricity</td>
<td>
scedes.altelec
</td>
</tr>
<tr>
<td>Alternative Transportation</td>
<td>
scedes.alttrnp
</td>
</tr>
<tr>
<td>Alternative Electricity Alternative Transportation Combination</td>
<td>
scedes.electrnp
</td>
</tr>
<tr>
<td>High Electricity Demand</td>
<td>
scedes.highheldmd
</td>
</tr>
</tbody>
</table>

In NEMS for AEO2026, we created scenario-specific files generally by
adding suffixes. For example:

- ecpdaty_highztc.xlsx is the HIGHZTC scenario file

- ecpdaty_lowztc.xlsx is the LOWZTC scenario file

- Ecpdaty.xlsx is the reference case file

In the scedes, you would point to the desired file for the scenario
being run. For example:  
ECPDATYN=\$NEMS/input/emm/ecpdaty_highztc.xlsx

## NEMS Installation Instructions

### Clone the repository to your PC’s hard drive.

Choose a drive where you want NEMS files to be installed. Create a
folder on that NEMS drive to designate this new version/vintage of
NEMS. Unzip/extract the contents of the NEMS zip to your new folder on
the NEMS drive. Also, create a folder for the NEMS job log such as
*Y:\RabbitMQ*.

### Install Intel® Fortran Compiler Classic and Intel® Fortran Compiler.

NEMS was compiled using a free download of the 23.2.0 release for
Intel Fortran Compiler Classic and Intel Fortran Compiler for Windows
(2023.2.1). You can find more information on the
[Intel](https://www.intel.com/content/www/us/en/developer/articles/tool/oneapi-standalone-components.html#fortran) website.

### Install the Meson Build System

NEMS Fortran functions were compiled using Meson Build. You can find
more information on the [Meson](https://mesonbuild.com/Quick-guide.html) website.

### Install GAMS

NEMS is programmed to invoke version 47.6.0 of GAMS (64-bit). If you
buy the Xpress solver link and obtain the Xpress solver license
independently from GAMS (as we did), you will need to copy the Xpress
licensing file (*xpauth.xpr*) to the GAMS folder in which *gams.exe*
resides. That folder may have a file called *xpauth.ini*; if so, you
must delete it because it interferes with GAMS finding and using the
*xpauth.xpr* license file. You will have to change a setting line in
the scedes file to identify your specific version of GAMS. The scedes
key to change is GAMSVERS.

### Install AIMMS

The AIMMS software is downloaded and set up without a standard
installation process, which AIMMS refers to as an *installation free
release*. We located our AIMMS software in a folder accessible to all
users called *C:\AIMMS_installation_free_releases* rather than the
default location in a specific user home directory.

C:/AIMMS_Installation_Free_Releases/25.5.7.1-x64-VS2022

The above location is identified in the NEMS run submitter via the
scedes file option AIMMSLOC. For us, the setting for AIMMS we used for
AEO2026 was:

C:/AIMMS_Installation_Free_Releases/25.5.7.1-x64-VS2022

We first saved the download installation free executables in the
folder

*C:\aimms_installation_free_releases*.

The first time a user runs AIMMS from that location, AIMMS unzips a
full installation folder into the user's app folder (for example*,
C:\users\xxx\AppData\Local\AIMMS\IFA\AIMMS\\ 25.5.7.1-x64-VS2022*) and
from then on looks there when you invoke either the original
downloaded executable or invoke from the AIMMS desktop icon. This
setup is not ideal for NEMS. To make the folder available without
multiple copies installed and to be locatable to NEMS via the AIMMS
toolkit, we copied our AIMMS appdata folder to
*C:\aimms_installation_free_releases\25.5.7.1-x64-VS2022*and set up
shortcuts on the desktop to point to the executable *..\bin\aimms.exe*
in that folder.

### Run NEMS

You can set up NEMS runs in several ways. **For the AEO2026 runs, we
used parnems** **to set up the parallel version of NEMS.** Three NEMS
executables and output folders divide NEMS into two simultaneously run
processes to speed up run time. To replicate the AEO2026 cases, use
parnems. If you run a subset of NEMS modules (such as one, on a
standalone basis), you should use jognems instead of parnems.

#### Run with the GUI

You can run NEMS through the GUI which simplifies the NEMS setup
process. Most EIA users run NEMS this way. Launch the GUI by
double-clicking on the RunNEMS.bat file located in the NEMS folder.

You will have the following options to configure your NEMS run:

1.  scenario descriptor (scedes) file: Select the appropriate scedes
    file for the scenario you want to run
1.  run output directory: Select the output directory where you want the
    final NEMS output to be saved
1.  NEMS run mode: Select the NEMS run mode you want to use. For the
    AEO2026 runs, we used parnems to setup the parallel version of NEMS.
1.  Job Type: We have a queue server to distribute the NEMS run load
    across multiple servers. If you do not have a queue server setup,
    use the “local” Job Type option.
1.  Run: Click on this button once you’ve completed the setup and are
    ready to initiate the NEMS run

#### Run with a CMD terminal

**Parnems and jognems:** From a cmd prompt, go to the directory with
the scedes file you will use (via the cd command). Launch the run
using the parnems or jognems command, for example, *parnems
common_scenario user_scenario* where *common_scenario* is a common
scenario descriptor file and *user_scenario* is the suffix name of a
user scedes file such as **cb2026** (for the file ***scedes.cb2026***)
in the current directory. The common scenario descriptor file is
generally a file in */scedes*, such as cb2026.

When you make a NEMS run, the settings for that run are stored in both
the directory from which the run was launched and the output directory
of the run. 

The run output directory, such as
*/output/\[user\]/scenario/dMMDDYYa/* will contain written output from
the run. You can switch to that directory to examine the run. Parallel
runs have subfolders p1, p2, and p3 to hold output from the parallel
components of a run. The file *nohup.out* contains the primary trace
output of the run as well as any error messages. For parallel runs, a

*nohup.out* will be in each of the p1, p2, and p3 folders. Any error
messages from the p1 execution will be in the *p1/nohup.out* and so
on. Numerous other files are written during the course of a NEMS runs
holding debug and trace information.

Many of these files are useful only to NEMS developers of a particular
module, and many are larger than a gigabyte. Files in the output
directory with the extension *.gz* have been compressed. The command
uncompress *\*.gz* restores them to their original state.

## The NEMS report writer (*NEMRWR*)

You can run the NEMS report writer to compare one run with others in a
convenient format using the RW_reporter_main.py python file under the
reporter folder. The NEMS report writer will generate all NEMS tables
into the reporter/output folder labeled *TN nnn.xlsx* where *nnn*
corresponds to the NEMS table number.

Another NEMS report writer output file is the *test.d000000a.RAN*. The
*.RAN* file is used with the software *graf2000* (that is, *grafnem*,
and provided in the scripts folder) to review run results graphically.
