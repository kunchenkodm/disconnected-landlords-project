The University cluster allows to store data and run scripts through a queueing system

1. Apply to a SCRTP account following: https://warwick.ac.uk/research/rtp/sc/desktop/gettingstarted/
   After receiving a confirmation email, register with Bugzilla, following the instructions in the same link. This will allow to report bugs and making requests, like creating a group and shared folder

2. There are several methods to access the cluster: https://docs.scrtp.warwick.ac.uk/linux-pages/remote.html#desktop-remote
   I have been using a remote terminal: PuTTY (needs to be installed: https://www.putty.org/)

	Host name: godzilla.csc.warwick.ac.uk
	Port: 22
	Connection type: SSH
	Saved sessions: godzilla (or any name)

	Click "Save", then "Open"
   
3. Once in the terminal, we can either submit a job script, which the server will run once resources are available (usually right away), or use an interactive job
   Documentation: https://docs.scrtp.warwick.ac.uk/taskfarm.html

4. Submitting a job script:
   Need two files: the R script and a slurm file. There are several slurm files in the folder. Example:
	
	"
	#!/bin/bash
	#SBATCH --job-name=R_ports_data
	#SBATCH -n 1
	#SBATCH -c 2
	#SBATCH --part=compute
	#SBATCH --time=01:00:00
	#SBATCH --mem-per-cpu=1000

	#GLOBAL PREPARATION
	cd /storage/economics/username/Ports 

	#PREPARATION
 	module purge
	module load GCC/11.3.0 OpenMPI/4.1.4
	module load RStudio-Server/2022.07.2+576-Java-11.0.2-R-4.2.1

	# RUN

	Rscript download_SO2_data_earthdata.R
	"


  This slurm file can be used to run any R script, just change the name of the script in the last line. Save the file, eg: R.slurm

  To run a stata do file: "srun stata -b do pm25_first_stage_airports.do"
	
	Firstly, change working directory to where the slurm file is: "cd /storage/economics/username/Ports"
	To submit a job script, type: "sbatch R.slurm". A job ID will be assigned to this task.
	To view the progress type: "squeue -u username" (change name of user). Under ST, R means "running", PD means "pending".username
	To view output from R console see the *job ID*.out files that are created in the working directory.
	To cancel a job script type: "scancel *job ID*", replace *job ID* 
	Regarding the R scripts, no need to install packages, just load them from library()

5. Using interactive jobs. 	
	First, set wd to storage: cd /storage/economics/username/Ports

	Request an interactive session:
		 ssh -CX <user>@godzilla.csc.warwick.ac.uk (only first time using)
                 salloc --nodes=1 --ntasks=1 --mem-per-cpu=3988 --partition=interactive --time=01:00:00 --x11
                
	
	Then, load the necessary modules. 
		For RStudio:
			module purge && module load GCC/11.3.0 OpenMPI/4.1.4 RStudio/2022.07.2+576-Java-11.0.2-R-4.2.1
		For Stata:
			module load GCC/13.2.0 libpng/1.6.40 Stata/18-SE

	To open RStudio:
		rstudio --no-sandbox  (may need to open Xming or equivalent)
	
	
	To open Stata:
                xstata  (may need to open Xming or equivalent)
		
	
	See also:
		https://docs.scrtp.warwick.ac.uk/taskfarm-pages/interactive.html
		https://docs.scrtp.warwick.ac.uk/taskfarm-pages/tf-slurm-pages/tf-R.html
                https://docs.scrtp.warwick.ac.uk/taskfarm-pages/applications-pages/rstudio.html

	Note: For Windows laptop, I had to install Xming

6. To easily view the folder, I installed WinSCP (for Windows). I can easilly see all files and transfer files from the cluster to my laptop (see screenshot below). 
	See File Transfer info in https://docs.scrtp.warwick.ac.uk/linux-pages/remote.html 


# ----------------------------------------------------------------------------------

Note: For Mac, installing putty is not required. 

Python file notes:

1. Connect to server by entering in local server: 
	
	ssh username@godzilla.csc.warwick.ac.uk

2. Set wd
	cd /storage/economics/username/

3. If need to upload files to the server, open a new terminal and run the following locally:
	
	scp "local/path/to/file" username@godzilla.csc.warwick.ac.uk:/storage/economics/username/

To confirm file uploaded correctly, run in the server connected terminal:

	ls /storage/economics/username/

4. Once your python script is written/uploaded, write the SLURM job script:
	
	nano python_job.slurm

	Example slurm file:

	"""

	#!/bin/bash
	#SBATCH --job-name=meaninful_entry    # Give a meaningful job name 
	#SBATCH -n 1                         # Number of tasks
	#SBATCH -c 2                         # Number of CPU cores
	#SBATCH --time=02:00:00              # Maximum runtime (48 hr limit on server I believe)
	#SBATCH --mem-per-cpu=2000           # Memory per CPU in MB
	#SBATCH --output=company_search_output.log  # Name the output log file

	# Load the required modules
	module load GCC/11.3.0 OpenMPI/4.1.4 Python/3.9.6

	# Navigate to your working directory
	cd /storage/economics/ecucth/

	# Run the Python script
	python3 script_name.py

	"""

	Macbook shortcuts:
		§ Press Ctrl + O to save.
		§ Press Enter to confirm the file name.
		§ Press Ctrl + X to exit.

5. Submit the job:

	sbatch python_job.slurm

To monitor a job:
		squeue -u username

To cancel a job: 
		scancel <job_id>

To monitor a file: 
		"Stat /path/to/file"

If you wish to download some file from your directory on the server to your local machine, run in the local server:
		scp ecucth@godzilla.csc.warwick.ac.uk:/storage/economics/username/file name  "/local/path/to/file" 




	
