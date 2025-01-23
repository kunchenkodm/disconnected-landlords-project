# Server Access Notes

The University cluster allows to store data and run scripts through a queueing system
----

## 1. Applying for an SCRTP Account
- Apply to a SCRTP account: [Getting Started Guide](https://warwick.ac.uk/research/rtp/sc/desktop/gettingstarted/)
- After receiving a confirmation email, register with Bugzilla, following the instructions in the same link. This will allow to report bugs and make requests (e.g., creating a group or shared folder).

---

## 2. Accessing the Cluster
There are several methods to access the cluster: [Remote Access Guide] (https://docs.scrtp.warwick.ac.uk/linux-pages/remote.html#desktop-remote).

### For Windows Users:
- **Using PuTTY**:
  1. Download and install PuTTY: [Putty.org](https://www.putty.org/).
  2. Configure PuTTY:
     - **Host Name**: `godzilla.csc.warwick.ac.uk`
     - **Port**: `22`
     - **Connection Type**: `SSH`
     - **Saved Sessions**: `godzilla` (or any name you prefer).
  3. Click **Save**, then **Open** to connect.

### For Mac Users:
- PuTTY is **not required**. Use the built-in terminal:
  1. Connect to the server by entering the following command:
     ```bash
     ssh username@godzilla.csc.warwick.ac.uk
     ```
---

Once connected, we can either submit a job script, which the server will run once resources are available (usually right away), or use an interactive job.
- [Wariwck Documentation](https://docs.scrtp.warwick.ac.uk/taskfarm.html)

## 3. Running Jobs on the Cluster

### Submitting a Job Script
You need two files:
- Code script (e.g. R/Python/Stata script).
- A Slurm file.

---

### Example: Running a Python Job
1. **Set the working directory**:
   ```bash
   cd /storage/economics/username/

2. **Upload files to the server**:
   - To upload a file to the server, open a new terminal and run the following locally:
     ```bash
     scp "local/path/to/file" username@godzilla.csc.warwick.ac.uk:/storage/economics/username/
     ```
   - To confirm the file uploaded correctly, run on the server:
     ```bash
     ls /storage/economics/username/
     ```

3. **Write the Slurm job script**:
   - Create the Slurm file:
     ```bash
     nano python_job.slurm
     ```

   - **Example Slurm File for Python**:
     ```bash
     #!/bin/bash
     #SBATCH --job-name=meaningful_entry    # Give a meaningful job name
     #SBATCH -n 1                         # Number of tasks
     #SBATCH -c 2                         # Number of CPU cores
     #SBATCH --time=02:00:00              # Maximum runtime
     #SBATCH --mem-per-cpu=2000           # Memory per CPU in MB
     #SBATCH --output=company_search_output.log  # Name the output log file

     # Load the required modules
     module load GCC/11.3.0 OpenMPI/4.1.4 Python/3.9.6

     # Navigate to your working directory
     cd /storage/economics/username/

     # Run the Python script
     python3 script_name.py
     ```

4. **Save and exit**:
   - MacBook shortcuts:
     - Press `Ctrl + O` to save.
     - Press `Enter` to confirm the file name.
     - Press `Ctrl + X` to exit.

5. **Submit the job**:
   ```bash
   sbatch python_job.slurm

6. **Monitor or cancel the job**:
   - To monitor the job:
     ```bash
     squeue -u username
     ```
     - **ST Values**:
       - `R`: Running
       - `PD`: Pending

   - To cancel the job:
     ```bash
     scancel <job_id>
     ```

7. **Download files from the server**:
   - To download somethin from the server to your local machine, run the following locally:
     ```bash
     scp username@godzilla.csc.warwick.ac.uk:/storage/economics/username/file_name "/local/path/to/file"
     ```

8. **Monitor a specific file**:
   - To check the status of a file:
     ```bash
     stat /path/to/file
     ```

9. **View output logs**:
   - Output from the script can be found in the `.out` files generated in the working directory.
   - Example:
     ```bash
     cat job_output_file.out
     ```
---

## Supplementary Notes: Running R and Stata Jobs, and Windows Tools

### Example Slurm File for R
Use the following Slurm file to run an R script. Modify the script name in the last line as needed. Regarding the R scripts, no need to install packages, just load them from library(). Follow the same steps as listed above for the Python job otherwise.

```bash
#!/bin/bash
#SBATCH --job-name=R_ports_data        # Job name
#SBATCH -n 1                          # Number of tasks
#SBATCH -c 2                          # Number of CPU cores
#SBATCH --part=compute                # Partition
#SBATCH --time=01:00:00               # Maximum runtime
#SBATCH --mem-per-cpu=1000            # Memory per CPU in MB

# GLOBAL PREPARATION
cd /storage/economics/username/Ports 

# PREPARATION
module purge
module load GCC/11.3.0 OpenMPI/4.1.4
module load RStudio-Server/2022.07.2+576-Java-11.0.2-R-4.2.1

# RUN
Rscript download_SO2_data_earthdata.R
```


  
### Stata jobs
After setting wd, run a stata do file: 

```
"srun stata -b do filename.do"
```

---

### Using Interactive Jobs
Interactive jobs allow you to run processes directly on the server.

#### Steps:
1. **Set the working directory**:
   ```bash
   cd /storage/economics/username/Ports
   ```
   
2. **Request an interactive session**:
   ```bash
   ssh -CX username@godzilla.csc.warwick.ac.uk  # For first-time use only
   salloc --nodes=1 --ntasks=1 --mem-per-cpu=3988 --partition=interactive --time=01:00:00 --x11
	```

3. **Request an interactive session**:

	For RStudio:
	```bash
	module purge
	module load GCC/11.3.0 OpenMPI/4.1.4 RStudio/2022.07.2+576-Java-11.0.2-R-4.2.1
	```
			
	For Stata:
	```bash
	module load GCC/13.2.0 libpng/1.6.40 Stata/18-SE
	```

4. **Open the application**:
	
 To open RStudio:
```bash
rstudio --no-sandbox  
```
	
To open Stata:
```bash
xstata 
```
	
Additional resources:
- https://docs.scrtp.warwick.ac.uk/taskfarm-pages/interactive.html
- https://docs.scrtp.warwick.ac.uk/taskfarm-pages/tf-slurm-pages/tf-R.html
- https://docs.scrtp.warwick.ac.uk/taskfarm-pages/applications-pages/rstudio.html

### Windows-Specific Tools

1. **Xming**  
   - May need to download and run Xming to enable graphical applications like RStudio or Stata on Windows.  

2. **WinSCP**  
   - WinSCP can be used to view folders and transfer files between the cluster and your laptop.  
   - For more details, refer to the [File Transfer Guide](https://docs.scrtp.warwick.ac.uk/linux-pages/remote.html).






	
