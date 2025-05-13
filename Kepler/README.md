# Folder with all the scripts and results using the Exoplanet dataset

## Initial
The scripts for the initial experiments are in this folder. 
- **`experiment.R`**: This script runs the full set of experiments.
- **`experiment_single_mult_cores.R`**: This script runs the single-chain experiments using multiple cores.

The results from each experiment are stored in the **`Results`** folder, and the metrics for the experiments are calculated using **`metrics.R`**.

## Extended
The scripts and result from the extended experiments are in this folder.
- **`experiment_parallel.R`**: This script runs the parallel experiments.
- **`experiment_single_mult_cores.R`**: This script runs the single-chain experiments using multiple cores.

The results from each experiment are stored in the **`Results`** folder, and the metrics for the experiments are calculated using **`metrics_ex.R`**. The data from each individual chain is required to calculate the individual chain metrics (**`individual_chain_metrics.R`**). However, this data is not stored in the repository due to its large size. 



