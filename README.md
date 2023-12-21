# Moult phenology
Code for simulation studies and analyses for the manuscript "Extended molt phenology models improve inferences about molt duration and timing" by Philipp H. Boersch-Supan, Hugh J. Hanmer & Robert A. Robinson.

The subdirectories correspond to analyses in the following manuscript sections

- `AMRE_example`: Worked example using real-world American Redstart data
- `misclassified_ON`: Misclassification of non-moulting individuals
- `sampling_bias_mk2`: Constant moult-dependent sampling bias simulation study
- `sigma_tau_tradeoffs`: Assessment of parameter uncertainty in the Type 3 model
- `siskin_sampling_mk2`: Non-constant moult dependent sampling bias
- `t1r`: Type 1 recapture model for high-frequency recapture data
- `t12_data_integration`: Mixed record types for active moult 

## Software installation
The analyses depend on the `moultmcmc` package. The easiest and quickest way of installing `moultmcmc` is to install the package from R-universe using the following code:
```r
install.packages("moultmcmc", repos = "https://pboesu.r-universe.dev")
```
On MacOS and Windows systems this will make use of pre-compiled binaries, which means the models can be installed without a C++ compiler toolchain. On Linux this will install the package from a source tarball. Because of the way the Stan models are currently structured, compilation from source can be a lengthy process (15-45 minutes), depending on system setup and compiler toolchain (where possible it is strongly recommended to compile using multiple threads).

To install `moultmcmc` directly from the github source use the following code. This requires a working C++ compiler and a working installation of `rstan`:
```r
#not generally recommended for Windows or MacOS users
install.packages("remotes")
remotes::install_github("pboesu/moultmcmc")
```
