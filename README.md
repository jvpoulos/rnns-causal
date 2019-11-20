# rnns-causal

This repository provides data and code for reproducing ["RNN-based counterfactual prediction"](https://arxiv.org/abs/1712.03553).

Please cite the paper if you use this code for academic research:

```
@ARTICLE{2017arXiv171203553P,
       author = {{Poulos}, Jason},
        title = "{RNN-based counterfactual prediction}",
      journal = {arXiv e-prints},
     keywords = {Statistics - Machine Learning, Economics - Econometrics, Statistics - Applications},
         year = "2017",
        month = "Dec",
          eid = {arXiv:1712.03553},
        pages = {arXiv:1712.03553},
archivePrefix = {arXiv},
       eprint = {1712.03553},
 primaryClass = {stat.ML},
       adsurl = {https://ui.adsabs.harvard.edu/abs/2017arXiv171203553P},
      adsnote = {Provided by the SAO/NASA Astrophysics Data System}
}
```

Prerequsites
------

* Python 2 (tested on Python 2.7.16)

* Tensorflow 1.2

* **R** version 3.5.2 (2018-12-20). 

Set up
------
* Clone a copy of the repository to your working directory with the command
```
$ git clone https://github.com/jvpoulos/rnns-causal
```
* In **R**, install MCPanel from the forked repo:
```
install.packages("devtools")
library(devtools) 
install_github("jvpoulos/MCPanel")
```
* Open `package-list.R` in a script editor
  * Verify that all required packages in `package-list.R` are installed in your **R** library

* Make shell file `main.sh` executable from the Linux/Unix command line:
```
$ chmod +x main.sh
```
* Execute the file:
```
$ ./main.sh > main.txt
```

Experiments
------

To run placebo experiments, for each DATANAME={basque,california,germany,stock,stock_fixed,educ.pc,votediff} and each MODEL={encoder-decoder,lstm,rvae}:

```
$ mkdir data/$$DATANAME$$
$ mkdir results/$$MODEL$$/$$DATANAME$$
```

and then execute

* `synth-placebo.sh`: synthetic control datasets
* `stock-placebo.sh`: stock market prices with increasing dimensions
* `stock-placebo-fixed.sh`: stock market prices with fixed dimensions
* `educ-placebo.sh`: state government education spending

Counterfactual predictions
------

To get encoder-decoder estimates run `train_encoder_decoder.py <GPU_ID> <epochs> <batches> <data name> <t_0> <T> <imputation_method>`; e.g., 
```
python train_encoder_decoder.py 0 1000 8 'educ' 87 156 'locf'
```

Similarly, for RVAE estimates run `train_rvae.py <GPU ID> <epochs> <batches> <data name> <t_0> <T> <imputation_method>`; e.g., 
```
python train_rvae.py 0 1000 8 'educ' 87 156 'locf'
```

To plot the training and validation error, run `plot_history.py <file location of training log>`; e.g., 
```
python plot_history.py '../results/rvae/educ/training_log_educ.csv'
```