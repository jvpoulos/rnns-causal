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

* Python 2.7
  * keras-self-attention, scikit-learn, numpy, pandas,h5py
* Tensorflow 2.1.0
* Keras 2.2.5
* **R** 3.6.1

Set up
------
* Clone a copy of the repository to your working directory with the command
```
$ git clone https://github.com/jvpoulos/rnns-causal
```
* Open `package-list.R` in a script editor
  * Verify that all required packages in `package-list.R` are installed in your **R** library

Placebo test experiments
------

Make each file below executable, execute in shell:

* `sine-placebo.sh`: generated sine waves data
* `rbf-placebo.sh`: generated RBF data
* `educ-placebo.sh`: U.S. state government education spending
* `stock-placebo.sh`: U.S. stock prices

To extract RMSEs for table, run in **R** `placebo-results-table.R`


Application: counterfactual predictions
------

First, prepare public education spending data by running in **R** `prepare-funds.R`

Second, run in shell with command line arguments `<GPU_ID> <patience> <dropout rate> <penalty> <learning_rate> <epochs> <batches> <data_name> <t_0> <T> <imputation_method>`; e.g., 
```
python code/train_encoder_decoder.py 3 100 0.5 0.7 0.001 5000 16 'educ' 87 156 'none'
python code/train_lstm.py 3 100 0.5 0.7 0.001 5000 16 'educ' 87 156 'none'
```

To plot the training and validation error, run `code/plot_history.py <file location of training log> <title>`; e.g., 
```
python code/plot_history.py './results/encoder-decoder/educ/training_log_educ_locf.csv' 'Training vs. validation loss'
```
To plot causal estimates: `educ-plot.R`

To compare estimates with alternative estimators, execute in shell `educ-comparison.R` # TODO: bootstrap CIs