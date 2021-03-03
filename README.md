# rnns-causal

This repository provides data and code for reproducing ["RNN-based counterfactual prediction, with an application to homestead policy and public schooling"](https://arxiv.org/abs/1712.03553).

Please cite the paper if you use this code for academic research:

```
@misc{poulos2020rnnbased,
      title={RNN-based counterfactual prediction, with an application to homestead policy and public schooling}, 
      author={Jason Poulos and Shuxi Zeng},
      year={2020},
      eprint={1712.03553},
      archivePrefix={arXiv},
      primaryClass={stat.ML}
}
```

Prerequsites
------

* Python 3 (tested on Python 3.6.8)
  * scikit-learn (tested on 0.23.2)
  * numpy (tested on 1.19.1)
  * pandas (tested on 1.0.3)
  * h5py (tested on 2.10.0)
  * matplotlib (tested on 2.0.2)
* Tensorflow 2.1.0 (CUDA 10.1 and cudDNN 7.6 for Linux GPU)
* Keras (tested on 2.3.1)
* **R** (tested on 3.6.3) 

Set up
------
* Clone a copy of the repository to your working directory with the command
```
$ git clone https://github.com/jvpoulos/rnns-causal
```
* Open `code/package-list.R` in a script editor
  * Verify that all required packages are installed in your **R** library

Placebo test experiments
------

Make each file below executable, then execute in shell (within the home dir.):

* `code/sine-placebo.sh`: sine waves data
* `code/rbf-placebo.sh`: GP data
* `code/educ-placebo.sh`: education spending data
* `code/stock-placebo.sh`: stock market data

The results reproduce Tables 1 (staggered treatment) and SM-1 (simultaneous treatment). 

`code/educ-placebo-plot.R`  and `code/rbf-placebo-plot.R` creates the plots to reproduce Figures 2 (staggered) and SM-1 (simultaneous).

`code/stock-placebo-plot.R` creates the plot to reproduce Figures 3 (staggered) and SM-2 (simultaneous). 

Application: counterfactual predictions
------

1. First, prepare public education spending data by running in **R** `code/prepare-funds.R`

2. Second, run in shell with command line arguments `<GPU_ID> <hidden_activation>  <n_hidden> <patience> <dropout rate> <penalty> <learning_rate> <epochs> <batches> <data_name> <window_size> <T> <imputation_method>`; e.g., 
```
python3 code/train_encoder_decoder.py 3 'tanh' 128 25 0.5 0.01 0.001 500 32 'educ' 10 203 'locf'
python3 code/train_lstm.py 3 'tanh' 128 25 0.5 0.01 0.001 500 32 'educ' 10 203 'locf'
```

The batch script `code/educ-plot-prep.sb` runs the code for differently imputed datasets. 

  * To plot the training and validation error, run `code/plot_history.py <file location of training log> <title>`; e.g., 
  ```
  python3 code/plot_history.py './results/encoder-decoder/educ/training_log_educ_locf_tanh_128_25_0.2_0.01_32.csv' 'Encoder-decoder loss'
  python3 code/plot_history.py './results/lstm/educ/training_log_educ_locf_tanh_128_25_0.2_0.01_32.csv' 'LSTM loss'
  ```
  * To estimate randomization confidence intervals and plot causal estimates, execute in shell `code/educ-plot.sh` (Figure 4)

3. To compare estimates with alternative estimators and imputation methods, execute in shell `code/educ-benchmark-compare.sh` (Tables 2 and SM-3)

4. To compare estimates with different RNNs configurations, execute in shell `code/educ-rnns-compare.sh` (Table SM-2)

5. For RNNs placebo treatement effects estimates on pre-treatment data, execute in shell `code/educ-placebo-pretreatment.sh`, which produces results for second column of Table 2. 

6. To plot autocorrelation function for the placebo test datasets (Figure 1), run `code/autocorrelation-plot.R`

7. For figures summarizing non-response (SM-3) and treatment status (SM-4) in education spending data, run `code/non-response-plot.R` 