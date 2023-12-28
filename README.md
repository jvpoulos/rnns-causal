# rnns-causal

This repository provides data and code for reproducing ["RNN-based counterfactual prediction, with an application to homestead policy and public schooling"](https://academic.oup.com/jrsssc/article/70/4/1124/7034010).

Please cite the paper if you use this code for academic research:

```
@article{poulos2021rnn,
  title={RNN-based counterfactual prediction, with an application to homestead policy and public schooling},
  author={Poulos, Jason and Zeng, Shuxi},
  journal={Journal of the Royal Statistical Society Series C: Applied Statistics},
  volume={70},
  number={4},
  pages={1124--1139},
  year={2021},
  publisher={Oxford University Press}
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

The results reproduce Table 1 (staggered treatment) and Table SM-1 (simultaneous treatment). 

`code/educ-placebo-plot.R`  and `code/sine-placebo-plot.R` creates the plots to reproduce Figure 2.

`code/stock-placebo-plot.R` creates the plot to reproduce Figure 3. 

Application: counterfactual predictions
------

1. First, prepare public education spending data by running in **R** `code/prepare-funds.R`

2. Second, run in shell with command line arguments `<GPU_ID> <hidden_activation>  <n_hidden> <patience> <dropout rate> <penalty> <learning_rate> <epochs> <batches> <data_name> <window_size> <T> <imputation_method>`; e.g., 
```
python3 code/train_encoder_decoder.py 3 'tanh' 128 25 0.5 0.01 0.001 500 32 'educ' 10 203 'locf'
python3 code/train_lstm.py 3 'tanh' 128 25 0.5 0.01 0.001 500 32 'educ' 10 203 'locf'
```

The script `code/educ-plot-prep.sh` trains RNNs on differently imputed datasets and different RNNs configurations. 

  * To plot the training and validation error, run `code/plot_history.py <file location of training log> <title>`; e.g., 
  ```
  python3 code/plot_history.py './results/encoder-decoder/educ/training_log_educ_locf_tanh_128_25_0.2_0.01_32.csv' 'Encoder-decoder loss'
  python3 code/plot_history.py './results/lstm/educ/training_log_educ_locf_tanh_128_25_0.2_0.01_32.csv' 'LSTM loss'
  ```
  * To estimate and plot causal estimates and randomization confidence intervals for RNNs trained on differently imputed datasets and different configurations, execute in shell `code/educ-plot.sh` (Figure 4, first column of Table 2, Table SM-2, and Table SM-3)

3. To compare RNNs estimates with alternative estimators and imputation methods, execute in shell `code/educ-benchmark-compare.sh` (Table 2 and Table SM-3)

4. For RNNs placebo treatement effects estimates on pre-treatment data, execute in shell `code/educ-placebo-pretreatment.sh`, which produces results for second column of Table 2. 

5. To plot autocorrelation function for the placebo test datasets (Figure 1), run `code/autocorrelation-plot.R`

6. To plot the extent of non-response in education spending data (Figure SM-1) , run `code/non-response-plot.R` 