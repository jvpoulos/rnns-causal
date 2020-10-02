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
  * keras-self-attention (tested on 0.47.0)
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
* Open `package-list.R` in a script editor
  * Verify that all required packages in `package-list.R` are installed in your **R** library

Placebo test experiments
------

Make each file below executable, then execute in shell:

* `sine-placebo.sh`: generated sine waves data
* `rbf-placebo.sh`: generated RBF data
* `educ-placebo.sh`: U.S. state government education spending
* `stock-placebo.sh`: U.S. stock prices

`educ-placebo-plot.R`  and `rbf-placebo-plot.R` creates the plots to reproduce Figure 2 in the paper. 

`stock-placebo-plot.R` creates the plot to reproduce Figure 3 in the paper. 

Application: counterfactual predictions
------

First, prepare public education spending data by running in **R** `prepare-funds.R`

Second, run in shell with command line arguments `<GPU_ID> <hidden_activation>  <n_hidden> <patience> <dropout rate> <penalty> <learning_rate> <epochs> <batches> <data_name> <window_size> <T> <imputation_method>`; e.g., 
```
python3 code/train_encoder_decoder.py 3 'tanh' 128 25 0.2 0.001 0.001 500 32 'educ' 20 159 'locf'
python3 code/train_lstm.py 3 'tanh' 128 25 0.2 0.001 0.001 500 32 'educ' 20 159 'locf'
```

To plot the training and validation error, run `code/plot_history.py <file location of training log> <title>`; e.g., 
```
python code/plot_history.py './results/encoder-decoder/educ/training_log_educ_locf_tanh_128_25_0.2_0.001_32.csv' 'Encoder-decoder'
python code/plot_history.py './results/lstm/educ/training_log_educ_locf_tanh_128_25_0.2_0.001_32.csv' 'LSTM'
```
To estimate randomization confidence intervals and plot causal estimates, execute in shell `educ-plot.sh` (Figure 4)

To compare estimates with alternative estimators, execute in shell `educ-benchmark-compare.sh` (Tables 2 and SM-2)

To compare estimates with different RNNs configurations, execute in shell `educ-rnns-compare.sh` (Table SM-1)

For RNNs placebo treatement effects estimates on pre-treatment data, execute in shell `educ-placebo-pretreatment.sh` 