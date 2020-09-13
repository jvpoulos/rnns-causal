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

* Download the [MNIST in CSV train set](https://pjreddie.com/media/files/mnist_train.csv) into data folder if running placebo tests on sequential MNIST data

* Extract sales prices data: 
```
$ tar xf data/sales_train_validation.tar.xz -C data/
```
Placebo test experiments
------

Make each file below executable, then execute in shell:

* `sine-placebo.sh`: generated sine waves data
* `rbf-placebo.sh`: generated RBF data
* `mnist-placebo.sh`: sequential MNIST data
* `educ-placebo.sh`: U.S. state government education spending
* `basque-placebo.sh`: Basque Country study
* `california-placebo.sh`: California smoking study 
* `germany-placebo.sh`: W. Germany reunification study
* `stock-placebo.sh`: U.S. stock prices
* `sales-placebo.sh`: Sale prices data
* `covid-placebo.sh`: U.S. county covid cases

To extract RMSEs for table, run in **R** `placebo-results-table.R`


Application: counterfactual predictions
------

First, prepare public education spending data by running in **R** `prepare-funds.R`

Second, run in shell with command line arguments `<GPU_ID> <hidden_activation>  <n_hidden> <patience> <dropout rate> <penalty> <learning_rate> <epochs> <batches> <data_name> <window_size> <T> <imputation_method>`; e.g., 
```
python3 code/train_encoder_decoder.py 3 'relu' 128 100 0.7 2.0 0.001 500 32 'educ' 22 156 'locf'
python3 code/train_lstm.py 3 'relu' 128 100 0.7 2.0 0.001 500 32 'educ' 22 156 'locf'
```

To plot the training and validation error, run `code/plot_history.py <file location of training log> <title>`; e.g., 
```
python code/plot_history.py './results/encoder-decoder/educ/training_log_educ_locf_relu_128_100_0.7_2.0_32.csv' 'Encoder-decoder'
python code/plot_history.py './results/lstm/educ/training_log_educ_locf_relu_128_100_0.7_2.0_32.csv' 'LSTM'
```
To estimate confidence intervals and plot causal estimates, execute in shell `educ-plot.sh` 

To compare estimates with different RNNs configurations, execute in shell `educ-rnns-compare.sh` 

To compare estimates with alternative estimators, execute in shell `educ-benchmark-compare.sh` # TODO: bootstrap CIs