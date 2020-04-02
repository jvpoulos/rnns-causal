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
  * keras-self-attention, scikit-learn, numpy, pandas
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

* Make shell file `main.sh` executable from the Linux/Unix command line:
```
$ chmod +x main.sh
```
* Execute the file:
```
$ ./main.sh > main.txt
```

Placebo test experiments
------

Uncompress sales data:

```
$ tar xf data/sales_train_validation.tar.xz --directory data/
```

Make each file below executable, then execute

* `basque-placebo.sh`: Basque Country dataset
* `germany-placebo.sh`: W. German reunification dataset
* `california-placebo.sh`: California smoking dataset
* `educ-placebo.sh`: U.S. state government education spending
* `elections-placebo.sh`: U.S. mayoral elections vote margin
* `covid-placebo.sh`: U.S. counties COVID-19 cases
* `stock-placebo.sh`: U.S. stock prices
* `sales-placebo.sh`: U.S. product sales prices

Application: counterfactual predictions
------

To get encoder-decoder estimates run `train_encoder_decoder.py <GPU_ID> <patience> <epochs> <batches> <data name> <t_0> <T> <imputation_method>`; e.g., 
```
python train_encoder_decoder.py 0 250 000 8 'educ' 87 156 'locf'
```

To plot the training and validation error, run `plot_history.py <file location of training log>`; e.g., 
```
python plot_history.py '../results/encoder-decoder/educ/training_log_educ.csv'
```