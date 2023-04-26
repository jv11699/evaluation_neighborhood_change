# Introduction

This is the public code repository for the following article (temporarily an arxiv link, pending submission):
```
@article{vergara2021promises,
  title={Promises and Pitfalls of a New Early Warning System for Gentrification in Buffalo, NY},
  author={Vergara, Jan Voltaire and Rodriguez, Maria Y and Dohler, Ehren and Phillips, Jonathan and Villodas, Melissa and Wilson, Amy Blank and Joseph, Kenneth},
  journal={arXiv preprint arXiv:2111.14915},
  year={2021}
}
```

The repository contains cod necessary to reproduce results from the paper - if you use this code, please cite the article! Note that while we provide all materials needed to replicate our work, we do not provide data. The data we use can be collected using the code provided here, but **we do not include raw data files here**. This is because we subscribe to the belief that while the data we study are public, there is no reason to make personal information more accessible than it already is. If you are a researcher and are interested in replication with the original data files, we will handle those requests on a case-by-case basis, please contact the last author if so.

# Code Details

To replicate our case study first run `00_data_collection.ipynb` (which requires `util_datacollection.ipynb`) in order to collect the data used for our case study model. Note that you'll have to set `OLD_PATH` and `DATA_DIR` to valid paths on your machine.

The next step is to run `01_feature_generation.ipynb`, which generates the features described in the paper.

You can then run `02_modeling.ipynb` (which requires `util_modeling.py`) to generate experiment results (which will be placed into the `results` directory.

You can then see and make use of our evaluation framework by running the code in `03_evaluation.R`, which makes use of `util_evaluation.R` for utility code. Please note that you will need a key for the U.S. census API to make use of this code, open `util_evaluation.R` and replace `"PUT_YOUR_KEY_HERE"` with your key.

Finally, note that the repository also contains an empty `celltransactions` folder used by `01_feature_generation.ipynb`, and a folder with a shapefile used in `02_modeling.ipynb` that we used in previous versions of the model to construct features and in later versions of the model because it has a map projection that proved easiest for other tasks.



