# nfl-bdb-25

This is the accompanying code for my NFL Big Data Bowl Submission, located here: [insert link]

1.  data_cat.R loads the BDB data and filters and processes it, including categorizing plays as disguised or undisguised.
2.  model_cr.R fits the coverage read model and applies it to the data to feed into model2.
3.  model_ds.R fits the Disguise Score model and contains the post-modeling analysis.

Model architecture taken from:

<https://www.kaggle.com/c/nfl-big-data-bowl-2020/discussion/119400>

via:

Baldwin (2021, June 7). Open Source Football: Computer Vision with NFL Player Tracking Data using torch for R. Retrieved from <https://www.opensourcefootball.com/posts/2021-05-31-computer-vision-in-r-using-torch/>
