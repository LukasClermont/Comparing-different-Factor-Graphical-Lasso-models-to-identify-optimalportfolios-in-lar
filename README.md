# Masterthesis: Comparing different Factor Graphical Lasso models to identify optimalportfolios in large dimensions
## by Lukas Clermont
### Topic contributer:  Prof.  Dr.  Roman Liesenfeld
---------------------
## Description
The factor graphical approach by Lee and Seregina (2021) is applied, providing a framework for the estimation of high dimensional precision matrices in order to optimize portfolio return. It combines a factor approach with a graphical approach. In the factor estimation step, macroeconomic, fundamental and statistical factors are taken into account. The observable factors are sourced from the Yahoo Finance database and the website https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html. Principal Component Analysis (PCA) is applied as the statistic factor model. The graph structure is estimated by the Meinshausen-Buhlmann estimator or the graphical lasso. This results in the following factor graphical models: 
1. Statistical factor graphical lasso (SFGL),
2. Statistical factor nodewise regression (SFMB),
3. Fundamental factor graphical lasso (FFGL),
4. Fundamental factor nodewise regression (FFMB),
5. Macroeconomic factor graphical lasso (MFGL),
6. Macroeconomic factor nodewise regression (MFMB).
---------------------
## Folder strucutre
This project is structured as follows: 
Relevant R scripts are in the code folder. There are three scripts for loading the data. The first loads the returns from the S&P500, and the second and third create a list of all relevant daily or monthly data. In the second step, graphs for the period from 1.1.2021 to 31.6.2021 are divided into communities using the Walktrapmethod by Pons and Latapy (2005) and plotted graphically. In the third step, a daily and monthly experiment is conducted. In a rolling window, all models are calculated and compared for a Minimum Variance Portfolio. Fourth, all results are formatted in Latex. In addition, a Monte Carlo simulation was applied, which yields equivalent results to Lee and Seregina (2021).
The functions folder contains all relevant functions for the calculation of each model. In addition, functions for the benchmark models of POET and the Leoit and Wolf estimator are also included. The function "getGlasso_Manual" is the graphical Lasso algorithm by Friedman et al. (2008). Due to the faster performance, the Glasso package was used for the calculation of the results (http://www-stat.stanford.edu/~tibs/glasso). The Fama-French factors are saved in the folder data/raw. In addition, some intermediate steps of the script are stored in the folder data/temp. The empirical experiment results can be found in the results folder and all the corresponding plots in the figures folder. 
