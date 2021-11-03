# Masterthesis: Comparing different Factor Graphical Lasso models to identify optimalportfolios in large dimensions
## by Lukas Clermont
### Topic contributer:  Prof.  Dr.  Roman Liesenfeld

#### CONTENTS OF THIS FILE
---------------------

##Description
The factor graph lasso approach by Lee and Seregina (2021) is applied, providing a framework for estimation of high dimensional precision matrices in order to optimize portfolio return. It combines a factor approach with a graphical approach. In the factor estimation step, macroeconomic, fundamental and statistical factors are taken into account. The observable factors are sourced from the Yahoo Finanze database and the website https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html. Principal Component Analysis (PCA) is applied as the stastic factor model. For the estimation of the graph for the error precision matrix, the graph structure is estimated by the Meinshausen-Buhlmann graph estimation or the graphical lasso. This results in the following factor graphical models: 
1. statistical factor graphical lasso (SFGL),
2. statistical factor nodewise regression (SFMB),
3. fundamental factor graphical lasso (FFGL),
4. fundamental factor nodewise regression (FFMN),
5. macroeconomic factor graphical lasso (MFGL),
6. macroeconomic factor nodewise regression (MFNR).

## Folder strucutre
This project is structured as follows: 
Relevant R scripts are in the code folder. There are three scripts for loading the data. The first loads the returns from the S&P500 and the second and third create a list of all relevant daily or monthly data. In the second step, graphs for the period from 1.1.2021 to 31.6.2021 are divided into communities using the Walktrapmethod by Pons and Latapy (2005) and plotted graphically. In the third step, a daily and monthly experiment is conducted. In a rolling window, all models are calculated and compared for a Minimum Variance Portfolio. Fourth, all results are formatted in Latex. In addition, a Monte Carlo simulation was applied, which yields equivalent results to Lee and Seregina (2021).  