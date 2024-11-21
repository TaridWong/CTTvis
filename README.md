### R/CTTvis: Visualize Item Metrics Of The Classical Test Theory Framework
[Tarid Wongvorachan](https://taridwong.github.io/)

<img src="https://github.com/TaridWong/CTTvis/blob/main/hexlogo.png" alt="A hexagon logo with writing CTTvis" width="160" />

#### Installation
Install R/CTTvis from Github using the devtools package
```r
devtools::install_github("TaridWong/CTTvis")
```

#### Example use
```r
library(CTTvis)
```
To demonstrate the `difficulty_plot` and `point_biserial_plot` functions, we will first load a built-in dataset called `dichotomous_response`.

In some context, item difficulty flag thresholds may change. This can be adjusted using the `easyFlag` and `hardFlag` arguments. 
The following use the easy flag threshold of .8, meaning that items that gets answered correctly 80% of the total test takers or greater are considered easy. 
On the other hand, items that gets answered correctly 60% of the total test takers or less are considered difficult.

```r
data(dichotomous_response)

difficulty_plot(responses = dichotomous_response, 
		title = "Item Difficulty Plot", easyFlag = .80, hardFlag = .60)
```
For the `point_biserial_plot` function, you could adjust your point-biserial correlation (pBis) threshold as well. 
For example, if you want the pBis threshold to be .3, you could configure the `pBis_threshold` as follows:

```r
point_biserial_plot(responses = dichotomous_response, 
		title = "Item Discrimination Plot", pBis_threshold = 0.30)
```
To demonstrate the `coefficient_alpha_plot` function, we need to load another built-in dataset called `reliability_df`. 
This dataset was simulated to test the capability of this function.

The influence of an item when dropped to the overall unidimensional coefficient alpha could vary, hence the option to configure the rounding of overall coefficient alpha. 
For example, if dropping an item increases the overall coefficient alpha by 0.001, then rounding the alpha by three decimal places could allow researchers to see the increase compared to rounding the alpha by two decimal points.

The following demonstration rounds the overall alpha by four decimal points. This argument can be adjusted based on the researchers' needs.

```r
data(reliability_df)

coefficient_alpha_plot(responses = reliability_df, 
		title = "Coefficient Alpha Plot", alpha_round = 4)
```

#### Licenses

The R/CTTvis package as a whole is distributed under
[GPL-3 (GNU General Public License version 3)](https://www.gnu.org/licenses/gpl-3.0.en.html).
