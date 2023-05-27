# Stock Simulator App

This is a README file for the Stock Simulator R Shiny app. The app allows users to explore the stock market and simulate the potential paths of a given stock using Monte Carlo simulations.

<img width="1432" alt="image" src="https://github.com/viniono/MonteCarloStocks/assets/73005714/35af9189-7977-4890-a4bd-906bc7036889">

## Prerequisites

Before running the app, make sure you have the following R packages installed:

- shiny
- shinythemes
- shinyWidgets
- DT
- tidyquant
- ggplot2
- dplyr

You can install these packages by running the following command in R:

```R
install.packages(c("shiny", "shinythemes", "shinyWidgets", "DT", "tidyquant", "ggplot2", "dplyr"))
```

## Getting Started

To run the app, follow these steps:

1. Download or clone the repository to your local machine.
2. Open the R script file (`app.R`) in RStudio or any other R editor.
3. Set the working directory to the directory where the app files are located.
4. Install the required packages if you haven't done so already (see Prerequisites section).
5. Run the app by clicking the "Run App" button in RStudio or by executing the following command in R:

```R
shiny::runApp()
```

6. The app will open in your default web browser.

## App Usage

The app consists of several tabs:

### Home Tab

This tab provides an introduction to the app and explains its purpose and features.

### Design Process Tab

In this tab, you can find a description of the design process followed for the app. It includes an analysis of system interactivity and user-system performance.

### Table Tab

In the Table tab, you can explore stocks listed in NASDAQ, AMEX, and NYSE. Use the sidebar filters to select specific industries and countries, and the table will display the filtered stocks.

### Simulation (Monte Carlo) Tab

The Simulation (Monte Carlo) tab allows you to simulate the potential paths of a given stock using Monte Carlo simulations. Specify the stock ticker symbol, time period for historical data, and the number of simulation days. The simulation will generate multiple scenarios and provide outputs such as total percentage returns and simulated stock prices. These outputs help you assess risk and make informed investment decisions.

## Acknowledgements

- This app extensively uses the tools provided by the libraries `Quandl` and `Tidyquant`. The stock data is obtained from NASDAQ and Yahoo Finance.

## License

This app is released under the [MIT License](LICENSE).

## Contact

For any questions or issues, please contact [your email address].

## App Deployment

The Stock Simulator app is deployed and can be accessed using the following link: [Stock Simulator App](https://viniono.shinyapps.io/Finance_324/).
