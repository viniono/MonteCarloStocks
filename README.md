# Stock Simulator App

This is a README file for the Stock Simulator R Shiny app. The app allows users to explore the stock market and simulate the potential paths of a given stock using Monte Carlo simulations.

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

Certainly! Here's an updated version of the README file with a single heading named "Simulation (Monte Carlo)":

# Simulation (Monte Carlo)

This repository contains a Monte Carlo simulation implemented in R. The simulation aims to forecast potential stock price movements based on historical data using a Monte Carlo method. By generating multiple random scenarios, it provides a range of possible outcomes for future stock prices, allowing users to assess risk and make informed investment decisions.

## Prerequisites

Before running the simulation, make sure you have the following:

- R installed on your machine ([Download R](https://www.r-project.org/))
- Required R packages:
  - `quantmod` for retrieving stock data
  - `tidyquant` for calculating daily returns and performing financial analysis

## Usage

To run the Monte Carlo simulation, follow these steps:

1. Set the desired ticker symbol for the stock of interest in the R script.
2. Specify the time period for historical data by adjusting the 'from' and 'to' dates.
3. Set the number of simulation days for forecasting future stock prices.
4. Run the R script to execute the simulation.

The simulation will generate multiple simulated scenarios of stock price paths and calculate the corresponding total percentage returns for each scenario.

## Results

The simulation provides the following outputs:

- `total_returns`: A list of total percentage returns for each simulated scenario.
- `prices_list`: A matrix containing the simulated stock prices for each scenario, including the initial stock price and prices for each simulation day.

These outputs allow users to analyze the range of potential returns and visualize the simulated stock price paths.


## Acknowledgements

- This simulation was inspired by the concept of Monte Carlo simulations in finance.
- The `quantmod` and `tidyquant` packages were used to retrieve stock data and perform financial analysis.

## Acknowledgements

This app extensively uses the tools provided by the libraries `Quandl` and `Tidyquant`. The stock data is obtained from NASDAQ and Yahoo Finance.

## License

This app is released under the [MIT License](LICENSE).

## Contact

For any questions or issues, please contact [your email address].
