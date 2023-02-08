# EIL research

This is an implementation of the whitepaper "Concentrated Liquidity Analysis in Uniswap V3" from DeFi'22: Proceedings of the 2022 ACM CCS Workshop on Decentralized Finance and Security November 2022, pages 63–70, available at https://doi.org/10.1145/3560832.3563438 (paywalled). The code implements the analytical computation of expected impermanent loss for Uniswap V3 and other ideas presented in the paper. It will produce a pdf with the results of the simulation.

## Requirements

- R

## Installation

- Install R from [here](https://cran.r-project.org/bin/linux/ubuntu/) (Ubuntu)
- Clone this repository

## Usage

- Open a terminal and navigate to the repository
- Check that gen_pdf has executable permissions or run `chmod +x gen_eil.r`
- Run `./gen_eil.r`

Pdf will be generated in the pdf folder.