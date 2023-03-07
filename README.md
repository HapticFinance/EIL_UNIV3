# IL research

This code implements concepts introduced in recent research papers on Uniswap V3 such as : 

  - Analytical computation of expected impermanent loss (EIL) [1]
  - Analytical computation of unit impermanent loss per liquidity (UIL) and static replication with options [2]

This work is part of the research conducted while designing the Haptic protocol and is made available for educational purposes.

## Requirements

- R

## Installation

- Install R from [here](https://cran.r-project.org/bin/linux/ubuntu/) (Ubuntu)
- Clone this repository

## Usage

- Open a terminal and navigate to the repository
- Check that gen_pdf has executable permissions and grant them with `chmod +x gen_report.r`
- Run `./gen_report.r`

The necessary dependencies will be installed and a pdf file will be generated in the `pdf/` folder. The simulation parameters can be found in the `config.r` file.

## References

- [1] Concentrated Liquidity Analysis in Uniswap V3 from DeFi'22: Proceedings of the 2022 ACM CCS Workshop on Decentralized Finance and Security November 2022, pages 63–70, available at https://doi.org/10.1145/3560832.3563438
- [2] Static Replication of Impermanent Loss for Concentrated Liquidity Provision in Decentralised Markets, available at https://arxiv.org/abs/2109.07054