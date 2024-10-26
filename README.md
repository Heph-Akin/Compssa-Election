# Compssa-Election

![Repository Owner](https://github.com/Heph-Akin)  
A project to compile and verify election results for the College of Medicine and Pharmaceutical Sciences Students Association (COMPSSA).

## Table of Contents
- [Introduction](#introduction)
- [Project Structure](#project-structure)
- [Files](#files)
- [How to Use](#how-to-use)
- [License](#license)

## Introduction
This repository contains scripts, data, and charts used to compile, verify, and analyze votes for the COMPSSA Executive Committee elections. It includes R scripts for processing data, managing the database of registered voters, and visualizations for analyzing voting demographics.

## Project Structure

The repository includes the following:
- **Data files**: `.xlsx` and `.csv` files containing voter and vote data.
- **R Scripts**: R files for compiling votes and verifying registration.
- **Visualizations**: PNG files representing data insights from the election results.

## Files

### Main Files
- **COMPSSA Student Database without duplicates.xlsx**  
  A cleaned database of registered COMPSSA students.

- **Vote Count.csv**  
  A compilation of votes cast in the election.

- **Voters List.csv**  
  A list of eligible voters for registration verification purposes.

### R Scripts
- **Compssa Votes Compilation.R**  
  Processes and compiles votes data from the `Vote Count.csv`.

- **Registration Verification.R**  
  Verifies voter eligibility based on the `Voters List.csv`.

### Visualizations
- **President Department.png**  
- **President Gender.png**  
- **President Level.png**  
- **President.png**  
  Charts showing demographic insights for the COMPSSA President election.

## How to Use

1. **Data Preparation**  
   Ensure data files (`Vote Count.csv`, `Voters List.csv`) are up-to-date before running the R scripts.

2. **Vote Compilation**  
   Run `Compssa Votes Compilation.R` to process votes and generate results.

3. **Registration Verification**  
   Use `Registration Verification.R` to cross-check voter eligibility from the `Voters List.csv`.

4. **Analysis and Visualization**  
   Analyze the generated data or refer to the provided visualizations for insights into voting patterns.

## License
This project is under the MIT License. See the LICENSE file for more details.
