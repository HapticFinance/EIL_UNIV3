---
title: "Haptic"
date: \today
abstract: ""
output: 
  pdf_document:
      toc: true
      number_sections: true
      toc_depth: 2
      keep_md: true
      keep_tex: true
      template: NULL
      #pandoc_args: [
      #  --template=extras/haptic_template.tex
      #]
html_document: default
tables: yes
geometry: margin=35mm
latex_engine: pdflatex
header-includes:
    - \usepackage{float}
    - \usepackage{pdfpages}
    - \usepackage{tabu}
    - \usepackage{lipsum}
    - \usepackage{booktabs}
    - \usepackage[justification=raggedright,labelfont=bf,singlelinecheck=false]{caption}
    - \usepackage{array}
    - \usepackage{xcolor} 
    - \usepackage{color, colortbl}
    - \usepackage{amsmath}
    - \usepackage{mathtools,mathptmx}
    - \usepackage{tabularx}
    - \usepackage{background}
    - \usepackage[english]{babel}
    - \usepackage{csquotes}                
    - \usepackage[style=alphabetic, backend=bibtex]{biblatex}
    - \bibliography{bibliography/haptic.bib}
    - \usepackage{tikz}
    - \usepackage[font=large,labelfont=bf]{caption}
    - \usetikzlibrary{shapes,positioning}
    - \usepackage{wrapfig}
    - \usepackage{eso-pic,graphicx,transparent}
    - \DeclareUnicodeCharacter{2212}{-}
    - \backgroundsetup{pages={some},contents={}, opacity={0.3}, color={gray}}
    - \usepackage{multirow}
    - \usepackage{caption}
    - \newcommand{\Tau}{\textstyle{\mathcal{T}}}
knit: (function(inputFile, encoding) {
  rmarkdown::render(inputFile, encoding = encoding, output_dir = "pdf") })
---
\captionsetup{font=small}
\captionsetup[table]{labelformat=empty}
\captionsetup[table]{labelfont=bf}
mu is 0.000075 and sigma is 0.001 and t is 168
mu is 0.000075 and sigma is 0.001 and t is 168
mu is 0.000075 and sigma is 0.001 and t is 168
mu is 0.000075 and sigma is 0.001 and t is 168
mu is 0.000075 and sigma is 0.001 and t is 168
mu is 0.000075 and sigma is 0.001 and t is 168
mu is 0.000075 and sigma is 0.002 and t is 168
mu is 0.000075 and sigma is 0.002 and t is 168
mu is 0.000075 and sigma is 0.002 and t is 168
mu is 0.000075 and sigma is 0.002 and t is 168
mu is 0.000075 and sigma is 0.002 and t is 168
mu is 0.000075 and sigma is 0.002 and t is 168
mu is 0.000075 and sigma is 0.001 and t is 168
mu is 0.000075 and sigma is 0.001 and t is 168
mu is 0.000075 and sigma is 0.001 and t is 168
mu is 0.000075 and sigma is 0.001 and t is 168
mu is 0.000075 and sigma is 0.001 and t is 168
mu is 0.000075 and sigma is 0.001 and t is 168
mu is 0.000075 and sigma is 0.002 and t is 168
mu is 0.000075 and sigma is 0.002 and t is 168
mu is 0.000075 and sigma is 0.002 and t is 168
mu is 0.000075 and sigma is 0.002 and t is 168
mu is 0.000075 and sigma is 0.002 and t is 168
mu is 0.000075 and sigma is 0.002 and t is 168
mu is 0.000075 and sigma is 0.001 and t is 720
mu is 0.000075 and sigma is 0.001 and t is 720
mu is 0.000075 and sigma is 0.001 and t is 720
mu is 0.000075 and sigma is 0.001 and t is 720
mu is 0.000075 and sigma is 0.001 and t is 720
mu is 0.000075 and sigma is 0.001 and t is 720
mu is 0.000075 and sigma is 0.002 and t is 720
mu is 0.000075 and sigma is 0.002 and t is 720
mu is 0.000075 and sigma is 0.002 and t is 720
mu is 0.000075 and sigma is 0.002 and t is 720
mu is 0.000075 and sigma is 0.002 and t is 720
mu is 0.000075 and sigma is 0.002 and t is 720

\newpage

\subsection{Simulation results} 
\label{simres}

\begingroup\fontsize{8}{10}\selectfont
\begingroup\fontsize{8}{10}\selectfont

\begin{longtable}[t]{>{}cccccccccc}
\caption{\label{tab:unnamed-chunk-2}Table 1: Results for setting $\mu$ = 7.5e−5, 1.5e−4 and $\sigma$ = 0.001 and $\sigma$ = 0.002 for a week (T = 168 hours) of providing liquidity with initial price \$1000.}\\
\toprule
\multicolumn{1}{c}{ } & \multicolumn{5}{c}{$\sigma = $ 0.001} & \multicolumn{4}{c}{$\sigma = $ 0.002} \\
\cmidrule(l{3pt}r{3pt}){2-6} \cmidrule(l{3pt}r{3pt}){7-10}
\multicolumn{1}{c}{ } & \multicolumn{1}{c}{ } & \multicolumn{4}{c}{Formula} & \multicolumn{4}{c}{Formula} \\
\cmidrule(l{3pt}r{3pt}){3-6} \cmidrule(l{3pt}r{3pt}){7-10}
$\mu$ & r & $\Tau$ & EIL & EFX & EFY & $\Tau$ & EIL & EFX & EFY\\
\midrule
\endfirsthead
\caption[]{Table 1: Results for setting $\mu$ = 7.5e−5, 1.5e−4 and $\sigma$ = 0.001 and $\sigma$ = 0.002 for a week (T = 168 hours) of providing liquidity with initial price \$1000. \textit{(continued)}}\\
\toprule
\multicolumn{1}{c}{ } & \multicolumn{5}{c}{$\sigma = $ 0.001} & \multicolumn{4}{c}{$\sigma = $ 0.002} \\
\cmidrule(l{3pt}r{3pt}){2-6} \cmidrule(l{3pt}r{3pt}){7-10}
\multicolumn{1}{c}{ } & \multicolumn{1}{c}{ } & \multicolumn{4}{c}{Formula} & \multicolumn{4}{c}{Formula} \\
\cmidrule(l{3pt}r{3pt}){3-6} \cmidrule(l{3pt}r{3pt}){7-10}
$\mu$ & r & $\Tau$ & EIL & EFX & EFY & $\Tau$ & EIL & EFX & EFY\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
 & 1.001 & 10.33 & -0.00493 & 0.0671 & 98.15798 & 5.17 & -0.01010 & 0.0350 & 46.34274\\
\nopagebreak
 & 1.010 & 94.01 & -0.00318 & 0.0534 & 103.5092 & 50.45 & -0.00812 & 0.0325 & 48.34008\\
\nopagebreak
 & 1.100 & 168.00 & -0.00045 & 0.0094 & 20.51256 & 167.96 & -0.00180 & 0.0107 & 18.18977\\
\nopagebreak
 & 1.200 & 168.00 & -0.00024 & 0.0049 & 10.74468 & 168.00 & -0.00096 & 0.0056 & 9.530385\\
\nopagebreak
 & 2.000 & 168.00 & -7.17e-05 & 0.0013 & 2.930366 & 168.00 & -0.00029 & 0.0015 & 2.599196\\
\nopagebreak
\multirow{-6}{*}{\centering\arraybackslash 7.50e-05} & 5.000 & 168.00 & -3.80e-05 & 0.0009 & 1.953578 & 168.00 & -0.00015 & 0.0010 & 1.732797\\
\cmidrule{1-10}\pagebreak[0]
 &  &  &  &  &  &  &  &  \vphantom{5} & \\
\nopagebreak
 &  &  &  &  &  &  &  &  \vphantom{4} & \\
\nopagebreak
 &  &  &  &  &  &  &  &  \vphantom{3} & \\
\nopagebreak
 &  &  &  &  &  &  &  &  \vphantom{2} & \\
\nopagebreak
 &  &  &  &  &  &  &  &  \vphantom{1} & \\
\nopagebreak
\multirow{-6}{*}{\centering\arraybackslash } &  &  &  &  &  &  &  &  & \\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{8}{10}\selectfont
\begingroup\fontsize{8}{10}\selectfont

\begin{longtable}[t]{>{}cccccccccc}
\caption{\label{tab:unnamed-chunk-2}Table 2: Results for setting $\mu$ = 7.5e−5, 1.5e−4 and $\sigma$ = 0.001 and $\sigma$ = 0.002 for 30 days (T = 720 hours) of providing liquidity with initial price \$1000.}\\
\toprule
\multicolumn{1}{c}{ } & \multicolumn{5}{c}{$\sigma = $ 0.001} & \multicolumn{4}{c}{$\sigma = $ 0.002} \\
\cmidrule(l{3pt}r{3pt}){2-6} \cmidrule(l{3pt}r{3pt}){7-10}
\multicolumn{1}{c}{ } & \multicolumn{1}{c}{ } & \multicolumn{4}{c}{Formula} & \multicolumn{4}{c}{Formula} \\
\cmidrule(l{3pt}r{3pt}){3-6} \cmidrule(l{3pt}r{3pt}){7-10}
$\mu$ & r & $\Tau$ & EIL & EFX & EFY & $\Tau$ & EIL & EFX & EFY\\
\midrule
\endfirsthead
\caption[]{Table 2: Results for setting $\mu$ = 7.5e−5, 1.5e−4 and $\sigma$ = 0.001 and $\sigma$ = 0.002 for 30 days (T = 720 hours) of providing liquidity with initial price \$1000. \textit{(continued)}}\\
\toprule
\multicolumn{1}{c}{ } & \multicolumn{5}{c}{$\sigma = $ 0.001} & \multicolumn{4}{c}{$\sigma = $ 0.002} \\
\cmidrule(l{3pt}r{3pt}){2-6} \cmidrule(l{3pt}r{3pt}){7-10}
\multicolumn{1}{c}{ } & \multicolumn{1}{c}{ } & \multicolumn{4}{c}{Formula} & \multicolumn{4}{c}{Formula} \\
\cmidrule(l{3pt}r{3pt}){3-6} \cmidrule(l{3pt}r{3pt}){7-10}
$\mu$ & r & $\Tau$ & EIL & EFX & EFY & $\Tau$ & EIL & EFX & EFY\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
 & 1.001 & 21.40 & -0.01046 & 0.0348 & 44.24397 & 10.70 & -0.02116 & 0.0185 & 20.41071\\
\nopagebreak
 & 1.010 & 209.24 & -0.00847 & 0.0281 & 52.12734 & 106.40 & -0.01908 & 0.0171 & 22.29173\\
\nopagebreak
 & 1.100 & 719.73 & -0.00193 & 0.0089 & 20.7859 & 665.52 & -0.00756 & 0.0095 & 17.24904\\
\nopagebreak
 & 1.200 & 720.00 & -0.00103 & 0.0046 & 10.89223 & 719.51 & -0.00413 & 0.0053 & 9.854368\\
\nopagebreak
 & 2.000 & 720.00 & -0.00031 & 0.0013 & 2.970609 & 720.00 & -0.00123 & 0.0014 & 2.6896\\
\nopagebreak
\multirow{-6}{*}{\centering\arraybackslash 7.50e-05} & 5.000 & 720.00 & -0.00016 & 0.0008 & 1.980406 & 720.00 & -0.00065 & 0.0010 & 1.793067\\
\cmidrule{1-10}\pagebreak[0]
 &  &  &  &  &  &  &  &  \vphantom{5} & \\
\nopagebreak
 &  &  &  &  &  &  &  &  \vphantom{4} & \\
\nopagebreak
 &  &  &  &  &  &  &  &  \vphantom{3} & \\
\nopagebreak
 &  &  &  &  &  &  &  &  \vphantom{2} & \\
\nopagebreak
 &  &  &  &  &  &  &  &  \vphantom{1} & \\
\nopagebreak
\multirow{-6}{*}{\centering\arraybackslash } &  &  &  &  &  &  &  &  & \\*
\end{longtable}
\endgroup{}
\endgroup{}

\section{Plots} 
\label{plots}

\subsection{Stock price and impermanent loss}

NULL

\includegraphics[height=0.5\textheight]{pdf/eil_files/figure-latex/unnamed-chunk-3-1} 
\includegraphics[height=0.5\textheight]{pdf/eil_files/figure-latex/unnamed-chunk-3-2} 
