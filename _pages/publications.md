---
layout: archive
title: ""
permalink: /research/
author_profile: true
redirect_from:
  - /publications/
---

## Working Papers

<div class="research-entry">
  <p>Dimitrova, D. S., Kaishev, V. K., and S&aacute;enz Guill&eacute;n, E. L. (2025a). <em>GeDS: An R package for Regression, Generalized Additive Models and Functional Gradient Boosting, based on Geometrically Designed (GeD) Splines</em>. Manuscript submitted for publication. Under review in the <em>Journal of Statistical Software</em>. <a href="https://cran.r-project.org/web/packages/GeDS/vignettes/jss_article.pdf">Pre-print</a>.</p>
  <details class="paper-abstract">
    <summary>Abstract</summary>
    <p>In recent years, geometrically designed variable knot splines, named GeDS, have emerged as a promising technique in the domain of spline regression, with <a href="https://link.springer.com/article/10.1007/s00180-015-0621-7">Kaishev, Dimitrova, Haberman, and Verrall (2016)</a> and <a href="https://doi.org/10.1016/j.amc.2022.127493">Dimitrova, Kaishev, Lattuada, and Verrall (2023)</a> showcasing their potential. In this paper, we introduce the R package GeDS that includes the implementation of two significant enhancements of the original GeDS methodology. The first broadens the applicability of GeDS to encompass generalized additive models (GAM), by implementing the local scoring algorithm using GeD splines as function smoothers. This approach stands as a competitive alternative, complementing existing practices suggested by <a href="https://www.taylorfrancis.com/books/mono/10.1201/9780203753781/generalized-additive-models-hastie">Hastie and Tibshirani (1990)</a> and <a href="https://www.taylorfrancis.com/books/mono/10.1201/9781315370279/generalized-additive-models-simon-wood">Wood (2017)</a>, and implemented in the R packages <a href="https://CRAN.R-project.org/package=gam">gam</a> and <a href="https://CRAN.R-project.org/package=mgcv">mgcv</a>, respectively. Secondly, we incorporate functional gradient boosting (FGB) to estimate the number and location of the spline knots, as well as the associated regression coefficients. This novel approach allows the final boosted fit to be expressed as a single spline model, contrasting with typical gradient boosting models, which generally lack a straightforward, interpretable representation. We demonstrate that this technique yields competitive spline fits comparing favorably in both accuracy and efficiency to the outputs of existing boosting-with-splines procedures proposed by <a href="https://www.tandfonline.com/doi/abs/10.1198/016214503000125">B&uuml;hlmann and Yu (2003)</a> and <a href="https://doi.org/10.1016/j.csda.2008.09.009">Schmid and Hothorn (2008a)</a>, and implemented in the R package <a href="https://CRAN.R-project.org/package=mboost">mboost</a>.</p>
    <p>The above extensions position GeDS as a versatile tool for additive modeling within the exponential family, suitable for both regression and classification tasks. The GeDS methodology, including GAM-GeDS and FGB-GeDS, is implemented in the R package GeDS available from <a href="https://cran.r-project.org/package=GeDS">https://cran.r-project.org/package=GeDS</a>. We illustrate the capabilities of this package foregrounding the competitiveness of GeDS, and its potential for applications in the wider contexts of data science and machine learning.</p>
  </details>
</div>

<div class="research-entry">
  <p>Dimitrova, D. S., Kaishev, V. K., and S&aacute;enz Guill&eacute;n, E. L. (2026a). <em>Density and distribution function estimation using variable-knot splines</em>. R package: <a href="https://github.com/emilioluissaenzguillen/ddfs"><strong>DDFS</strong></a>.</p>
  <details class="paper-abstract">
    <summary>Abstract</summary>
    <p>We propose a novel nonparametric framework for density estimation based on B-splines with data-driven knot selection. The method, termed DDFS (Density and Distribution Function Splines), simultaneously estimates the probability density function and the cumulative distribution function using a common spline representation, ensuring internal consistency between the two.</p>
    <p>The approach combines constrained maximum likelihood estimation with a sequential, bias-driven knot insertion procedure inspired by Geometrically Designed Splines (GeDS) introduced by <a href="https://link.springer.com/article/10.1007/s00180-015-0621-7">Kaishev et al. (2016)</a> and extended by <a href="https://doi.org/10.1016/j.amc.2022.127493">Dimitrova et al. (2023)</a>. This yields an adaptive, non-uniform knot sequence with data-driven refinement, where a small number of tuning parameters admit robust default choices but can be adjusted when modelling complex density features.</p>
    <p>We develop a comprehensive asymptotic theory for the proposed estimator in both conditional and unconditional settings. In particular, we show that the data-driven knot sequence satisfies suitable growth and quasi-uniformity properties with high probability, enabling a rigorous sieve maximum likelihood analysis. Under standard smoothness assumptions, we establish uniform (sup-norm) convergence rates for the spline coefficients, density, distribution, and quantile estimators. These rates achieve the classical minimax optimal order (up to logarithmic factors) over H&ouml;lder classes. Moreover, the estimator is shown to attain these rates adaptively, without prior knowledge of the underlying smoothness.</p>
    <p>The spline representation further allows for closed-form expressions of key risk measures, including Value-at-Risk and Tail Value-at-Risk. The corresponding plug-in estimators inherit the optimal convergence rates, supporting accurate and theoretically grounded risk assessment.</p>
    <p>Numerical experiments demonstrate the effectiveness of the proposed method across a range of benchmark densities, highlighting its flexibility and strong finite-sample performance.</p>
  </details>
</div>

## Work in Progress

*Bariatric Data Analytics Based on the UK National Bariatric Surgery Registry (NBSR)*, joint project with Prof. Vladimir Kaishev, Dr. Dimitrina Dimitrova, and external collaborators, Miss Emma Rose McGlone from Imperial College London and Mr Omar Khan from the British Obesity & Metabolic Specialist Society (BOMSS).

## Acknowledged Collaborations

Montes-Rojas and Elosegui (2020). &lsquo;Network ANOVA random effects models for node attributes&rsquo;. In: <em>Journal of Dynamics and Games</em> 7.3, pp. 239&ndash;252. issn: 2164-6066. doi: <a href="https://doi.org/10.3934/jdg.2020017">10.3934/jdg.2020017</a>

## Software

Dimitrova, D. S., Kaishev, V. K., Lattuada, A., S&aacute;enz Guill&eacute;n, E. L., & Verrall, R. J. (2025). *GeDS: Geometrically designed spline regression* [R package version 0.3.4]. [https://CRAN.R-project.org/package=GeDS](https://CRAN.R-project.org/package=GeDS)

Dimitrova, D. S., Kaishev, V. K., & S&aacute;enz Guill&eacute;n, E. L. (2025b). *DDFS: Density & distribution function estimation using variable-knot splines* [R package version 0.1.0]. [https://github.com/emilioluissaenzguillen/ddfs](https://github.com/emilioluissaenzguillen/ddfs)

## Master's Thesis

S&aacute;enz Guill&eacute;n, E. L. (2021). *The Chilean Pension System: Actuarial Analysis of a Paradigmatic Social Security Program*. [https://hdl.handle.net/10171/124147](https://hdl.handle.net/10171/124147)
