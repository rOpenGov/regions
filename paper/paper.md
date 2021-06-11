---
  title: 'Gala: A Python package for galactic dynamics'
tags:
  - Python
- astronomy
- dynamics
- galactic dynamics
- milky way
authors:
  - name: Adrian M. Price-Whelan^[co-first author] # note this makes a footnote saying 'co-first author'
orcid: 0000-0003-0872-7098
affiliation: "1, 2" # (Multiple affiliations must be quoted)
- name: Author Without ORCID^[co-first author] # note this makes a footnote saying 'co-first author'
affiliation: 2
- name: Author with no affiliation^[corresponding author]
affiliation: 3
affiliations:
  - name: Lyman Spitzer, Jr. Fellow, Princeton University
index: 1
- name: Institution Name
index: 2
- name: Independent Researcher
index: 3
date: 13 August 2017
bibliography: paper.bib

# Optional fields if submitting to a AAS journal too, see this blog post:
# https://blog.joss.theoj.org/2018/12/a-new-collaboration-with-aas-publishing
aas-doi: 10.3847/xxxxx <- update this with the DOI from AAS once you know it.
aas-journal: Astrophysical Journal <- The name of the AAS journal.
---
  
# Summary
  
We often take the availability of a cross-section of national statistical indicators for granted.  Eurostat, the European statistical body, usually publishes data on 25-35 European countries, and global bodies often on more. For statistical comparisons, these are not large cross-sections, even if they are organized in longitudinal panels. There is a growing need to provide statistical indicators on a sub-national level, for example, for instead of treated the United States or Germany as an aggregation unit, using xxxx or xxxxx.

This task is far more difficult than it appears, because while international borders are relatively stable, sub-national boundaries of provinces, states, regions, metropolitan areas change very rapidly – within the European Union there were many thousand changes since 1999. This poses three important problems: the aggregation is not made on the same territory (or its human, enterprise population); the regional units get new names and new geographical ID codes. Because of the very frequent changes, if you try to join data from two independent regional statistics sources, it is almost certain that many geographical codes will not match, or, even if they match, they will not refer to the same boundary. Our package aims to help with these programs in the countries that apply Europe’s NUTS territorial divisions, which includes the European Union, the European Economic Area, and many candidate countries. We also aim to provide some level of global comparability.

This package is an offspring of the Eurostat package, which provides programmatic, reproducible access to the Eurostat data warehouse. We soon realized that the problem of sub-national geographical coding, naming, correct imputation, is a very large task and we created a new package. The resulting clean crossesctional data and longitudional panels already have made some scientific achievements in non-statistical domains


# Statement of need

The NUTS classification (*Nomenclature of territorial units for statistics*) is a hierarchical system for dividing up the economic territory of the EU, the United Kingdom, and some other European countries for the purpose of collection, development and harmonisation of European regional statistics.

It has three hierarchical levels with one connecting top-level territory, i.e. the country itself, and the LAU unit for cities, villages, settlements and their surroundings.

* NUTS 0: the country itself, not formally part of NUTS, but used for practical aggregation tasks in our package. 
* NUTS 1: major socio-economic regions, which are often historically solid sub-divisions, such as Germany's member states;
* NUTS 2: basic regions for the application of regional policies; which are often also historically stable units, such as the provinces of the Netherlands;
* NUTS 3: small regions for specific diagnoses, which is a very fast changing territorial aggregation unit.

The NUTS system used to have smaller NUTS 4 and NUTS 5 definitions, but they were so rapidly changing that it became impractical to use them for statistical aggregation.  Eventually they were replaced with the LAU classification, which is updated annually, and which provides a hierarchical connection to settlements, municipalities.

The currently valid *NUTS 2021 classification* lists 104 regions at NUTS 1, 283 regions at NUTS 2 and 1345 regions at NUTS 3 level. The currently most used NUTS 2016 classification that was valid in the period 2018-2020 listed 104 regions at NUTS 1, 281 regions at NUTS 2 and 1348 regions at NUTS 3 level.

The NUTS is a European standard, and it is not interoperable with the ISO-3166-2 standard on sub-national subdivisions. The ISO-3166-2 is a global standard, but it was developed mainly for public administration and not statistical purposes. It’s greatest shortcoming for statistical application is that it is not hierarchical, i.e. there is no clear additive connection between two metropolitan areas that are located in the same province, for example.  In many cases, European ISO-3166-2 boundary definitions (and naming, coding metadata) coincide with NUTS1, NUTS2 or NUTS3 boundary definitions. But in many cases, they are so differently defined that there is no clear way to match them, even by aggregated or grouping several units. 

# Recoding And Renaming

The simplest changes that occur after the redefinition of the NUTS or ISO-3166-2 boundaries is that the territorial units receive a new code or a new name, even if the boundary is not changed.  This is a good practice:  by assigning a new identification code to the same region in the 2016 definition of NUTS, even if the boundary did not change from the 2013 definitions makes it unambiguous that your are working with data aggregated according to the boundaries defined in 2016 (and in place for the period 2018-2020.) Similarly, sometimes regions, provincies and other territorial units are simply renamed.

In Europe, member states share much statistical information via Eurostat and the ESSnet system, but they are usually not back-casting historically released data. This means that the regional section of the Eurostat data warehouse often contains indicators with mixed coding, and mixed name definitions.  In the same dataset, you find regions with NUTS 2010, NUTS 2013 and NUTS 2016 definitions. This problem is even greater if you start to work with other data sources, which may be less proficient with territorial statistics, and may not be even aware of NUTS changes.

In our understanding, recoding, and renaming only affects the metadata of a territory, and the metadata of any statistical aggregates that were created on the population of that territory.  If the actual territory changes, then aggregates are no longer comparable, and this must be reflected in the metadata by assigning new codes and applying new names. One of the reasons why it is dangerous to use the ISO-3166-2 boundaries for statistical purposes is that boundary changes are often not reflected in the coding. (If a village is administered from a different provincial city, it has no significant importance in public administration; and as the ISO-3166-2 is a public administration standard, the lack of aggregation compatibility is not a criterion of its definition.)

Our package helps with recoding and renaming problems with .....


# Projection and Imputation

It is very rare to find a large cross-sectional or longitudional regional dataset that is complete; and for many analytic purposes, imputation is desirable.  Yet most imputation methods work on the assumption that data is not systematically omitted, which is exactly the case with missing territorially aggregated data.  Europe’s NUTS territories are explicitly defined on a homogeneity criterion. The data shows very strong spatial autocorrelation, but other, more complex relationship with its neighbours, for example, two NUTS 3 level neighbours are divided because on part of the broader territory is more urban, the other is more rural.  Assuming that these two geographically close neighbors are similar for imputation purposes is wrong. It is likely that the typical occupations, education level, age composition is very different – that is why they were defined as separate statistical units.

Our package handles missing values when the hierarchical definition of the territorial boundaries allows an unambiguous filling of missing values. In published datasets, this solves a surprisingly high number of issues.

....


# Applications


## Cross-sectional analysis


## Small area statistics





# Mathematics

Single dollars ($) are required for inline mathematics e.g. $f(x) = e^{\pi/x}$
  
  Double dollars make self-standing equations:
  
  $$\Theta(x) = \left\{\begin{array}{l}
    0\textrm{ if } x < 0\cr
    1\textrm{ else}
    \end{array}\right.$$
    
You can also use plain \LaTeX for equations
    \begin{equation}\label{eq:fourier}
    \hat f(\omega) = \int_{-\infty}^{\infty} f(x) e^{i\omega x} dx
    \end{equation}
    and refer to \autoref{eq:fourier} from text.
    
# Citations
    
Citations to entries in paper.bib should be in [rMarkdown](http://rmarkdown.rstudio.com/authoring_bibliographies_and_citations.html) format.
    
     If you want to cite a software repository URL (e.g. something on GitHub without a preferred
                                                   citation) then you can do it with the example BibTeX entry below for @fidgit.
    
    For a quick reference, the following citation commands can be used:
      - `@author:2001`  ->  "Author et al. (2001)"
    - `[@author:2001]` -> "(Author et al., 2001)"
    - `[@author1:2001; @author2:2001]` -> "(Author1 et al., 2001; Author2 et al., 2002)"
    
# Figures
    
    Figures can be included like this:
      ![Caption for example figure.\label{fig:example}](figure.png)
    and referenced from text using \autoref{fig:example}.
    
    Figure sizes can be customized by adding an optional second parameter:
      ![Caption for example figure.](figure.png){ width=20% }
    
# Acknowledgements
    
    We acknowledge contributions from Brigitta Sipocz, Syrtis Major, and Semyeong
    Oh, and support from Kathryn Johnston during the genesis of this project.
    
# References