PredictoR
=========

Introduction
------------

R library to help creating prediction models.

Engineering guidelines
----------------------
 - Code Style: https://google.github.io/styleguide/Rguide.xml
 - Project Organization: http://nicercode.github.io/blog/2013-04-05-projects/
 - Unit Testing: https://cran.r-project.org/web/packages/RUnit/vignettes/RUnit.pdf

Naming&Data guidelines
----------------------
 - Include log on functions beginning.
 - Use data.table instead of data.frame (whenever possible).
 - Do not use "names" to change data.table column names.
 - Use "double quotes" instead of 'single quotes' for strings.
 - Use English (or convert to English as early as possible).
 - Column names in lowercase.
 - Do not consider the order (or number) of columns to be static.
 - First letter of column names must be associated with the class/entity.
    - E.g.: pname (product name), cgroup (client group)
 - All text in lowercase.
