HEAT
====================


[![pipeline status](https://gitlab.inria.fr/sed-bso/heat/badges/master/pipeline.svg)](https://gitlab.inria.fr/sed-bso/heat/commits/master)
[![coverage report](https://gitlab.inria.fr/sed-bso/heat/badges/master/coverage.svg)](https://gitlab.inria.fr/sed-bso/heat/commits/master)
[![Quality Gate](https://sonarqube.bordeaux.inria.fr/sonarqube/api/badges/gate?key=sedbso%3Aheat%3Adev%3Amaster&blinking=true)](https://sonarqube.bordeaux.inria.fr/sonarqube/dashboard?id=sedbso%3Aheat%3Adev%3Amaster)

[![Lines of code](https://sonarqube.bordeaux.inria.fr/sonarqube/api/badges/measure?key=sedbso%3Aheat%3Adev%3Amaster&metric=ncloc)](https://sonarqube.bordeaux.inria.fr/sonarqube/component_measures?id=sedbso%3Aheat%3Adev%3Amaster&metric=ncloc)
[![Comment line density](https://sonarqube.bordeaux.inria.fr/sonarqube/api/badges/measure?key=sedbso%3Aheat%3Adev%3Amaster&metric=comment_lines_density)](https://sonarqube.bordeaux.inria.fr/sonarqube/component_measures?id=sedbso%3Aheat%3Adev%3Amaster&metric=comment_lines_density)
[![Coverage](https://sonarqube.bordeaux.inria.fr/sonarqube/api/badges/measure?key=sedbso%3Aheat%3Adev%3Amaster&metric=coverage)](https://sonarqube.bordeaux.inria.fr/sonarqube/component_measures?id=sedbso%3Aheat%3Adev%3Amaster&metric=coverage)

[![Bugs](https://sonarqube.bordeaux.inria.fr/sonarqube/api/badges/measure?key=sedbso%3Aheat%3Adev%3Amaster&metric=bugs)](https://sonarqube.bordeaux.inria.fr/sonarqube/component_measures?id=sedbso%3Aheat%3Adev%3Amaster&metric=bugs)
[![Vulnerabilities](https://sonarqube.bordeaux.inria.fr/sonarqube/api/badges/measure?key=sedbso%3Aheat%3Adev%3Amaster&metric=vulnerabilities)](https://sonarqube.bordeaux.inria.fr/sonarqube/component_measures?id=sedbso%3Aheat%3Adev%3Amaster&metric=vulnerabilities)
[![Code Smells](https://sonarqube.bordeaux.inria.fr/sonarqube/api/badges/measure?key=sedbso%3Aheat%3Adev%3Amaster&metric=code_smells)](https://sonarqube.bordeaux.inria.fr/sonarqube/component_measures?id=sedbso%3Aheat%3Adev%3Amaster&metric=code_smells)

[![New Bugs](https://sonarqube.bordeaux.inria.fr/sonarqube/api/badges/measure?key=sedbso%3Aheat%3Adev%3Amaster&metric=new_bugs)](https://sonarqube.bordeaux.inria.fr/sonarqube/component_measures?id=sedbso%3Aheat%3Adev%3Amaster&metric=new_bugs)
[![New Vulnerabilities](https://sonarqube.bordeaux.inria.fr/sonarqube/api/badges/measure?key=sedbso%3Aheat%3Adev%3Amaster&metric=new_vulnerabilities)](https://sonarqube.bordeaux.inria.fr/sonarqube/component_measures?id=sedbso%3Aheat%3Adev%3Amaster&metric=new_vulnerabilities)
[![New Code Smells](https://sonarqube.bordeaux.inria.fr/sonarqube/api/badges/measure?key=sedbso%3Aheat%3Adev%3Amaster&metric=new_code_smells)](https://sonarqube.bordeaux.inria.fr/sonarqube/component_measures?id=sedbso%3Aheat%3Adev%3Amaster&metric=new_code_smells)

Mathematical problem
---------------------

This C program aims at solving the following **heat propagation** equation

```math
\frac{\partial u(x,t) }{\partial t} - \Delta u(x,t) = 0 \qquad \forall  t \in [0,T] \, , \forall x \in [0,1]^2
```
```math
u(x,t) = 1 \, \qquad \forall  t \in [0,T] \, , \forall x \in \partial [0,1]^2.
```

Project
---------------------

This program serves as a toy code.
Several software engineering techniques are used:

* CMake build system with CTest
* Doxygen documentation
* A pipeline to test the code, either gitlab-ci (.gitlab-ci.yml) or Jenkins (Jenkinsfile) can be used
* Org-mode script for the code analysis and the integration into a SonarQube instance

Formation
---------------------

* [Gitlab-CI](https://sed-bso.gitlabpages.inria.fr/heat/gitlab-ci.html)
* [SonarQube](https://sed-bso.gitlabpages.inria.fr/heat/sonarqube.html)